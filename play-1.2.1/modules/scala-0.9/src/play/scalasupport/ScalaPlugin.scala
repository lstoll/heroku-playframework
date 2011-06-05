package play.scalasupport

import play._
import play.test._
import play.vfs.{VirtualFile => VFile}
import play.exceptions._
import play.classloading.ApplicationClasses.ApplicationClass

import scala.tools.nsc._
import scala.tools.nsc.reporters._
import scala.tools.nsc.util._
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

import scala.tools.nsc.io._

import java.util.{List => JList, Map => JMap}
import java.net.URLClassLoader
import java.io.{PrintStream, ByteArrayOutputStream}

import org.scalatest.{Suite, Assertions}
import org.scalatest.tools.ScalaTestRunner
import javax.print.attribute.standard.Severity

/**
 * The ScalaPlugin in mainly responsible for compiling .scala files
 *
 * As scala compilation is pretty slow, we try to optimize things a lot:
 * The plugin will keep an in memory instance of the scala compiler. After each change to a scala file,
 * it will check for dependent files and recompile only needed sources. 
 *
 * When a compilation error occurs we will try to recompile all sources to avoid problems.
 *
 * We discard the in-memory compiler in some cases:
 * - After a change in the .scala files set (adding or removing a scala file to the application)
 * - When a modification to a scala file destroy some types (however if the type was anonymous we try to keep the compiler anyway)
 */
class ScalaPlugin extends PlayPlugin {
    
    override def onLoad {
        play.templates.CustomGroovy()
        play.data.binding.Binder.register(classOf[play.db.anorm.Pk[_]], new PkBinder())
        play.data.binding.Binder.register(classOf[Option[_]], new OptionBinder())
    }
  
    override def overrideTemplateSource(template: play.templates.BaseTemplate, source: String) = {
      if(template.isInstanceOf[play.templates.GroovyTemplate]) {
          template.source.replace("?.", "?.safeNull()?.")
      } else {
          null
      }
    }
    
    override def addTemplateExtensions(): JList[String] = List("play.templates.TemplateScalaExtensions")
    
    override def willBeValidated(o: Any) = {
        o match {
            case Some(v) => v.asInstanceOf[AnyRef]
            case _ => null
        }
    }

    /**
     * Custom binders for Scala
     * - Option[T]
     */
    override def bind(name:String, clazz:Class[_], t:java.lang.reflect.Type, annotations:Array[java.lang.annotation.Annotation] , params: java.util.Map[String, Array[String]])= {
        clazz match {
            case c if c == classOf[Option[_]] => {
                val parameterClass = t.asInstanceOf[java.lang.reflect.ParameterizedType].getActualTypeArguments()(0)
                val result = play.data.binding.Binder.bind(name, parameterClass.asInstanceOf[Class[_]], parameterClass, annotations, params)
                Option(result)
            }
            case c if c == classOf[play.db.anorm.Pk[_]] => {
                val parameterClass = t.asInstanceOf[java.lang.reflect.ParameterizedType].getActualTypeArguments()(0)
                val result = play.data.binding.Binder.bind(name, parameterClass.asInstanceOf[Class[_]], parameterClass, annotations, params)
                play.db.anorm.Id(result)
            }
            case c if c == classOf[play.db.anorm.Id[_]] => {
                val parameterClass = t.asInstanceOf[java.lang.reflect.ParameterizedType].getActualTypeArguments()(0)
                val result = play.data.binding.Binder.bind(name, parameterClass.asInstanceOf[Class[_]], parameterClass, annotations, params)
                play.db.anorm.Id(result)
            }
            case _ => null
       }
    }
    
    override def unBind(o:Any, name:String) = {
        o match {
            case play.db.anorm.Id(id) => Map(name -> id).asInstanceOf[Map[String,AnyRef]]
            case play.db.anorm.NotAssigned => null
            case Some(v) => Map(name -> v).asInstanceOf[Map[String,AnyRef]]
            case None => null
            case _ => null
        }
    }

    // ---------------- COMPILATION
   
    var lastHash = 0

  /**
   * Scanning both java and scala sources for compilation
   */
  def scanSources = {
    val sources = ListBuffer[VFile]()
    val hash = new StringBuffer
    def scan(path: VFile): Unit = {
      path match {
        case _ if path.isDirectory => path.list foreach scan
        case _ if (path.getName().endsWith(".scala") || path.getName().endsWith(".java")) && !path.getName().startsWith(".") => sources add path; hash.append(path.relativePath)
        case _ =>
      }
    }
    Play.javaPath foreach scan
    (sources, hash.toString.hashCode)
  }

  /**
   * try to detect source changes
   */
  override def detectChange = {
    if (lastHash != scanSources._2) {
      reset()
      throw new PathChangeException
    }
  }

  def reset() {
    lastHash = 0
    compiler = new ScalaCompiler
  }
  
  def eval(code: String): Any = compiler.eval(code)

  /**
   * compile all classes
   * @param classes classes to be compiled
   * @param return return compiled classes
   */
  override def compileAll(classes: JList[ApplicationClass]) = {

    // Precompiled
    if (Play.usePrecompiled) {
      new java.util.ArrayList[ApplicationClass]()
    }

    val (sources, hash) = scanSources
    if (lastHash == hash) {
      classes.addAll(compile(ListBuffer[VFile]()))
    } else {
      if (compiler == null) {
        compiler = new ScalaCompiler
      }
      lastHash = hash
      try {
        classes.addAll(compile(sources))
      } catch {
        case ex: PathChangeException => // Don't bother with path changes here
        case ex: Throwable => lastHash = 0; throw ex
      }

    }
  }

  /**
   * inject ScalaTestRunner into play's test framework
   * @param testClass a class under testing
   */
  override def runTest(testClass: Class[BaseTest]) = {
    testClass match {
      case suite if classOf[Suite] isAssignableFrom testClass => ScalaTestRunner runSuiteClass suite.asInstanceOf[Class[Suite]]
      case junit if classOf[Assertions] isAssignableFrom testClass => ScalaTestRunner runJunitClass junit
      case _ => null
    }
  }

  /**
   * compile a class if a change was made.
   * @param modified classes that were modified
   */
  override def onClassesChange(modified: JList[ApplicationClass]) = {
    val sources = new java.util.ArrayList[VFile]
    modified foreach {
      cl: ApplicationClass =>
        var source = cl.javaFile
        if (!(sources contains source)) {
          sources add source
        }
    }
    compile(sources)
  }
  
  override def onConfigurationRead {
  }

  // Compiler
  private var compiler: ScalaCompiler = _

  class PathChangeException extends Exception

  /**
   * compiles all given source files
   * @param sources files to be compiled
   * @param return List of compiled classes
   */
  def compile(sources: JList[VFile]) = {
    detectChange()
    val classes = compiler.compile(sources.toList)
    detectChange()
    classes
  }

  private class ScalaCompiler {
      
    private def realOne(name: String) = if(name.equals("/eval")) null else realFiles.get(name.stripPrefix(File.separator)).get

    // Errors reporter
    private val reporter = new Reporter() {
      override def info0(position: Position, msg: String, severity: Severity, force: Boolean) = {
        severity match {
          case ERROR if position.isDefined => throw new CompilationException(realOne(position.source.file.name), msg, position.line, position.startOrPoint-1, position.endOrPoint-1)
          case ERROR => throw new CompilationException(msg);
          case WARNING if position.isDefined => Logger.warn(msg + ", at line " + position.line + " of " + position.source)
          case WARNING => Logger.warn(msg)
          case INFO if position.isDefined => Logger.info(msg + ", at line " + position.line + " of " + position.source)
          case INFO => Logger.info(msg)
        }
      }

    }

    // VFS
    private val realFiles = HashMap[String, VFile]()
    private val virtualDirectory = new SDirectory("(out)", None)

    // Compiler
    private val settings = new Settings()
    settings.outputDirs.setSingleOutput(virtualDirectory)
    settings.deprecation.value = true
    settings.classpath.value = System.getProperty("java.class.path")    
    //adding anything in WEB-INF to the classpath.  this is for running in a servlet container
    for(path <- this.getClass().getClassLoader().asInstanceOf[URLClassLoader].getURLs){
        if(path.toString.matches(".*WEB-INF.*")) settings.classpath.value += System.getProperty("path.separator") + path.toString
    }
    settings.debuginfo.value = "vars"
    settings.debug.value = false
    settings.dependenciesFile.value = "none"
    settings.make.value = "transitive" // We set it transitive to have the dependencies generated.
    private val compiler = new Global(settings, reporter)

    // Dependencies
    private val dependencies = new java.util.HashMap[String, java.util.Set[String]]
    private val targets = new java.util.HashMap[String, java.util.Set[String]]
    private val currentClasses = new java.util.HashSet[String]

    // Clean the compiler
    def clean() {
      virtualDirectory.clear()
      dependencies.clear()
      targets.clear()
      currentClasses.clear()
      realFiles.clear()
    }
  
    //eval snippet on the fly
    def eval(code: String): Any = {
        
      // Compile code snippet
      val script = "package interpreted {\n object Script { \n" + code + "\n def execute=None \n}\n  }"
      val file =  new BatchSourceFile("/eval", script)
      val run = new compiler.Run()
      run.compileSources(List(file))
      
      // Retrieve byte code
      val compiledCode = new java.util.HashMap[String, Array[Byte]]
      def scan(path: AbstractFile): Unit = {
        path match {
          case d: VirtualDirectory => path.iterator foreach scan
          case d: SDirectory => path.iterator foreach scan
          case f: VirtualFile =>
            val byteCode = play.libs.IO.readContent(path.input)
            val sourceFile = sourceFileFor(path.toString)
            val className = path.toString.replace("(out)/", "").replace("/", ".").replace(".class", "")
            if(className.startsWith("interpreted.")) {
               compiledCode.put(className, byteCode) 
            }
          case _ => 
        }
      }
      virtualDirectory.iterator foreach scan
      
      // Build a fake classloader
      val fakeLoader = new ClassLoader(Play.classloader) {
         override def loadClass(name: String, resolve: Boolean) = {
             if(name.startsWith("interpreted.")) {
                 val bytecode = compiledCode.get(name)
                 super.defineClass(name, bytecode, 0, bytecode.length)
             }
             super.loadClass(name, resolve)
         }
      }
      
      // Run the script
      val scriptClass = fakeLoader.loadClass("interpreted.Script")
      val result = scriptClass.getDeclaredMethod("execute").invoke(null)
      
      return result
    }

    // Retrieve the source file for a scala compiled class
    def sourceFileFor(clazzFile: String): VFile = {
      for (sf <- targets.keySet) {
        for (cf <- targets.get(sf)) {
          if (cf.equals(clazzFile)) {
            return realFiles.get(sf.stripPrefix(File.separator)).get.asInstanceOf[VFile]
          }
        }
      }
      return null
    }

    // Compile a set of Play source files
    def compile(sources: List[VFile]) = {
      val run = new compiler.Run()

      // Compute the transitive closure of dependent sources
      def transitiveClosure(recompile: JList[VFile], tFile: VFile) {
        if (!recompile.contains(tFile)) {
          recompile.add(tFile)
          val name = tFile.relativePath
          for (sf <- dependencies.keySet) {
            for (df <- dependencies.get(sf)) {
              val dvf = VFile.open(new java.io.File(df))
              if (tFile.equals(dvf)) {
                val trcf = realFiles.get(sf.stripPrefix(File.separator)).get.asInstanceOf[VFile]
                transitiveClosure(recompile, trcf)
              }
            }
          }
        }
      }

      // Adding dependent sources
      val toRecompile = new java.util.ArrayList[VFile]
      sources map {
        vfile =>
          transitiveClosure(toRecompile, vfile)
      }

      // BatchSources
      var sourceFiles = toRecompile.toList map {
        vfile =>
          val name = vfile.relativePath.stripPrefix(File.separator)
          realFiles.put(name, vfile)
          new BatchSourceFile(new SFile(name, vfile.getRealFile), vfile.contentAsString)
      }

      // Clear compilation results
      compiler.dependencyAnalysis.dependencies = compiler.dependencyAnalysis.newDeps
      toRecompile.toList foreach {
        vfile =>
          val name = vfile.relativePath.stripPrefix(File.separator)
          val toDiscard = targets.get(name)
          if (toDiscard != null) {
            for (d <- toDiscard) {
              currentClasses.remove(d)
            }
          }
      }
      
      // Clear out space
      virtualDirectory.clear()

      // Compile
      if (!toRecompile.isEmpty()) {

        play.Logger.trace("Compiling %s", toRecompile)

        // The scala compiler use too much of println!!!
        Console.withOut(new PrintStream(new ByteArrayOutputStream())) {
            run.compileSources(sourceFiles)
        }    

        // Build dependencies
        val deps = compiler.dependencyAnalysis.dependencies

        for ((target, depends) <- deps.targets) {
          var s = new java.util.HashSet[String]
          for (c <- depends) {
            s.add(c.path)
            currentClasses.add(c.path)
          }
          targets.put(target.name, s)
        }

        for ((target, depends) <- deps.dependencies) {
          var s = new java.util.HashSet[String]
          for (c <- depends) {
            s.add(c.path)
          }
          dependencies.put(target.name, s)
        }

      }

      // Retrieve result
      val classes = new java.util.ArrayList[ApplicationClass]()

      def scan(path: AbstractFile): Unit = {
        path match {
          case d: VirtualDirectory => path.iterator foreach scan
          case d: SDirectory => path.iterator foreach scan
          case f: VirtualFile if currentClasses.contains(path.toString) =>
            val byteCode = play.libs.IO.readContent(path.input)
            val sourceFile = sourceFileFor(path.toString)
            val className = path.toString.replace("(out)/", "").replace("/", ".").replace(".class", "")

            var applicationClass = Play.classes.getApplicationClass(className)
            if (applicationClass == null) {
              applicationClass = new ApplicationClass() {
                override def compile() = {
                  javaByteCode
                }

              }
              applicationClass.name = className
              applicationClass.javaFile = sourceFile
              applicationClass.javaSource = applicationClass.javaFile.contentAsString
              play.Play.classes.add(applicationClass)
            }
            applicationClass.compiled(byteCode)
            classes.add(applicationClass)

          case _ => 
        }
      }
      virtualDirectory.iterator foreach scan

      // Remove classes that don't exist anymore
      val toRemove = new java.util.ArrayList[String]
      for (ac <- play.Play.classes.all()) {
        if (ac.javaFile.getName().endsWith(".scala")) {
          var isDiscarded = true
          for (cc <- currentClasses) {
            val className = cc.replace("(out)/", "").replace("/", ".").replace(".class", "")
            if (className.equals(ac.name)) {
              isDiscarded = false
            }
          }
          if (isDiscarded) {
            toRemove.add(ac.name)
          }
        }
      }
      for (tr <- toRemove) {
        play.Play.classes.remove(tr)
        if (!tr.contains("$anonfun$")) reset() // force full reload since we have destroyed some types
      }

      // Computed scala classes
      classes
    }

  }

}

object OnTheFly {
  
  def eval(code: String): Any = {
    play.Play.plugin(classOf[ScalaPlugin]).eval(code)
  }

}

