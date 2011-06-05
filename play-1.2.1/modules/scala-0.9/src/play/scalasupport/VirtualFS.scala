package play.scalasupport

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream, File => JFile }
import scala.tools.nsc.io._
import scala.collection.mutable
import PartialFunction._

/**
 * defines a scala source file
 */
private[scalasupport] class SFile(val name: String, val jfile: JFile) extends AbstractFile {
  
  override def hashCode = name.hashCode
  override def equals(that: Any) = cond(that) { case x: SFile => x.name == name }

  def path = jfile.getAbsolutePath()
  def absolute = this
  final def file: JFile = jfile.getAbsoluteFile() 
  def isDirectory: Boolean = false
  
  // Not implemented  
  override def sizeOption: Option[Int] = throw new UnsupportedOperationException
  def input : InputStream = throw new UnsupportedOperationException
  override def output: OutputStream = throw new UnsupportedOperationException
  def container : AbstractFile = throw new UnsupportedOperationException
  def lastModified: Long = scala.Long.MinValue
  def iterator: Iterator[AbstractFile] = Iterator.empty
  def create { throw new UnsupportedOperationException }
  def delete { throw new UnsupportedOperationException }
  def lookupName(name: String, directory: Boolean): AbstractFile = null 
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = throw new UnsupportedOperationException

}

/**
 * defines a scala source directory
 */
private[scalasupport] class SDirectory(val name: String, maybeContainer: Option[SDirectory])
extends AbstractFile {
  def path: String =
    maybeContainer match {
      case None => name
      case Some(parent) => parent.path+'/'+ name
    }

  def absolute = this

  def container = maybeContainer.get
  def isDirectory = true
  var lastModified: Long = System.currentTimeMillis

  override def file = null
  override def input = error("directories cannot be read")
  override def output = error("directories cannot be written")

  /** Does this abstract file denote an existing file? */
  def create {
    throw new UnsupportedOperationException
  }

  /** Delete the underlying file or directory (recursively). */
  def delete {
    throw new UnsupportedOperationException
  }

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = {
      if(directory) {
          subdirectoryNamed(name)
      } else {
          fileNamed(name)
      }
  }
  
  private val files = mutable.Map.empty[String, AbstractFile]

  // the toList is so that the directory may continue to be
  // modified while its elements are iterated
  def iterator = files.valuesIterator.toList.iterator
  
  override def lookupName(name: String, directory: Boolean): AbstractFile =
    files get name filter (_.isDirectory == directory) orNull
    
  override def fileNamed(name: String): AbstractFile =
    Option(lookupName(name, false)) getOrElse {
      val newFile = new VirtualFile(name, path+'/'+name)
      files(name) = newFile
      newFile
    }
  
  override def subdirectoryNamed(name: String): AbstractFile =
    Option(lookupName(name, true)) getOrElse {
      val dir = new SDirectory(name, Some(this))
      files(name) = dir
      dir
    }

  def clear() {
    files.clear();
  }
}
