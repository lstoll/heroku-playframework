package play.cache

import play.libs.Time._
import scala.actors.Actor._
import scala.actors._

/**
 * Extends the Cache API with two scala specific methods, this is made public via type alias
 */
private[cache] object ScalaCache extends CacheDelegate {

    case class CacheMessage[A](key: String, expiration: String, window: String, waitForEvaluation: String = "10s", f:()=>A, isDesireable:A => Boolean)
    case class Caching()
    
    private def prefixed(key: String) = "__" + key
    
    private lazy val supervisor = actor {
        self.trapExit = true
        loop {
            react { 
                case Exit(from: Actor, exc: Exception) => {
                    play.Logger.warn("cache actor crashed on: "+exc.toString+" resstarting...");
                    from.restart();
                    play.Logger.warn("restarted cache actor")
                }
            }
        }
    }
  
    private lazy val cacheActor = actor {
        self.link(supervisor)
        loop {
            react {
                
                case CacheMessage(key, expiration, window, waitForEvaluation,f,isDesireable) => {
                    val flagWhileCaching = "___" + key
                    getFromCache1(flagWhileCaching).getOrElse {
                        set(flagWhileCaching, Caching(), waitForEvaluation);
                        get[Any](key,expiration,window)(f())(isDesireable)
                        play.Logger.info("asynchronously recached: "+ key)
                        _impl.delete(flagWhileCaching)
                    }

                }
          
                case _ => None
            }
        }
    }

    private def getFromCache[T](key: String) = Option(_impl.get(key).asInstanceOf[T])

    private def getFromCache1(key: String): Option[_] = Option(_impl.get(key))

    /**
    * Retrieves value from Cache based on the type parameter
    * @param key the name of the cache key
    * @param return either the value or None
    */
    def get[T](key: String)(implicit m: ClassManifest[T]): Option[T] = {
        if (key == null) None
        val v = _impl.get(key).asInstanceOf[T]
        if (v == null) {
            None
        } else if (m.erasure.isAssignableFrom(v.asInstanceOf[AnyRef].getClass)) {
            Some(v)
        } else {
            play.Logger.warn("Found a value in cache for key '%s' of type %s where %s was expected", key, v.asInstanceOf[AnyRef].getClass.getName, m.erasure.getName)
            None
        }
    }


    /**
    * Retrieves value from Cache based on the type parameter
    * @param key the name of the cache key
    * @param return either the value or None
    * @param expiration expiration period
    */
    def get[T](key: String, expiration: String)(getter: => T): T = {

        get(key) match {
            case Some(x) => x
            case None => {
                val r = getter
                set(key, r, expiration)
                r
            }
        }
        
    }
    // Refactor this, you need to have a specific type to avoid implicits conflicts
    object Instances {
        implicit def isDesirable[A](o: Option[A]): Boolean = o.isDefined
        implicit def isDesirableSeq[A, B[X] <: Seq[X]](seq: B[A]): Boolean = seq.nonEmpty
    }

    /**
    * Retrieves a key in async fashion
    * @param key cache key
    * @param expiration experiation period
    * @param window
    * @param waitForEvaluation
    * @return parameterized type
    */
    def getAsync[A](key: String, expiration: String, window: String, waitForEvaluation: String = "10s")(getter: => A)(implicit isDesirable: A => Boolean): A = {

        getFromCache[A](key).getOrElse(
            getFromCache[A](prefixed(key)).map(v => {
                cacheActor ! CacheMessage(key, expiration, window, waitForEvaluation, () => getter,isDesirable); v
            }
        ).getOrElse(get(key,expiration,window)(getter)(isDesirable)))

    }

    /**
    * Retrieves key if it's not in cache it populates cache using the the passed in getter
    * @param key
    * @param experiation
    * @param window
    * @param getter
    * @return parameterized type
    */
    def get[A](key: String, expiration: String, window: String)(getter: => A)(implicit isDesirable: A => Boolean): A = {
        
        val cacheIt = (v: A) => {
            set(prefixed(key), v, parseDuration(expiration) + parseDuration(window) + "s")
            set(key, v, expiration)
            v
        }
        
        get(key).getOrElse({
            val result = getter;
            if (isDesirable(result)) {
                cacheIt(result)
            } else {
                get(prefixed(key)).map({v=> set(key, v, "2min");v}).getOrElse(result)
            }
        })
        
    }
    
}
