package controllers

import play._
import play.mvc._

object Application extends Controller {
    
  def index = models.AAA.name
    
}
