package controllers

import play._
import play.mvc._
import play.libs._
import play.cache._
import play.data.validation.Annotations._

import models._

trait Defaults {
    self:Controller =>
    
    @Before def setDefaults {
        renderArgs += "blogTitle" -> configuration("blog.title")
        renderArgs += "blogBaseline" -> configuration("blog.baseline") 
    }
    
}

object Application extends Controller with Defaults {
    
    def index = {
        val allPosts = Post.allWithAuthorAndComments.map( 
            t => new { val (post,author,comments) = t }
        )
        Template(
            'front -> allPosts.headOption, 
            'older -> allPosts.drop(1)
        )
    }
    
    def show(id: Long) = {
        Post.byIdWithAuthorAndComments(id).map( p =>
            Template(
                'item -> new { val (post,author,comments) = p },
                'pagination -> p._1.prevNext,
                'randomID -> Codec.UUID
            )
        ).getOrElse(NotFound("No such Post"))
    }
    
    def postComment(
        postId:Long, 
        @Required(message="Author is required") author:String, 
        @Required(message="A message is required") content:String,
        @Required(message="Please type the code") code:String,
        randomID:String) = {

        validation.equals(
            code, Cache.get(randomID).orNull
        ).message("Invalid code. Please type it again");

        if(validation.hasErrors) {
            show(postId)
        } else {
            Comment.create(Comment(postId, author, content))
            flash += "success" -> ("Thanks for posting " + author)
            Cache.delete(randomID)
            Action(show(postId))
        }
    }
    
    def captcha(id:String) = {
        val captcha = Images.captcha
        val code = captcha.getText("#E4EAFD")
        Cache.set(id, code, "10mn")
        captcha
    }
    
}
