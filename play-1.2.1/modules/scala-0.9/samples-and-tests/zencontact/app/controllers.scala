package controllers

import play._
import play.mvc._
import play.data.validation._

import java.util.Date

import models._

import play.db.anorm._

object Application extends Controller {
    
    def index = {
        Template('now -> new Date)    
    }
    
    def list = {
        Template('contacts -> Contact.find("order by name, firstname ASC").list())
    }
       
    def form(id: Long) = {
        Template('contact -> Contact.find("id={id}").onParams(id).first())
    }
    
    def save(@Valid contact: Contact) = {
        if(Validation.hasErrors()) {
            Template("@form", 'contact -> contact)
        } else {
            Contact.update(contact)        
            Action(list)
        }        
    }
    
    def create(@Valid contact: Contact) = {
        if(Validation.hasErrors()) {
            Template("@form", 'contact -> contact )
        } else {
            Contact.create(contact)
            Action(list)
        }
    }
    
    def delete(id: Long) = {
        Contact.delete("id={id}").onParams(id).executeUpdate()
        Action(list)
    }
    
}

