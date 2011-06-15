# Play framework on Heroku

## Key points

* Create the app on the cedar stack

    heroku create --stack cedar

* Give it a project.clj, to make it think it's a 'support' app configuration

    (defproject ring-on-heroku "1.0.0-SNAPSHOT"
      :description "Example Ring app running on Heroku")

* Create an app

    play-1.2.2RC1/play new app

* Setup procfile to start app from extracted play framework, and set port/JVM memory opts correctly

Procfile:

    web: play-1.2.2RC1/play run scala_app --%prod -Xmx200m --http.port=$PORT

Create a Gemfile and Gemfile.lock to make heroku think it's a supported app:



* Push to heroku, and enjoy!

## TODO

* Make scala work! Java only fine, scala strange memory errors. Check JVM config?
* Parse out DB/memcache env vars in to java -D properties, then pick them up in app config via ${}
