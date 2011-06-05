(ns ring-on-heroku.core
  (:use ring.util.response
        ring.adapter.jetty))

(defn app [req]
  (response "Hello World"))

(defn -main []
  (let [port (Integer/parseInt (get (System/getenv) "PORT" "8080"))]
    (run-jetty app {:port port})))
