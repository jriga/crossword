(ns crossword.web
  (:require [compojure.core :refer [defroutes GET PUT POST DELETE ANY]]
            [compojure.handler :refer [site]]
            [compojure.route :as route]
            [clojure.java.io :as io]
            [ring.adapter.jetty :as jetty]
            [environ.core :refer [env]]
            [crossword.core :as cw]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(defn request-words [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [max (Integer/parseInt (get-in req [:params :max] "7"))
                  min (Integer/parseInt (get-in req [:params :min] "3"))
                  limit (Integer/parseInt (get-in req [:params :limit] "10"))]
              (-> (take limit
                        (cw/words "words.txt" 
                                  {:max-length max
                                   :min-length min
                                   :exclude-pattern #"\s|\d|\W"}))
                  (json/write-str)
                  str))})

(defn landing [req]
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body "Words World"})

(defroutes app
  (GET "/" [] (io/resource "public/index.html"))
  (GET "/request" [] request-words)
  (route/resources "/")
  (ANY "*" [] (io/resource "public/index.html")))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty (site #'app) {:port port :join? false})))

;; For interactive development:
;; (.stop server)
;; (def server (-main))

