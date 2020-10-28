(ns crossword.core
  (:require [crossword.generator :as cg]))

(defn- parse-js-options [o]
  (into {} (for [[k v] (js->clj o)]
             [(keyword k) v])))

(defn ^:export create 
  ([words] (create words {}))
  ([words user-options]
   (let [options (merge cg/default-options (parse-js-options user-options))]
     (clj->js (cg/generate words options)))))

