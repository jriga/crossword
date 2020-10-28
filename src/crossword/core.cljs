(ns crossword.core
  (:require [crossword.generator :as cg]))

(defn ^:export create 
  ([words] (create words {}))
  ([words user-options]
   (let [options (merge cg/default-options user-options)]
     (clj->js (cg/generate words (js->clj options))))))

