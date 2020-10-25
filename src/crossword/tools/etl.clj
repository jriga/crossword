(ns crossword.tools.etl
  (:require [clojure.java.io :as io]))

(declare random-start)

(def default-options
  {:max-length 7
   :min-length 3
   :exclude-pattern #"\s|\d|\W"})

(defn words
  "Creates subset of words that match required criterias
  from the word.txt base file
  (words source 7 3 #\".- \")
  args:
  - source, text file with a wourd per line
  - max-length, maximum word length 
  - min-length, minimum word length
  - exclude-pattern, regex that exclude certain words"
  ([source] (words source 
                   (:max-length default-options)
                   (:min-length default-options)
                   (:exclude-pattern default-options)))
  ([source max-length min-length exclude-pattern]
   (with-open [rdr (io/reader (io/resource source))]
     (->> (line-seq rdr)
          (remove #(or (re-find exclude-pattern %)
                       (< max-length (count %))
                       (> min-length (count %))))
          (shuffle)
          (into '())))))
