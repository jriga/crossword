(ns crossword.core
  (:require [crossword.generator :as cg]
            [clojure.java.io :as io]))

(defn words
  "Creates subset of words that match required criterias
  from the word.txt base file
  (words source 7 3 #\".- \")
  args:
  - source, text file with a wourd per line
  - max-length, maximum word length
  - min-length, minimum word length
  - exclude-pattern, regex that exclude certain words

  TODO: look into transducers for this function instead of the threading macro"
  [source {:keys [max-length min-length exclude-pattern]}]
  (with-open [rdr (io/reader (io/resource source))]
    (->> (line-seq rdr)
         (remove #(or (re-find exclude-pattern %)
                      (< max-length (count %))
                      (> min-length (count %))))
         (map #(.toLowerCase %))
         (shuffle)
         (into '() ))))

(defn create 
  ([file]       (create file 100 {}))
  ([file limit] (create file limit {}))
  ([file limit user-options]
   (let [options (merge cg/default-options user-options)]
     (cg/generate (take limit (words file options)) options))))
