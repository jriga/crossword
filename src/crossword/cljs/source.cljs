(ns crossword.cljs.source)

(defn words
  "Creates subset of words that match required criterias
  from the word.txt base file
  (words source 7 3 #\".- \")
  args:
  - source, text file with a wourd per line
  - max-length, maximum word length
  - min-length, minimum word length
  - exclude-pattern, regex that exclude certain words"
  [source {:keys [max-length min-length exclude-pattern]}]
  (with-open [rdr (open-file source)]
    (->> (line-seq rdr)
         (remove #(or (re-find exclude-pattern %)
                      (< max-length (count %))
                      (> min-length (count %))))
         (map #(.toLowerCase %))
         (shuffle)
         (into '() ))))
