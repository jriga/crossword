(ns crossword.core
  (:require [clojure.java.io :as io]))

(declare init-board random-start find-space add-word)

(defonce default-options
  {:max-length 7
   :min-length 3
   :exclude-pattern #"\s|\d|\W"})

(defonce ^:dynamic *options* (merge default-options {}))

(defonce empty-char \.)

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
                   (:max-length *options*)
                   (:min-length *options*)
                   (:exclude-pattern *options*)))
  ([source max-length min-length exclude-pattern]
   (with-open [rdr (io/reader (io/resource source))]
     (->> (line-seq rdr)
          (remove #(or (re-find exclude-pattern %)
                       (< max-length (count %))
                       (> min-length (count %))))
          (shuffle)
          (into '())))))

(defn create-board
  "Creates a crossword board
  args:
  - n, int for the size of the square board"
  ([] (create-board (:max-length *options*)))
  ([n] (atom (mapv #(into [] %) (partition n (repeat (* n n) empty-char))))))

(defn fill-board
  "Fills in given board with the words provided"
  [board word-seq]
  (let [sorted-word (sort-by count > word-seq)
        _ (add-word board (random-start board) (first sorted-word))]
    ))

(defn generate [user-options]
  (binding [*options* (merge default-options user-options)]
    (fill-board (create-board) (words "words.txt"))))

(defn- add-word [board [dir [x y]] word]
  (let [coord #(if (= 0 dir) [y (+ x %)] [(+ y %) x])]
    (doseq [[i c] (map-indexed vector word)]
      (swap! board update-in (coord i) (constantly c)))))

(defn- seq-to-re-str
  "Return a regex str from a seq s"
  [s]
  (->> s
       (map #(str (if (= empty-char %) "." %)))
       (partition-by #(re-find (re-pattern (str empty-char)) %))
       (reduce #(str %1 (first %2) "{" (count %2) "}") "")))

(defn- find-space
  ([board start-position] (find-space board start-position (:min-length *options*)))
  ([board [dir [x y]] min-length]
   (let [extract (partial take-while #(re-matches (re-pattern (str empty-char "|" \w)) (str %)))]
     (if (= 0 dir)
       {:dir dir :coord [x y] :pattern (seq-to-re-str (extract (drop x (get @board y))))}
       {:dir dir :coord [x y] :pattern (seq-to-re-str (extract (drop y (map #(get % x) @board))))}))))

(defn- random-start
  "Returns a tuple that represents:
  - direction, 0 for horizontal and 1 for vertical
  - position, on the board"
  [board]
  (let [direction (rand-int 2)
        n (rand-int (count @board))]
    (if (= 0 direction)
      [direction [0 n]]
      [direction [n 0]])))
