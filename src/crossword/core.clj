(ns crossword.core
  (:require [clojure.java.io :as io]
            [clojure.set :as s]))

(declare init-board random-start filled? find-coord can-place find-space add-word)

(defonce default-options
  {:max-length 7
   :min-length 3
   :exclude-pattern #"\s|\d|\W"})

(defonce ^:dynamic *options* (merge default-options {}))

(defonce empty-char \.)

(defrecord Word [direction coord pattern])

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
          (map #(.toLowerCase %))
          (shuffle)
          (into '() )))))

(defn create-board
  "Creates a crossword board
  args:
  - n, int for the size of the square board"
  ([] (create-board (:max-length *options*)))
  ([n] (atom
        {:words []
         :grid (mapv #(into [] %) (partition n (repeat (* n n) empty-char)))})))

(def ^:private not-nil? (complement nil?))
(defn- add-word [board [dir [x y]] word]
  (when (and (not-nil? dir) (not-nil? x) (not-nil? y))
    (let [coord #(if (= 0 dir) [y (+ x %)] [(+ y %) x])]
      (swap! board (fn [b]
                     {:words (conj (b :words) (Word. dir [x y] word))
                      :grid (reduce #(update-in %1 (coord (first %2)) (constantly (last %2)))
                                    (b :grid)
                                    (map-indexed vector word))})))))

(defn fill-board
  "Fills in given board with the words provided"
  ([board word-seq] (fill-board board word-seq (random-start board)))
  ([board word-seq position]
   (let [sorted-word (sort-by count > word-seq)
         _ (add-word board position (first sorted-word))]
     (doseq [w (drop 1 sorted-word)]
       (when-let [intersections (s/intersection (set w) (set (:pattern (last (:words @board)))))]
         (println "Current word: " w "intersections with " (:pattern (last (:words @board))) "is " intersections)
         (add-word board
                   (first (map  #(-> %
                                     (find-coord (last (:words @board)))
                                     (can-place board w)
                                     (first))
                                intersections))
                   w))))))


(defn- find-coord [c {:keys [direction coord pattern]}]
  (println "--find-coord--" c " in word " pattern)
  (let [idx (.indexOf pattern (str c) 0)
        [x y] coord]
    (if (= 0 direction)
      [1 [(+ x idx) y]]
      [0 [x (+ y idx)]])))

(defn- row [board y]
  (get (@board :grid) y))

(defn- col [board x]
  (map #(get % x) (@board :grid)))

(defn- word-fit-at-intersection-at [dir x y board word]
  (fn [idx]
    (let [seq (if (= dir 0) (row board y) (col board x))
          begin-seq (if (= dir 0) (- x idx) (- y idx))
          end-seq  (- x (- (count word) idx))
          chopped-seq (drop-last end-seq (drop begin-seq seq))
          char-tuple-seq (partition 2 (interleave chopped-seq word))]
      (println "--word-fit--idx: " idx word seq begin-seq end-seq chopped-seq char-tuple-seq "\n")
      (every? #(or (= (first %) (last %))
                   (= (first %) empty-char)) char-tuple-seq))))

(defn- can-place
  "From intersection location find if a word can be place on a board
  returns starting location or nil"
  [[dir [x y]] board word]
  (let [c (get-in (@board :grid) [y x])
        indexes (remove nil? (map-indexed #(when (= %2 c) %1) word))]
    (println "---can-place---" c indexes )
    (map #(identity [dir [(- x %) y]])
     (filter (word-fit-at-intersection-at dir x y board word) indexes))))

(defn- seq-to-re-str
  "Return a regex str from a seq s"
  [s]
  (->> s
       (map #(str (if (= empty-char %) "." %)))
       (partition-by #(re-find (re-pattern (str empty-char)) %))
       (reduce #(str %1 (first %2) "{" (count %2) "}") "")))

(defn- find-space
  "Find a space "
  ([board start-position] (find-space board start-position (:min-length *options*)))
  ([board [dir [x y]] min-length]
   (let [extract (partial take-while #(re-matches (re-pattern (str empty-char "|" \w)) (str %)))]
     (if (= 0 dir)
       {:dir dir :coord [x y] :pattern (seq-to-re-str (extract (drop x (row board y))))}
       {:dir dir :coord [x y] :pattern (seq-to-re-str (extract (drop y (col board x))))}))))

(defn- random-start
  "Returns a tuple that represents:
  - direction, 0 for horizontal and 1 for vertical
  - position, on the board"
  [board]
  (let [direction (rand-int 2)
        n (rand-int (count (@board :grid)))]
    (if (= 0 direction)
      [direction [0 n]]
      [direction [n 0]])))

(defn- filled?
  "Predicate than tells if a board is filled
  TODO: find better way to define a full board"
  [board]
  (>= (count (:words @board)) (:max-length *options*)))


(defn generate [n user-options]
  (binding [*options* (merge default-options user-options)]
    (fill-board (create-board) (take n (words "words.txt")))))
