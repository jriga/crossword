(ns crossword.core
  #?(:clj
     (:require [clojure.set :as s]
               [crossword.clj.source :as source]))
  #?(:cljs
      (:require [clojure.set :as s]
                [crossword.cljs.source :as source])))

(defonce default-options
  {:max-length 7
   :min-length 3
   :exclude-pattern #"\s|\d|\W"})

(defonce empty-char \.)

(defrecord Word [direction coord pattern])

(defn create-board
  "Creates a crossword board
  args:
  - n, int for the size of the square board"
  ([] (create-board (:max-length default-options)))
  ([n]
   {:words []
    :grid (mapv #(into [] %) (partition n (repeat (* n n) empty-char)))}))

(declare fill-board add-noise random-start)
(defn ^:export generate
  ([]  (generate 100))
  ([n] (generate n {}))
  ([n user-options]
   (let [options (merge default-options user-options)
         board (create-board (:max-length options))]
     (-> board
         (fill-board
                 (take n (source/words "words.txt" options))
                 (:start options (random-start board)))
         add-noise))))

(defn- present? [board word]
  (some #(= word %) (map :pattern (:words board))))

(defn- add-word [board [dir [x y]] word]
  "TODO: add a stop # mark at each end of word unless end is board's edge"
  (let [coord #(if (= 0 dir) [y (+ x %)] [(+ y %) x])]
    (if (present? board word)
      board
      {:words (conj (board :words) (Word. dir [x y] word))
       :grid (reduce #(update-in %1 (coord (first %2)) (constantly (last %2)))
                     (:grid board)
                     (map-indexed vector word))})))

(defn- build-slots [c board-word word]
  "retuns a sequence of potential position for word based on one
   intersection of word on the board board-word and word"
  (let [[cx cy] (:coord board-word)]
    (for [[x _] (filter #(= c (last %)) (map-indexed vector (:pattern board-word)))
          [y _] (filter #(= c (last %)) (map-indexed vector word))]
      (do
        ;; (print "--build-slots-- char: " c " bword: " (:pattern board-word) " word: " word)
        (if (= (:direction board-word) 0)
           [1 [(+ cx x) (- cy y)]]
           [0 [(- cx y) (+ cy x)]])))))

(defn- find-slot [word]
  "returns fn that will returns all the potential positions for word based on all the
   intersections with a word on the board"
  (fn [{:keys [bword inter]}]
    (mapcat #(build-slots % bword word) inter)))

(defn- row [board y]
  "returns row at y on board"
  (get (:grid board) y))

(defn- col [board x]
  "returns column at x on board"
  (map #(get % x) (:grid board)))

(defn- collision-free?
  "predicate retuns true word collides with existing words"
  [board [dir [x y]] word]
  (let [seq (if (= dir 0) (row board y) (col board x))
        begin-seq (if (= dir 0) x y)
        char-tuple-seq (partition 2 (interleave (drop begin-seq seq) word))]
    ;; (println "check collisions: " [dir [x y]] word char-tuple-seq )
    (every? #(or (= (first %) (last %))
                 (= (first %) empty-char)) char-tuple-seq)))

(defn- invalid-slot [board word]
  "returns a predicate fn that returns true when proposed
   position for word is invalid when:
   - start x,y is out of bounds
   - horizontal propositon results in last char out of bounds
   - vertical propositon results in last char out of bounds
   - placing word would interfere with existing words"
  (let [max (count (:grid board))
        size (count word)]
    (fn [[dir [x y] :as position]]
      ;; (println "validation: (neg? x): "(neg? x) " (neg? y):" (neg? y)
      ;;        "\n (and (= dir 0) (> (+ x size) max)):" (and (= dir 0) (> (+ x size) max)) " " (+ x size) max
      ;;        "\n (and (= dir 1) (> (+ y size) max)):" (and (= dir 1) (> (+ y size) max)) " " (+ y size) max
      ;;        "\n (not (collision-free? board position word)): ")
      (or (neg? x)
          (neg? y)
          (and (= dir 0) (> (+ x size) max))
          (and (= dir 1) (> (+ y size) max))
          (not (collision-free? board position word))))))

(defn- find-position [board word]
  "Returns the first valid slot for word on the board"
  (let [set-word (set word)
        xform (comp (map #(hash-map :bword % :inter (s/intersection set-word (set (:pattern %)))))
                    (remove #(empty? (:inter %)))
                    (mapcat (find-slot word))
                    ;; (map #(do (print "   position: " % " -- \n") %) )
                    (remove (invalid-slot board word)))]
    (first (sequence xform (reverse (:words board))))))

(defn- place-word [board word]
  (if-let [position (find-position board word)]
    (add-word board position word)
    board))

(defn- random-start
  "Returns a tuple that represents:
  - direction, 0 for horizontal and 1 for vertical
  - position, on the board"
  [board]
  (let [direction (rand-int 2)
        n (rand-int (count (:grid board)))]
    (if (= 0 direction)
      [direction [0 n]]
      [direction [n 0]])))

(defn- fill-board
  "Fills in given board with the words provided"
  ([board word-seq] (fill-board board word-seq (random-start board)))
  ([board word-seq position]
   (let [sorted-word (sort-by count > word-seq)
         init-board (add-word board position (first sorted-word))]
     (reduce place-word init-board word-seq))))

(defn- add-noise [board]
  "Returns a board with empty-char replaced by random alphabet char"
  (let [chars (map char (range (int \a) (+ (int \a) 26)))]
    {:words (:words board)
     :grid (map
            (fn [row] (map #(if (= % empty-char) (rand-nth chars) %) row))
            (:grid board))}))
