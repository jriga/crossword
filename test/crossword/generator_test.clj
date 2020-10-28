(ns crossword.generator-test
  (:require [clojure.test :refer :all]
            [crossword.generator :as c :refer :all]))

(def testfile "small.txt")

(deftest test-default-options
  (testing "max length"
    (is (= 7 (:max-length c/default-options))))
  (testing "min length"
    (is (= 3 (:min-length c/default-options))))
  (testing "exclude pattern"
    (is (= (.pattern #"\s|\d|\W") (.pattern (:exclude-pattern c/default-options))))))

(deftest test-record-word
  (testing "attributes"
    (is (= [:direction :coord :pattern] (keys (c/->Word 1 [0 0] "foo"))))))

(deftest test-words
  (testing "returns random sample of words from file"
    (letfn [(testfn [] (take 10 testfile))]
      (repeatedly 5 #(is (not= (testfn) (testfn)))))))

(deftest test-create-board
  (is (= [[\. \.] [\. \.]] (:grid (create-board 2))))
  (is (= [[\. \. \.] [\. \. \.] [\. \. \.]] (:grid (create-board 3))))
  (testing "default arguments"
    (is (= (:max-length c/default-options) (count (:grid (create-board)))))))

(deftest test-random-start
  (let [random-start #'c/random-start
        board (create-board)
        size (count (:grid board))
        [direction [x y]] (random-start board)]
    (is (= 2 (count (random-start board))))
    (is (> 2 direction))
    (is (>= x 0))
    (is (>= y 0))
    (is (< x size))
    (is (< y size))))

(defn- fill-board-with
  ([words] (fill-board-with (create-board) words))
  ([board words]
   (let [add-word #'c/add-word]
     (reduce #(add-word %1 (last %2) (first %2)) board words))))

(deftest test-add-word
  (let [add-word #'c/add-word
        board (create-board)]
    (testing "add word horizontally"
      (is (= [[\. \. \. \. \. \. \.]
              [\. \. \. \a \r \c \s]
              [\. \. \. \. \. \. \.]
              [\. \. \. \. \. \. \.]
              [\. \. \. \. \. \. \.]
              [\. \. \. \. \. \. \.]
              [\. \. \. \. \. \. \.]]
             (:grid (add-word board [0 [3 1]] "arcs")))))
    (testing "add word vertically"
      (is (= [[\. \. \. \. \. \. \.]
              [\. \. \. \a \. \. \.]
              [\. \. \. \r \. \. \.]
              [\. \. \. \c \. \. \.]
              [\. \. \. \s \. \. \.]
              [\. \. \. \. \. \. \.]
              [\. \. \. \. \. \. \.]]
             (:grid (add-word board [1 [3 1]] "arcs")))))
    (testing "does nothing when word pattern already present"
      (is (= [[\. \. \. \. \. \. \.]
              [\. \. \. \a \. \. \.]
              [\. \. \. \r \. \. \.]
              [\. \. \. \c \. \. \.]
              [\. \. \. \s \. \. \.]
              [\. \. \. \. \. \. \.]
              [\. \. \. \. \. \. \.]]
             (:grid (fill-board-with [["arcs" [1 [3 1]]]
                                      ["arcs" [1 [3 1]]]])))))
    (testing "add new word to board words list"
      (is (= [(->Word 1 [3 1] "arcs")] (:words (add-word board [1 [3 1]] "arcs")))))))


(def word-list
  [["arc"    [0 [0 0]]]
   ["rise"   [1 [1 0]]]
   ["exit"   [0 [1 3]]]
   ["rinse"  [1 [3 2]]]
   ["sins"   [0 [3 5]]]
   ["stains" [1 [6 0]]]
   ["the"    [0 [1 6]]]])


(deftest test-helpers
  (let [full-board (fill-board-with word-list)]
    (testing "col"
      (is (= [\r \i \s \e \. \. \t] ((var c/col) full-board 1))))
    (testing "row"
      (is (= [\. \e \x \i \t \. \i] ((var c/row) full-board 3))))))


(deftest test-build-slots
  (let [build-slots #'c/build-slots
        vertical-word (c/->Word 1 [3 1] "nine")
        horizontal-word (c/->Word 0 [3 1] "nine")
        word "netten"]
    (is (= [[0 [3 1]]
            [0 [-2 1]]
            [0 [3 3]]
            [0 [-2 3]]]
           (build-slots \n vertical-word word)))
    (is (= [[0 [2 4]]
            [0 [-1 4]]]
           (build-slots \e vertical-word word)))
    (is (= [[1 [6 0]]
            [1 [6 -3]]]
           (build-slots \e horizontal-word word)))))

(deftest test-find-slot
  (let [find-slot #'c/find-slot
        vertical-word (->Word 1 [3 1] "nine")
        horizontal-word (->Word 0 [3 1] "nine")
        word "netten"]
    (is (= [[0 [2 4]]
            [0 [-1 4]]
            [0 [3 1]]
            [0 [-2 1]]
            [0 [3 3]]
            [0 [-2 3]]]
           ((find-slot word) {:bword vertical-word :inter #{\n \e}})))))

(deftest test-invalid-slot
  (let [invalid-slot #'c/invalid-slot
        board (fill-board-with (drop-last 3 word-list))]
    (testing "start position out of bounds"
      (is ((invalid-slot board "tether") [0 [-1 6]])))
    (testing "end position out of bounds"
      (is ((invalid-slot board "sinister") [0 [3 5]])))
    (testing "collides with other existing word"
      (is ((invalid-slot board "car") [0 [1 2]])))))

(deftest test-find-position
  (let [find-position #'c/find-position
        board (fill-board-with (drop-last 3 word-list))
        actual (find-position board "sins")]
    (testing "returns one of valid positions"
      (is (some #(= % actual) [[0 [1 4]] [0 [3 5]] [0 [0 5]] [0 [0 1]]])))
    (testing "returns nil otherwise"
      (is (= nil (find-position board "sinister"))))))

(deftest test-place-word
  (let [place-word #'c/place-word
        board (fill-board-with (drop-last 3 word-list))]
    (testing "add word when a position is found"
      (is (some #(= "sins" (:pattern %)) (:words (place-word board "sins")))))
    (testing "returns original board when no position found"
      (is (empty? (filter #(= "sinister" (:pattern %)) (:words (place-word board "sinister"))))))))

(deftest test-fill-board
  (let [fill-board #'c/fill-board
        board (create-board)]
    (is (not (empty? (:words (fill-board board ["arcs" "sins" "stains"])))))))

(deftest test-add-noise
  (let [full-board (fill-board-with word-list)
        actual ((var c/add-noise) full-board)]
    (doseq [x (range 7) y (range 7)]
      (is (not= c/empty-char (get-in actual [x y]))))))
