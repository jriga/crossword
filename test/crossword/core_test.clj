(ns crossword.core-test
  (:require [clojure.test :refer :all]
            [crossword.core :as c :refer :all]))

(def testfile "small.txt")


(deftest test-default-options
  (testing "max length"
    (is (= 7 (:max-length c/default-options))))
  (testing "min length"
    (is (= 3 (:min-length c/default-options))))
  (testing "exclude pattern"
    (is (= (.pattern #"\s|\d|\W") (.pattern (:exclude-pattern c/default-options))))))

(deftest test-words
  (testing "returns random sample of words from file"
    (letfn [(testfn [] (take 10 testfile))]
      (repeatedly 5
                  #(is (not= (testfn) (testfn)))))))

(deftest test-create-board
  (is (= [[\. \.] [\. \.]] (@(create-board 2) :grid)))
  (is (= [[\. \. \.] [\. \. \.] [\. \. \.]] (@(create-board 3) :grid)))
  (testing "default arguments"
    (is (= (:max-length c/default-options) (count (@(create-board) :grid))))))

(deftest test-random-start
  (let [random-start #'c/random-start
        board (create-board)
        size (count (@board :grid))
        [direction [x y]] (random-start board)]
    (is (= 2 (count (random-start board))))
    (is (> 2 direction))
    (is (>= x 0))
    (is (>= y 0))
    (is (< x size))
    (is (< y size))))

(deftest test-seq-to-re-str
  (let [seq-to-re-str #'c/seq-to-re-str]
    (is (= ".{7}" (seq-to-re-str (map (fn [_] c/empty-char) (range 7)))))
    (is (= ".{2}a{1}.{1}e{1}.{2}" (seq-to-re-str [c/empty-char c/empty-char \a c/empty-char \e c/empty-char c/empty-char])))))

(deftest test-add-word
  (let [add-word #'c/add-word
        board (create-board)]
    (testing "add word horizontally"
      (add-word board [0 [3 1]] "arcs")
      (is (= [[\. \. \. \. \. \. \.]
              [\. \. \. \a \r \c \s]
              [\. \. \. \. \. \. \.]
              [\. \. \. \. \. \. \.]
              [\. \. \. \. \. \. \.]
              [\. \. \. \. \. \. \.]
              [\. \. \. \. \. \. \.]] (@board :grid))))
    (testing "add word vertically"
      (add-word board [1 [3 1]] "arcs")
      (is (= [[\. \. \. \. \. \. \.]
              [\. \. \. \a \r \c \s]
              [\. \. \. \r \. \. \.]
              [\. \. \. \c \. \. \.]
              [\. \. \. \s \. \. \.]
              [\. \. \. \. \. \. \.]
              [\. \. \. \. \. \. \.]] (@board :grid))))))


(def word-list
  [["arc"    [0 [0 0]]]
   ["rise"   [1 [1 0]]]
   ["exit"   [0 [1 3]]]
   ["rinse"  [1 [3 2]]]
   ["sins"   [0 [3 5]]]
   ["stains" [1 [6 0]]]
   ["the"    [0 [1 6]]]])


(defn- fill-board-with [words]
  (let [board (create-board)
        add-word #'c/add-word]
    (doseq [[w pos] words]
      (add-word board pos w))
    board))

(def full-board (fill-board-with word-list))

(deftest test-filled?
  (let [filled? #'c/filled?
        board (create-board)]
    (testing "when board empty"
      (is (not (filled? board))))
    (testing "when board have 7 or more words"
      (is (filled? full-board)))))

(deftest test-col
  (is (= [\r \i \s \e \. \. \t] ((var c/col) full-board 1))))

(deftest test-row
  (is (= [\. \e \x \i \t \. \i] ((var c/row) full-board 3))))

(deftest test-find-coord
  (let [find-coord #'c/find-coord
        w (->Word 1 [1 0] "rise")
        [dir [x y]] (find-coord \i w)]
    (is (= dir 0))
    (is (= x 1))
    (is (= y 1))))


(deftest test-word-fit-at-intersection-at
  (let [fun #'c/word-fit-at-intersection-at
        board (fill-board-with (drop-last 5 word-list))
        word "exited"
        pred-at-idx (fun 0 1 3 board word)]
    (is (pred-at-idx 0))
    (is (not (pred-at-idx 4)))))

(deftest test-can-place
  (let [can-place #'c/can-place
        board (fill-board-with (drop-last 5 word-list))
        word "exited"
        position [0 [1 3]]]
    (is (= 1 (count (can-place position board word))))
    (is (= [0 [1 3]] (first (can-place position board word))))))
;; (deftest test-find-space
;;   (let [empty-board (create-board)
;;         board (atom
;;                [[c/empty-char c/empty-char \a c/empty-char \e c/empty-char c/empty-char]
;;                 [c/empty-char c/empty-char \r c/empty-char \x c/empty-char c/empty-char]
;;                 [\r           \e           \s \e           \c \t           \s          ]
;;                 [c/empty-char c/empty-char \o c/empty-char \i c/empty-char c/empty-char]
;;                 [c/empty-char c/empty-char \n c/empty-char \t c/empty-char c/empty-char]])
;;         find-space #'c/find-space]
;;     (testing "horizontal search"
;;       (is (= {:dir 0 :coord [0 0] :pattern ".{7}"} (find-space empty-board [0 [0 0]] 3)))
;;       (is (= {:dir 0 :coord [0 0] :pattern ".{2}a{1}.{1}e{1}.{2}"} (find-space board [0 [0 0]] 3))))
;;     (testing "vertical search"
;;       (is (= {:dir 1 :coord [0 0] :pattern ".{2}r{1}.{2}"} (find-space board [1 [0 0]] 3))))))
