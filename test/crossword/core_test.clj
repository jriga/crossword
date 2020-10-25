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
  (is (= [[\. \.] [\. \.]] @(create-board 2)))
  (is (= [[\. \. \.] [\. \. \.] [\. \. \.]] @(create-board 3)))
  (testing "default arguments"
    (is (= (:max-length c/default-options) (count @(create-board))))))

(deftest test-random-start
  (let [random-start #'c/random-start
        board (create-board)
        [direction [x y]] (random-start board)]
    (is (= 2 (count (random-start board))))
    (is (> 2 direction))
    (is (>= x 0))
    (is (>= y 0))
    (is (< x (count @board)))
    (is (< y (count @board)))))

(deftest test-seq-to-re-str
  (let [seq-to-re-str #'c/seq-to-re-str]
    (is (= ".{7}" (seq-to-re-str (map (fn [_] c/empty-char) (range 7)))))
    (is (= ".{2}a{1}.{1}e{1}.{2}" (seq-to-re-str [c/empty-char c/empty-char \a c/empty-char \e c/empty-char c/empty-char])))))

(deftest test-find-space
  (let [empty-board (create-board)
        board (atom
               [[c/empty-char c/empty-char \a c/empty-char \e c/empty-char c/empty-char]
                [c/empty-char c/empty-char \r c/empty-char \x c/empty-char c/empty-char]
                [\r           \e           \s \e           \c \t           \s          ]
                [c/empty-char c/empty-char \o c/empty-char \i c/empty-char c/empty-char]
                [c/empty-char c/empty-char \n c/empty-char \t c/empty-char c/empty-char]])
        find-space #'c/find-space]
    (testing "horizontal search"
      (is (= {:dir 0 :coord [0 0] :pattern ".{7}"} (find-space empty-board [0 [0 0]] 3)))
      (is (= {:dir 0 :coord [0 0] :pattern ".{2}a{1}.{1}e{1}.{2}"} (find-space board [0 [0 0]] 3))))
    (testing "vertical search"
      (is (= {:dir 1 :coord [0 0] :pattern ".{2}r{1}.{2}"} (find-space board [1 [0 0]] 3))))))

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
              [\. \. \. \. \. \. \.]] @board)))
    (testing "add word vertically"
      (add-word board [1 [3 1]] "arcs")
      (is (= [[\. \. \. \. \. \. \.]
              [\. \. \. \a \r \c \s]
              [\. \. \. \r \. \. \.]
              [\. \. \. \c \. \. \.]
              [\. \. \. \s \. \. \.]
              [\. \. \. \. \. \. \.]
              [\. \. \. \. \. \. \.]] @board)))))
