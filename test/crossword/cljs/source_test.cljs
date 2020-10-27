;; (ns crossword.cljs.source-test
;;   (:require [cljs.test :refer :all]
;;             [crossword.cljs.source :refer :all]))

;; (def testfile "small.txt")

;; (def default-options
;;   {:max-length 7
;;    :min-length 3
;;    :exclude-pattern  #"\s|\d|\W"})

;; (deftest test-words
;;   (testing "returns random sample of words from file"
;;     (let [seq1 (words testfile default-options)
;;           seq2 (words testfile default-options)]
;;       (is (not= seq1 seq2))
;;       (is (= (count seq1) (count (re-find #"\w*" seq1)))))))
