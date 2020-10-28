(ns crossword.core-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [crossword.core :as c]))

(deftest test-create
  (testing "create a ofuscated grid"
    (is (= "" (c/create ["arc" "rise" "exit" "rinse" "sins"  "stains" "the"])))))
