(ns day6.core-test
  (:require [clojure.test :refer :all]
            [day6.core :refer :all]))

(deftest puzzle1-test
  (testing "puzzle1"
    (is (= 5 (puzzle1 [0 2 7 0])))))

(deftest puzzle2-test
  (testing "puzzle2"
    (is (= 4 (puzzle2 [0 2 7 0])))))
