(ns day5.core-test
  (:require [clojure.test :refer :all]
            [day5.core :refer :all]))

(deftest puzzle1-test
  (testing "puzzle1"
    (is (= 5 (puzzle1 [0 3 0 1 -3])))))

(deftest puzzle2-test
  (testing "puzzle2"
    (is (= 10 (puzzle2 [0 3 0 1 -3])))))
