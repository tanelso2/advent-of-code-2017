(ns day1.core-test
  (:require [clojure.test :refer :all]
            [day1.core :refer :all]))

(deftest puzzle1-test
  (testing "puzzle1"
    (are [input expected]
      (= expected (puzzle1 input))
      "1122" 3
      "1111" 4
      "91212129" 9)))

(deftest puzzle2-test
  (testing "puzzle2"
    (are [input expected]
      (= expected (puzzle2 input))
      "1212" 6
      "1221" 0
      "123425" 4
      "123123" 12
      "12131415" 4)))
