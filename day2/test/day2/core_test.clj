(ns day2.core-test
  (:require [clojure.test :refer :all]
            [day2.core :refer :all]))

(deftest puzzle1-test
  (testing "Puzzle 1"
    (is (= 18 (puzzle1 (parse-input "5 1 9 5\n7 5 3\n2 4 6 8"))))))

(deftest find-even-divisors-test
  (testing "Find even divisors")
  (are [input expected] (= expected (find-even-divisors input))
    '(5 9 2 8) 4
    '(9 4 7 3) 3
    '(3 8 6 5) 2))


(deftest puzzle2-test
  (testing "puzzle 2"
    (is (= 9 (puzzle2 (parse-input "5 9 2 8\n9 4 7 3\n3 8 6 5"))))))
