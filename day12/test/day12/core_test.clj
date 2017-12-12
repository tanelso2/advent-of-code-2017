(ns day12.core-test
  (:require [clojure.test :refer :all]
            [day12.core :refer :all]))

(def test-input "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5")

(deftest puzzle1-test
  (testing "puzzle1"
    (is (= 6 (puzzle1 test-input)))))

(deftest puzzle2-test
  (testing "puzzle2"
    (is (= 2 (puzzle2 test-input)))))
