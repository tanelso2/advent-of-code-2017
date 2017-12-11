(ns day11.core-test
  (:require [clojure.test :refer :all]
            [day11.core :refer :all]))

(deftest puzzle1-test
  (testing "puzzle1"
    (are [input expected] (= expected (puzzle1 input))
      "ne,ne,ne" 3
      "ne,ne,sw,sw" 0
      "ne,ne,s,s" 2
      "se,sw,se,sw,sw" 3)))
