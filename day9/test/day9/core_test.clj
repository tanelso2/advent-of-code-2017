(ns day9.core-test
  (:require [clojure.test :refer :all]
            [day9.core :refer :all]))

(deftest puzzle1-test
  (testing "puzzle1"
    (are [input expected] (= expected (puzzle1 input))
      "{}" 1
      "{{{}}}" 6
      "{{},{}}" 5
      "{{{},{},{{}}}}" 16
      "{<a>,<a>,<a>,<a>}" 1
      "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
      "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
      "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3)))

(deftest puzzle2-test
  (testing "puzzle2"
    (are [input expected] (= expected (puzzle2 input))
      "<>" 0
      "<random characters>" 17
      "<<<<>" 3
      "<{!>}>" 2
      "<!!>" 0
      "<!!!>>" 0
      "<{o\"i!a,<{i<a>" 10)))
