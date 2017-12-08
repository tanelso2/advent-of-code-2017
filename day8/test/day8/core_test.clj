(ns day8.core-test
  (:require [clojure.test :refer :all]
            [day8.core :refer :all]
            [clojure.string :as str]))

(def test-input (map parse-line (str/split-lines "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10")))

(deftest puzzle1-test
  (testing "puzzle1"
    (is (= 1 (puzzle1 test-input)))))

(deftest puzzle2-test
  (testing "puzzle2"
    (is (= 10 (puzzle2 test-input)))))
