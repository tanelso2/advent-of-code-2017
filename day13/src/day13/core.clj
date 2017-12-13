(ns day13.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-scanner-location-for-range
  [round size]
  (let [repeat-length (- (* 2 size) 2)
        place (mod round repeat-length)]
    (if (< place size)
      place
      (- size
        (+ 2 place (- size))))))

(defn parse-line
  [line]
  (str/split line #": "))

(defn get-layer-map
  [input]
  (->> (str/split-lines input)
       (map parse-line)
       (apply concat)
       (map #(Integer/parseInt %))
       (apply array-map)))

(defn get-severity-of-layer
  [layer size delay-len]
  (if (= 0 (get-scanner-location-for-range (+ layer delay-len) size))
    (* layer size)
    0))

(defn get-severity-of-crossing
  [layer-map delay-len]
  (->> layer-map
    (map (fn [[layer size]]
           (get-severity-of-layer layer size delay-len)))
    (reduce +)))

(defn caught-at-layer?
  [layer size delay-len]
  (= 0 (get-scanner-location-for-range (+ layer delay-len) size)))

(defn caught-during-crossing?
  [layer-map delay-len]
  (some (fn [[layer size]]
         (caught-at-layer? layer size delay-len))
        layer-map))

(defn puzzle1
  [input]
  (let [layer-map (get-layer-map input)]
    (get-severity-of-crossing layer-map 0)))

(defn puzzle2
  [input]
  (let [layer-map (get-layer-map input)]
    (first
      (filter
        (complement (partial caught-during-crossing? layer-map))
        (iterate inc 0))))) ; infinite list of numbers

(def puzzle-input "0: 3\n1: 2\n2: 4\n4: 8\n6: 5\n8: 6\n10: 6\n12: 4\n14: 6\n16: 6\n18: 17\n20: 8\n22: 8\n24: 8\n26: 9\n28: 8\n30: 12\n32: 12\n34: 10\n36: 12\n38: 12\n40: 8\n42: 12\n44: 12\n46: 10\n48: 12\n50: 12\n52: 14\n54: 14\n56: 12\n58: 14\n60: 14\n62: 14\n64: 14\n66: 14\n68: 12\n70: 14\n72: 14\n74: 14\n76: 14\n80: 18\n82: 14\n90: 18")

