(ns day10.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-circular-sublist
  [coll start length]
  (->> coll
       (cycle)
       (drop start)
       (take length)))

(defn write-circular-sublist
  [coll start sublist]
  (let [coll-length (count coll)]
    (loop [[x & xs] sublist
           ret-list (transient coll)
           curr-pos start]
      (if (nil? x)
        (persistent! ret-list)
        (recur xs
               (assoc! ret-list curr-pos x)
               (mod (+ curr-pos 1) coll-length))))))

(defn reduce-function
  [inter-val x]
  (let [{:keys [ls curr-pos skip-size]} inter-val
        ls-length (count ls)
        new-ls (->> (get-circular-sublist ls curr-pos x)
                    (reverse)
                    (write-circular-sublist ls curr-pos))]
    (assoc inter-val
      :ls new-ls
      :curr-pos (mod (+ curr-pos x skip-size) ls-length)
      :skip-size (+ skip-size 1))))

(defn apply-n-times
  [n f x]
  (reduce
    (fn [v _] (f v))
    x
    (range n)))

(defn puzzle1
  [input]
  (let [starting-val {:ls (into [] (range 256))
                      :curr-pos 0
                      :skip-size 0}]
    (->> (reduce reduce-function starting-val input)
         (:ls)
         (take 2)
         (apply *))))

(defn puzzle2
  [input-str]
  (let [input (concat (map int (str/trim input-str)) [17 31 73 47 23])
        tie-knots (fn [x] (reduce reduce-function x input))
        sixty-four-iterations-later (apply-n-times
                                      64
                                      tie-knots
                                      {:ls (into [] (range 256))
                                       :curr-pos 0
                                       :skip-size 0})]
    (->> (:ls sixty-four-iterations-later)
         (partition 16)
         (map #(apply bit-xor %))
         (map #(format "%02x" %))
         (apply concat)
         (apply str))))


(defn parse-int [x] (Integer/parseInt x))

(def puzzle-input "165,1,255,31,87,52,24,113,0,91,148,254,158,2,73,153")

(def puzzle1-input (map parse-int (str/split puzzle-input #",")))