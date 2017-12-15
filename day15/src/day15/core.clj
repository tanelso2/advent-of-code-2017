(ns day15.core
  (:gen-class))

(defn make-generator
  [n]
  (fn [x]
    (rem
      (* x n)
      2147483647)))

(def generator-a (make-generator 16807))

(def generator-b (make-generator 48271))

(defn do-iterations
  [generator-a generator-b num-iterations a-start b-start]
  (loop [a a-start
         b b-start
         acc 0
         n 0]
    (if (>= n num-iterations)
      acc
      (recur (generator-a a)
             (generator-b b)
             (if (= (bit-and a 0xffff)
                    (bit-and b 0xffff))
               (inc acc)
               acc)
             (inc n)))))

(defn puzzle1
  [a-start b-start]
  (do-iterations generator-a generator-b 40000000 a-start b-start))

(defn make-generator2
  [n d]
  (let [generator (make-generator n)]
    (fn [x]
      (loop [candidate (generator x)]
        (if (= 0 (rem candidate d))
          candidate
          (recur (generator candidate)))))))

(def generator-a2 (make-generator2 16807 4))

(def generator-b2 (make-generator2 48271 8))

(defn puzzle2
  [a-start b-start]
  (do-iterations generator-a2 generator-b2 5000000 a-start b-start))

(def a-start 591)

(def b-start 393)
