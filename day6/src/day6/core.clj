(ns day6.core
  (:gen-class))

(defn update!
  "Like update, but for transients"
  [m k f]
  (let [orig-val (get m k)]
    (assoc! m k (f orig-val))))

(defn redistribute-blocks
  [block-list pos]
  (let [num-blocks (nth block-list pos)
        length (count block-list)
        blocks (atom (transient block-list))]
    (swap! blocks assoc! pos 0)
    (dotimes [i num-blocks]
      (swap! blocks update! (mod (+ pos i 1) length) inc))
    (persistent! @blocks)))

(defn- find-position-for-redistribution
  [block-list]
  (let [max-val (apply max block-list)]
    (.indexOf block-list max-val)))

(defn puzzle1
  [block-list]
  (loop [iterations 0
         blocks block-list
         seen-configs #{}]
    (if (contains? seen-configs blocks)
      iterations
      (recur (inc iterations)
             (let [pos (find-position-for-redistribution blocks)]
               (redistribute-blocks blocks pos))
             (conj seen-configs blocks)))))

(defn puzzle2
  [block-list]
  (loop [iterations 0
         blocks block-list
         seen-configs {}]
    (if (contains? seen-configs blocks)
      (- iterations (get seen-configs blocks))
      (recur (inc iterations)
             (let [pos (find-position-for-redistribution blocks)]
               (redistribute-blocks blocks pos))
             (assoc seen-configs blocks iterations)))))

(def puzzle-input [2  8  8  5  4  2  3  1  5  5  1  2  15  13  5  14])
