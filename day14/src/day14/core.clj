(ns day14.core
  (:gen-class)
  (:require [day10.core :as day10]
            [clojure.string :as str]))

(defn- convert-hex-char-to-bin
  [hex-char]
  (str/replace
    (format "%4s"
      (Integer/toBinaryString
        (Integer/parseInt (str hex-char) 16)))
    #" "
    "0"))

(defn- convert-hex-to-bin
  [hex-str]
  (apply str
         (pmap convert-hex-char-to-bin hex-str)))

(defn- get-grid
  [input]
  (pmap (fn [n]
          (->> (str input "-" n)
               (day10/puzzle2)
               (convert-hex-to-bin)
               (#(str/replace % #"0" "."))
               (#(str/replace % #"1" "#"))))
        (range 128)))

(defn- get-neighbors
  [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn- out-of-bounds?
  [[x y]]
  (or (< x 0)
      (< y 0)
      (>= x 128)
      (>= y 128)))

(defn- find-region
  [grid current-region coords]
  (if (or (out-of-bounds? coords)
          (contains? current-region coords)
          (not= \# (get-in grid coords)))
    current-region
    (reduce (fn [region coords]
              (find-region grid region coords))
            (conj current-region coords)
            (get-neighbors coords))))

(defn puzzle1
  [input]
  (->> input
       (get-grid)
       (pmap (fn [row]
               (count (filter #(= \# %) row))))
       (reduce +)))

(defn puzzle2
  [input]
  (let [grid (into [] (get-grid input))]
    (->> (for [x (range 128)
               y (range 128)]
           [x y])
         (pmap (partial find-region grid #{}))
         (distinct)
         (remove #(= #{} %))
         (count))))

(def puzzle-input "oundnydw")