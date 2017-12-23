(ns day19.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn is-letter?
  [character]
  (let [char-val (int character)]
    (and (>= char-val (int \A))
         (<= char-val (int \Z)))))

(defn is-part-of-path?
  [grid coords]
  (not=
    (get-in grid coords \space)
    \space))

(defn get-next-space
  [coords direction]
  (mapv + coords
       (case direction
         :up [-1 0]
         :down [1 0]
         :left [0 -1]
         :right [0 1])))

(defn get-alternate-directions
  [direction]
  (let [axes [[:up :down] [:left :right]]]
    (->> axes
         (remove #(not=
                     -1
                     (.indexOf % direction)))
         (first))))

(defn find-alternate-route
  [grid coords direction]
  (let [alternate-directions (get-alternate-directions direction)]
    (->> alternate-directions
      (filter #(is-part-of-path? grid (get-next-space coords %)))
      (first))))

(defn travel-path
  [grid starting-coords]
  (loop [curr-coords starting-coords
         curr-dir :down
         letters-seen []
         steps-gone 0]
    (let [curr-space (get-in grid curr-coords)
          same-direction-coords (get-next-space curr-coords curr-dir)
          updated-letters-seen (if (is-letter? curr-space)
                                 (conj letters-seen curr-space)
                                 letters-seen)
          updated-steps-gone (inc steps-gone)]
      (if (is-part-of-path? grid same-direction-coords)
        (recur same-direction-coords
               curr-dir
               updated-letters-seen
               updated-steps-gone)
        (let [alternate-direction (find-alternate-route grid curr-coords curr-dir)]
          (if (nil? alternate-direction)
            {:letters-seen updated-letters-seen
             :steps-gone updated-steps-gone}
            (recur (get-next-space curr-coords alternate-direction)
                   alternate-direction
                   updated-letters-seen
                   updated-steps-gone)))))))

(defn find-starting-coords
  [grid]
  (let [first-line (first grid)
        x-coord (str/index-of first-line \|)]
    [0 x-coord]))

(defn parse-input
  [input]
  (str/split-lines input))

(defn puzzle1
  [input]
  (let [grid (parse-input input)
        starting-coords (find-starting-coords grid)]
    (apply str
           (:letters-seen
             (travel-path grid starting-coords)))))

(defn puzzle2
  [input]
  (let [grid (parse-input input)
        starting-coords (find-starting-coords grid)]
     (:steps-gone
       (travel-path grid starting-coords))))

(defn -main
  [& args]
  (let [puzzle-input (slurp (first args))]
    (println (time (puzzle1 puzzle-input)))
    (println (time (puzzle2 puzzle-input)))))
