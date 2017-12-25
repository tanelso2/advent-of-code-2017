(ns day22.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:import (clojure.lang Keyword)))

(def directions [:north :east :south :west])

(defn turn
  [^Keyword current-direction
   ^Keyword turn-direction]
  (if (= -1 (.indexOf directions current-direction))
    (throw (IllegalArgumentException. "Hey that's not a proper direction")))
  (let [curr-idx (.indexOf directions current-direction)
        turn-offset (case turn-direction
                      :left -1
                      :right 1
                      :around 2
                      :dont 0)
        new-idx (mod (+ curr-idx turn-offset) (count directions))]
    (get directions new-idx)))

(defn get-next-coords
  [curr-coords direction]
  (let [offset (case direction
                 :north [-1 0]
                 :south [1 0]
                 :west [0 -1]
                 :east [0 1])]
    (mapv + offset curr-coords)))

(defn parse-infection-string
  [^String infection-string]
  (let [grid (vec (str/split-lines infection-string))
        height (count grid)
        width (count (first grid))
        find-mid #(int (/ % 2))
        mid-point [(find-mid height) (find-mid width)]]
    (->>
      (for [i (range height)
            j (range width)]
        (if (= \# (get-in grid [i j]))
          (mapv - [i j] mid-point)
          nil))
      (remove nil?)
      (into #{}))))

(defn do-burst
  [{:keys [coords infected-squares direction infection-bursts]}]
  (let [infected? (contains? infected-squares coords)
        new-direction (turn direction (if infected?
                                        :right
                                        :left))
        new-coords (get-next-coords coords new-direction)]
    {:coords new-coords
     :infected-squares (if infected?
                         (disj infected-squares coords)
                         (conj infected-squares coords))
     :direction new-direction
     :infection-bursts (if infected?
                         infection-bursts
                         (inc infection-bursts))}))

(defn puzzle1
  [input]
  (let [initial-infection (parse-infection-string input)]
    (:infection-bursts
      (reduce (fn [status _] (do-burst status))
              {:coords [0 0]
               :infected-squares initial-infection
               :direction :north
               :infection-bursts 0}
              (range 10000)))))

(defn get-current-node-status
  [coords infected-squares weakened-squares flagged-squares]
  (cond
    (some? (infected-squares coords)) :infected
    (some? (weakened-squares coords)) :weakened
    (some? (flagged-squares coords)) :flagged
    :else :clean))


(defn do-burst-part-2
  [status]
  (let [{:keys [coords infected-squares weakened-squares flagged-squares direction infection-bursts]} status
        node-status (get-current-node-status coords infected-squares weakened-squares flagged-squares)
        new-direction (turn direction (case node-status
                                        :infected :right
                                        :weakened :dont
                                        :flagged :around
                                        :clean :left))
        new-coords (get-next-coords coords new-direction)
        updated-status (assoc! status
                         :coords new-coords
                         :direction new-direction)]
    (case node-status
      :infected (assoc! updated-status
                  :infected-squares (disj! infected-squares coords)
                  :flagged-squares (conj! flagged-squares coords))
      :weakened (assoc! updated-status
                  :weakened-squares (disj! weakened-squares coords)
                  :infected-squares (conj! infected-squares coords)
                  :infection-bursts (inc infection-bursts))
      :flagged (assoc! updated-status
                 :flagged-squares (disj! flagged-squares coords))
      :clean (assoc! updated-status
               :weakened-squares (conj! weakened-squares coords)))))

(defn puzzle2
  [input]
  (let [initial-infection (parse-infection-string input)]
    (:infection-bursts
      (reduce (fn [status _] (do-burst-part-2 status))
              (transient {:coords [0 0]
                          :infected-squares (transient initial-infection)
                          :weakened-squares (transient #{})
                          :flagged-squares (transient #{})
                          :direction :north
                          :infection-bursts 0})
              (range 10000000)))))

(defn -main
  [& args]
  (let [puzzle-input (slurp (first args))]
    (println (time (puzzle1 puzzle-input)))
    (println (time (puzzle2 puzzle-input)))))
