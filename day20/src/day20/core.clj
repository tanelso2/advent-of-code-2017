(ns day20.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn update-particle
  [particle]
  (let [{:keys [pos vel acc num]} particle
        new-vel (mapv + vel acc)
        new-pos (mapv + pos new-vel)]
    (assoc particle :pos new-pos :vel new-vel)))

(defn parse-particle
  [line-num line]
  (let [regex #"(?:-?\d+,){2}-?\d+" ; regex found entirely by trial and error
        [pos vel acc] (->> (re-seq regex line)
                           (map #(str/split % #","))
                           (map (fn [x] (map #(Integer/parseInt %) x)))
                           (map vec))]
    {:pos pos
     :vel vel
     :acc acc
     :num line-num}))

(defn particle-distance
  [{:keys [pos]}]
  (->> pos
    (mapv #(Math/abs %))
    (apply +)))

(defn do-iterations
  [particles num-iterations]
  (loop [particles particles
         iteration 0]
    (if (>= iteration num-iterations)
      particles
      (recur (pmap update-particle particles)
             (inc iteration)))))

(defn parse-input
  [input]
  (map-indexed parse-particle (str/split-lines input)))

(defn puzzle1
  [input]
  (let [initial-particles (parse-input input)
        final-particles (do-iterations initial-particles 1000)]
    (->> final-particles
         (apply min-key particle-distance)
         (:num))))

(defn remove-colliding-particles
  [particles]
  (->> particles
       (group-by :pos) ; group particles by position
       (vals) ; get the groups, no need for the key
       (filter #(= 1 (count %))) ; take only the groups with one particle
       (map first))) ; take the first of every group

(defn do-iterations-with-collisions
  [particles num-iterations]
  (loop [particles particles
         iteration 0]
    (if (>= iteration num-iterations)
      particles
      (recur (->> particles
                  (pmap update-particle)
                  (remove-colliding-particles))
             (inc iteration)))))

(defn puzzle2
  [input]
  (let [initial-particles (remove-colliding-particles (parse-input input))
        final-particles (do-iterations-with-collisions initial-particles 1000)]
    (count final-particles)))



(defn -main
  [& args]
  (let [puzzle-input (slurp (first args))]
    (println (time (puzzle1 puzzle-input)))
    (println (time (puzzle2 puzzle-input))))
  (shutdown-agents))

