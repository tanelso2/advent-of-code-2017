(ns day1.core
  (:gen-class))

(defn puzzle1
  [str]
  (reduce +
    (map-indexed
      (fn [idx itm]
        (let [length (count str)
              comparison-idx (mod (- idx 1) length)
              comparison-itm (nth str comparison-idx)]
          (if (= itm comparison-itm)
            (Character/getNumericValue itm)
            0)))
      str)))

(defn puzzle2
  [str]
  (reduce +
    (map-indexed
      (fn [idx itm]
        (let [length (count str)
              comparison-idx (mod (+ idx (/ length 2)) length)
              comparison-itm (nth str comparison-idx)]
          (if (= itm comparison-itm)
            (Character/getNumericValue itm)
            0)))
      str)))
