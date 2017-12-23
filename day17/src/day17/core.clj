(ns day17.core
  (:gen-class))

(defn insert-next
  [curr-pos curr-list number-of-steps next-val]
  (let [length (count curr-list)
        split-pos (inc (mod (+ curr-pos number-of-steps) length))
        [head tail] (split-at split-pos curr-list)]
    [(concat head (list next-val) tail)
     split-pos]))

(defn puzzle1
  [number-of-steps]
  (letfn [(reduction-fn
            [[curr-list curr-pos] next-val]
            (insert-next curr-pos curr-list number-of-steps next-val))]
    (->> (range 1 2018)
      (reduce reduction-fn ['(0) 0])
      (first)
      (drop-while #(not= 2017 %))
      (second))))

(defn puzzle2
  [number-of-steps]
  (letfn [(reduction-fn
            [[val-after-zero curr-pos list-length] next-val]
            (let [split-pos (inc (mod (+ curr-pos number-of-steps) list-length))
                  val-after-zero (if (= 1 split-pos)
                                   next-val
                                   val-after-zero)]
              [val-after-zero split-pos (inc list-length)]))]
    (->> (range 2 (inc 50000000))
         (reduce reduction-fn [1 1 2])
         (first))))

(def puzzle-input 303)

(defn -main
  [& args]
  (println (time (puzzle1 puzzle-input)))
  (println (time (puzzle2 puzzle-input))))
