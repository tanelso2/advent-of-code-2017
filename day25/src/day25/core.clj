(ns day25.core
  (:gen-class))

(defn curr-space-0?
  [tape-markings curr-pos]
  (not= 1 (get tape-markings curr-pos 0)))

(defn transition-func
  [val-to-write dir next-state]
  (fn [tape-markings curr-pos]
    [(assoc tape-markings curr-pos val-to-write)
     ((case dir
       :left dec
       :right inc) curr-pos)
     next-state]))

; This shouldn't be hard coded.
; I should have parsed this from the input
(defn get-next-state
  [tape-markings curr-pos curr-state]
  (let [curr-space-0 (curr-space-0? tape-markings curr-pos)]
    ((case curr-state
      :A
      (if curr-space-0
        (transition-func 1 :right :B)
        (transition-func 0 :left :B))
      :B
      (if curr-space-0
        (transition-func 0 :right :C)
        (transition-func 1 :left :B))
      :C
      (if curr-space-0
        (transition-func 1 :right :D)
        (transition-func 0 :left :A))
      :D
      (if curr-space-0
        (transition-func 1 :left :E)
        (transition-func 1 :left :F))
      :E
      (if curr-space-0
        (transition-func 1 :left :A)
        (transition-func 0 :left :D))
      :F
      (if curr-space-0
        (transition-func 1 :right :A)
        (transition-func 1 :left :E)))
     tape-markings curr-pos)))


(defn do-turing-loop
  [num-iterations]
  (loop
    [tape-markings {}
     curr-pos 0
     curr-state :A
     iteration 0]
    (if
      (> iteration num-iterations)
      tape-markings
      (let [[new-tape-markings new-pos new-state] (get-next-state tape-markings curr-pos curr-state)]
        (recur new-tape-markings
               new-pos
               new-state
               (inc iteration))))))

(defn puzzle1
  []
  (->> (do-turing-loop 12629077)
       (vals)
       (filter (partial = 1))
       (count)))
