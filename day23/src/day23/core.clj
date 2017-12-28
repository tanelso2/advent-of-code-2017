(ns day23.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn- get-val
 [registers x]
 (try (Integer/parseInt x)
     (catch NumberFormatException e (get registers x 0))))

(def initial-registers
  (apply hash-map
         (interleave
           (map (comp str char) (range (int \a) (inc (int \h))))
           (repeat 0))))

(defn set-register
  [registers & args]
  (let [[x y] args
        y-val (get-val registers y)]
    {:registers (assoc registers x y-val)}))

(defn common-binop
  [binop registers & args]
  (let [[x y] args
        x-val (get registers x 0)
        y-val (get-val registers y)]
    {:registers (assoc registers x (binop x-val y-val))}))

(def sub (partial common-binop -))

(def mul (partial common-binop *))

(defn jnz
  [registers & args]
  (let [[x y] args
        x-val (get-val registers x)
        y-val (get-val registers y)]
    (if (not= x-val 0)
      {:registers registers :jmp-offset y-val}
      {:registers registers})))

(defn parse-command-name
  [command-name]
  (case command-name
    "set" set-register
    "sub" sub
    "mul" mul
    "jnz" jnz))

(defn parse-line
  [line]
  (let [[command-name & args] (str/split line #"\s")]
    [command-name args]))

(defn parse-command-list
  [input]
  (mapv parse-line (str/split-lines input)))

(defn process-commands
  [command-list starting-registers]
  (loop [curr-pos 0
         registers starting-registers
         num-muls 0]
    (let [curr-command (get command-list curr-pos)
          [command-name command-args] curr-command
          command (parse-command-name command-name)
          result-map (apply (partial command registers) command-args)
          next-pos (if-some [offset (:jmp-offset result-map)]
                     (+ curr-pos offset)
                     (inc curr-pos))
          new-num-muls (if (= command-name "mul")
                         (inc num-muls)
                         num-muls)]
      (if (or (< next-pos 0)
              (>= next-pos (count command-list)))
        new-num-muls
        (recur next-pos
               (:registers result-map)
               new-num-muls)))))

(defn puzzle1
  [input]
  (process-commands (parse-command-list input) initial-registers))

(defn is-prime?
  [n]
  (empty? (filter #(= 0 (mod n %)) (range 2 n))))

(defn puzzle2
  [input]
  (let [start-val (+ (* 84
                        100)
                     100000)
        end-val (+ start-val 17000 17)]
    (count
      (filter #(not (is-prime? %)) (range start-val end-val 17)))))

(defn -main
  [& args]
  (let [puzzle-input (slurp (first args))]
    (println (time (puzzle1 puzzle-input)))
    (println (time (puzzle2 puzzle-input)))))
