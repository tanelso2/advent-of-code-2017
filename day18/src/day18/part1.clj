(ns day18.part1
  (:gen-class)
  (:require [clojure.string :as str]))

(defn- get-val
  [registers x]
  (try (Integer/parseInt x)
    (catch NumberFormatException e (get registers x 0))))

(defn snd
  [registers & args]
  (let [[x] args
        played-val (get-val registers x)]
    {:registers (assoc registers :last-val-played played-val)}))

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

(def add (partial common-binop +))

(def mul (partial common-binop *))

(def modulo (partial common-binop mod))

(defn jgz
  [registers & args]
  (let [[x y] args
        x-val (get-val registers x)
        y-val (get-val registers y)]
    (if (> x-val 0)
      {:registers registers :jmp-offset y-val}
      {:registers registers})))

(defn rcv
  [registers & args]
  (let [[x] args
        x-val (get-val registers x)]
    (if (not= x-val 0)
      {:registers registers :recovered-val (:last-val-played registers)}
      {:registers registers})))


(defn process-commands
  [command-list]
  (loop [curr-pos 0
         registers {}]
    (let [curr-command (get command-list curr-pos)
          [command command-args] curr-command
          result-map (apply (partial command registers) command-args)
          next-pos (if-some [offset (:jmp-offset result-map)]
                     (+ curr-pos offset)
                     (inc curr-pos))]
      (if-some [recovered-val (:recovered-val result-map)]
        recovered-val
        (recur next-pos
               (:registers result-map))))))

(defn parse-command-name
  [command-name]
  (case command-name
    "snd" snd
    "set" set-register
    "add" add
    "mul" mul
    "mod" modulo
    "jgz" jgz
    "rcv" rcv))

(defn parse-line
  [line]
  (let [[command-name & args] (str/split line #"\s")
        command (parse-command-name command-name)]
    [command args]))

(defn parse-command-list
  [input]
  (mapv parse-line (str/split-lines input)))

(defn puzzle1
  [input]
  (process-commands (parse-command-list input)))
