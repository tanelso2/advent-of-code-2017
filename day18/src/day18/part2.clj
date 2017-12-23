(ns day18.part2
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.core.async :as a
             :refer [go chan <!! >!! close!]]))

(def num-vals-sent (atom {0 0
                          1 0}))

(defn- get-val
  [registers x]
  (try (Integer/parseInt x)
    (catch NumberFormatException e (get registers x 0))))

(defn snd
  [output-chan registers & args]
  (let [[x] args
        x-val (get-val registers x)]
    (>!! output-chan x-val)
    {:registers registers}))

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
  [input-chan registers & args]
  (let [[x] args
        recieved-val (<!! input-chan)]
    (if (nil? recieved-val)
      {:registers registers :shut-it-down true}
      {:registers (assoc registers x recieved-val)})))

(defn parse-command-name
  [command-name input-chan output-chan]
  (case command-name
    "snd" (partial snd output-chan)
    "set" set-register
    "add" add
    "mul" mul
    "mod" modulo
    "jgz" jgz
    "rcv" (partial rcv input-chan)))

(defn process-commands
  [command-list output-chan input-chan process-num]
  (loop [curr-pos 0
         registers {"p" process-num}]
    (let [curr-command (get command-list curr-pos)
          [command-name command-args] curr-command
          command (parse-command-name command-name input-chan output-chan)
          result-map (apply (partial command registers) command-args)
          next-pos (if-some [offset (:jmp-offset result-map)]
                     (+ curr-pos offset)
                     (inc curr-pos))]
      (if (= command-name "snd")
        (swap! num-vals-sent update process-num inc))
      (if (or (< next-pos 0)
              (>= next-pos (count command-list))
              (contains? result-map :shut-it-down))
        result-map
        (recur next-pos
               (:registers result-map))))))

(defn parse-line
  [line]
  (let [[command-name & args] (str/split line #"\s")]
    [command-name args]))

(defn parse-command-list
  [input]
  (mapv parse-line (str/split-lines input)))

(defn puzzle2
  [input seconds-to-wait]
  (let [command-list (parse-command-list input)
        chan0->1 (chan 100)
        chan1->0 (chan 100)
        process0 (go (process-commands command-list chan0->1 chan1->0 0))
        process1 (go (process-commands command-list chan1->0 chan0->1 1))]
    ; I don't want to actually check for deadlock, so let's just let these
    ; goroutines run for a while and then check the result
    (Thread/sleep (* 1000 seconds-to-wait))
    (close! chan0->1)
    (close! chan1->0)
    (get @num-vals-sent 1)))
