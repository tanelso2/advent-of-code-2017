(ns day18.core
  (:gen-class)
  (:require [day18 [part1 :as part1]
                   [part2 :as part2]]))

(def puzzle-input "set i 31\nset a 1\nmul p 17\njgz p p\nmul a 2\nadd i -1\njgz i -2\nadd a -1\nset i 127\nset p 316\nmul p 8505\nmod p a\nmul p 129749\nadd p 12345\nmod p a\nset b p\nmod b 10000\nsnd b\nadd i -1\njgz i -9\njgz a 3\nrcv b\njgz b -1\nset f 0\nset i 126\nrcv a\nrcv b\nset p a\nmul p -1\nadd p b\njgz p 4\nsnd a\nset a b\njgz 1 3\nsnd b\nset f 1\nadd i -1\njgz i -11\nsnd a\njgz f -16\njgz a -19")

(defn -main
  [& args]
  (let [how-long-to-wait (if-some [s (first args)]
                           (Long/parseLong s)
                           30)]
    (time (println (part1/puzzle1 puzzle-input)))
    (time (println (part2/puzzle2 puzzle-input how-long-to-wait)))
    (shutdown-agents)))
