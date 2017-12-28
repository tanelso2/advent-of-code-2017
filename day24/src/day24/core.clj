(ns day24.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn remove-one
  [coll x]
  (let [[head tail] (split-with #(not= x %) coll)]
    (concat head (rest tail))))

(defn get-available-connectors
  [connectors num-pins]
  (if (nil? num-pins)
    connectors
    (filter #(not= -1 (.indexOf % num-pins)) connectors)))

(defn get-other-pin
  [connector pin]
  (first
    (remove-one connector pin)))

(defn connection-power
  [connectors]
  (reduce +
    (flatten connectors)))

(def get-most-connectors-rec
  (memoize
    (fn [connectors-left last-pin max-func]
      (let [valid-connectors (get-available-connectors connectors-left last-pin)]
        (if (empty? valid-connectors)
          '()
          (max-func
            (for [connector valid-connectors]
              (conj
                (get-most-connectors-rec (remove-one connectors-left connector)
                                         (get-other-pin connector last-pin)
                                         max-func)
                connector))))))))

(defn get-most-connectors
  [max-func connectors-list]
  (get-most-connectors-rec connectors-list 0 max-func))

(defn parse-connector
  [line]
  (->> (str/split line #"/")
       (mapv #(Integer/parseInt %))))

(defn parse-connectors
  [input]
  (map parse-connector (str/split-lines input)))

(defn puzzle1
  [input]
  (->> input
       (parse-connectors)
       (get-most-connectors (partial apply max-key connection-power))
       (connection-power)))

(defn max-keys
  [ks & coll]
  (let [[k & ks] ks
        max-val (apply max (map k coll))
        maxes (filter #(= max-val (k %)) coll)]
    (if (or (nil? ks)
            (= 1 (count maxes)))
      (first maxes)
      (recur ks
             maxes))))

(defn puzzle2
  [input]
  (->> input
       (parse-connectors)
       (get-most-connectors (partial apply max-keys [count connection-power]))
       (connection-power)))


(def puzzle-input "48/5\n25/10\n35/49\n34/41\n35/35\n47/35\n34/46\n47/23\n28/8\n27/21\n40/11\n22/50\n48/42\n38/17\n50/33\n13/13\n22/33\n17/29\n50/0\n20/47\n28/0\n42/4\n46/22\n19/35\n17/22\n33/37\n47/7\n35/20\n8/36\n24/34\n6/7\n7/43\n45/37\n21/31\n37/26\n16/5\n11/14\n7/23\n2/23\n3/25\n20/20\n18/20\n19/34\n25/46\n41/24\n0/33\n3/7\n49/38\n47/22\n44/15\n24/21\n10/35\n6/21\n14/50\n")
