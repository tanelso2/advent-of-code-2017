(ns day21.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn apply-n-times
  [n f x]
  (reduce
    (fn [v _] (f v))
    x
    (range n)))

(defn flip-sides
  [graph]
  (->> graph
    (map reverse)
    (mapv vec)))

(defn flip-top-bottom
  [graph]
  (vec (reverse graph)))

(defn rotate
  [graph]
  (->> graph
       (apply interleave)
       (partition (count graph))
       (reverse)
       (mapv vec)))

(defn generate-all-permutations
  [graph]
  (->> [rotate rotate rotate flip-top-bottom flip-sides]
       (combo/subsets)
       (map combo/permutations)
       (apply concat)
       (map #(apply comp %))
       (map #(% graph))
       (into #{})))

(defn substitution
  [substitution-map graph]
  (->> substitution-map
       (filter (fn [[key _]]
                 (contains? key graph)))
       (first)
       (val)))

(defn do-substitutions-to-whole-grid
  [substitution-map grid]
  (let [partition-size (if (even? (count grid))
                         2
                         3)
        num-patterns-in-row (/ (count grid) partition-size)]
    ; This mess was found with trial and error in the REPL
    (->> grid
         (map (partial partition partition-size))
         (apply interleave)
         (partition partition-size)
         (map (partial mapv vec))
         (map (partial substitution substitution-map))
         (partition num-patterns-in-row)
         (apply interleave)
         (partition num-patterns-in-row)
         (map (partial apply interleave))
         (apply concat)
         (partition num-patterns-in-row)
         (map (partial apply concat))
         (mapv vec))))

(defn- parse-graph-string
  [^String graph-str]
  (mapv vec
    (str/split graph-str #"/")))

(defn- parse-line
  [^String line]
  (let [[k v] (map parse-graph-string (str/split line #" => "))
        graph-permutations (generate-all-permutations k)]
    [graph-permutations v]))

(defn- parse-input
  [^String input]
  (->> input
       (str/split-lines)
       (map parse-line)
       (apply concat)
       (apply hash-map)))

(def initial-pattern (parse-graph-string ".#./..#/###"))

(defn do-iterations
  [input num-iterations]
  (let [substitution-map (parse-input input)]
    (apply-n-times
      num-iterations
      (partial do-substitutions-to-whole-grid substitution-map)
      initial-pattern)))

(defn count-in-nested-coll
  [coll x]
  (->> coll
       (flatten)
       (filter (partial = x))
       (count)))

(defn puzzle1
  [input]
  (count-in-nested-coll
    (do-iterations input 5)
    \#))

(defn puzzle2
  [input]
  (count-in-nested-coll
    (do-iterations input 18)
    \#))


(def puzzle-input "../.. => .##/.##/###\n#./.. => .../#.#/###\n##/.. => .##/.../.#.\n.#/#. => ###/.#./##.\n##/#. => .#./#../#.#\n##/## => .##/#.#/###\n.../.../... => ####/.##./####/.#..\n#../.../... => ..../..##/#.../.##.\n.#./.../... => #.#./##.#/#.../#.#.\n##./.../... => .#../.##./#.../....\n#.#/.../... => ###./..##/..##/##.#\n###/.../... => .###/#.##/..../....\n.#./#../... => ##.#/#..#/.##./...#\n##./#../... => ..../#..#/#.#./...#\n..#/#../... => #.##/.#../.#.#/###.\n#.#/#../... => ##../.#.#/...#/...#\n.##/#../... => ##.#/.##./..#./##.#\n###/#../... => ...#/####/..#./#...\n.../.#./... => ##.#/#.#./..##/.##.\n#../.#./... => .#.#/#.##/.##./....\n.#./.#./... => #..#/#.../.##./....\n##./.#./... => ###./###./..##/#..#\n#.#/.#./... => .###/...#/###./###.\n###/.#./... => ...#/..##/..#./#.##\n.#./##./... => .##./.#../...#/..#.\n##./##./... => .###/..#./.###/###.\n..#/##./... => .#.#/..#./..#./...#\n#.#/##./... => .#.#/##../#.../.##.\n.##/##./... => .##./...#/#.##/###.\n###/##./... => ...#/###./####/#.##\n.../#.#/... => #.#./#.../#.#./..#.\n#../#.#/... => ###./##../..#./.#..\n.#./#.#/... => #.../..##/#..#/#.#.\n##./#.#/... => #.#./.##./#..#/##.#\n#.#/#.#/... => #.##/.#.#/#..#/.#.#\n###/#.#/... => #.../##.#/###./....\n.../###/... => ..##/...#/##.#/###.\n#../###/... => .#.#/...#/#.##/.#..\n.#./###/... => ####/#.../..#./.#.#\n##./###/... => ..../####/#.##/#..#\n#.#/###/... => ####/..#./####/.#.#\n###/###/... => ..##/..../...#/.#..\n..#/.../#.. => .###/..##/.#.#/.##.\n#.#/.../#.. => #.##/#..#/.#.#/##.#\n.##/.../#.. => #.##/####/.#.#/..#.\n###/.../#.. => ##../##.#/..../##..\n.##/#../#.. => ...#/####/..##/.##.\n###/#../#.. => ..#./...#/#.../##.#\n..#/.#./#.. => #..#/##.#/..##/#..#\n#.#/.#./#.. => ..../.###/#..#/..##\n.##/.#./#.. => ..#./...#/..##/...#\n###/.#./#.. => ...#/..../##.#/....\n.##/##./#.. => .#../..##/...#/.#.#\n###/##./#.. => .###/#.#./####/#.#.\n#../..#/#.. => .###/##.#/##../##..\n.#./..#/#.. => ##../.#../###./##.#\n##./..#/#.. => #..#/####/####/..##\n#.#/..#/#.. => ..##/..../###./..##\n.##/..#/#.. => ..##/.#.#/.#../.#..\n###/..#/#.. => ...#/.###/.###/.#.#\n#../#.#/#.. => ##../##../##.#/.##.\n.#./#.#/#.. => ...#/.##./.#.#/#...\n##./#.#/#.. => .##./.#../.#../#...\n..#/#.#/#.. => ..##/##.#/####/###.\n#.#/#.#/#.. => ..../.###/#.../#..#\n.##/#.#/#.. => ..#./#.#./.#../...#\n###/#.#/#.. => ##.#/#.../##.#/.##.\n#../.##/#.. => ..../#.../..#./####\n.#./.##/#.. => #..#/.#../#.#./..##\n##./.##/#.. => .###/..##/###./....\n#.#/.##/#.. => .###/.##./.###/#.##\n.##/.##/#.. => #.##/###./.##./...#\n###/.##/#.. => ...#/#.##/.##./#.#.\n#../###/#.. => #..#/.###/.###/#.#.\n.#./###/#.. => ..#./#.#./..../...#\n##./###/#.. => ..##/##../#..#/....\n..#/###/#.. => ..##/.#../.#../###.\n#.#/###/#.. => ..#./.###/..../...#\n.##/###/#.. => .##./###./#.../#.##\n###/###/#.. => ##.#/..../.##./##.#\n.#./#.#/.#. => .##./.#.#/####/....\n##./#.#/.#. => ##.#/#.##/####/.#..\n#.#/#.#/.#. => ####/.##./##.#/...#\n###/#.#/.#. => #..#/#.##/.##./###.\n.#./###/.#. => .#../..../.##./##.#\n##./###/.#. => ##.#/.#../#.../.###\n#.#/###/.#. => ###./###./.#../###.\n###/###/.#. => #..#/#.../#..#/.#.#\n#.#/..#/##. => #..#/#.../##../###.\n###/..#/##. => #.../.#../.###/#...\n.##/#.#/##. => .#.#/.##./.#../##.#\n###/#.#/##. => #.../..../##../.###\n#.#/.##/##. => .#.#/##../.###/#.#.\n###/.##/##. => ###./..#./##.#/.###\n.##/###/##. => ..#./.#.#/##.#/#.#.\n###/###/##. => ##../.#.#/#..#/.#.#\n#.#/.../#.# => ##../###./..#./##.#\n###/.../#.# => .#../##../..#./##.#\n###/#../#.# => ###./#..#/####/....\n#.#/.#./#.# => .###/..../.###/##.#\n###/.#./#.# => ###./.###/..##/.#.#\n###/##./#.# => ..#./..##/#..#/#.##\n#.#/#.#/#.# => .#.#/.#../.#.#/#.##\n###/#.#/#.# => .###/#.../##../.###\n#.#/###/#.# => .#../...#/..../...#\n###/###/#.# => #..#/##.#/..#./#...\n###/#.#/### => .###/.#.#/..#./####\n###/###/### => ##.#/..##/.#../..##")

(defn -main
  [& args]
  (println (time (puzzle1 puzzle-input)))
  (println (time (puzzle2 puzzle-input))))
