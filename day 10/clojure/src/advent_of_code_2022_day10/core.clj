(ns advent-of-code-2022-day10.core)
(require '[clojure.string :as str])

(defn processCommands
  "Processes the given commands and returns a vector with the resulting values in each cycle."
  ([commands] (processCommands commands (vector 1)))
  ([pending collection]
   (if (empty? pending)
     collection
     (if (= (first (first pending)) "noop")
       (recur (drop 1 pending) (conj collection (last collection)))
       (recur (drop 1 pending) (conj collection (last collection) (+ (Integer/parseInt (second (first pending))) (last collection))))))))

(defn renderScreen
  "Renders the screen according to the given cycle values."
  ([cycles] (renderScreen 0 cycles (into [] (repeat 6 (into [] (repeat 40 "."))))))
  ([cycleID pending screen]
   (if (empty? pending) screen
       (let [row (quot cycleID 40)
             col (mod cycleID 40)]
         (if (<= (dec (first pending)) col (inc (first pending)))
           (recur (inc cycleID) (drop 1 pending) (assoc screen row (assoc (nth screen row) col "#")))
           (recur (inc cycleID) (drop 1 pending) screen))))))

(defn -main
  "Main function."
  []
  (let [input (mapv #(str/split % #" ") (str/split-lines (slurp "../input.txt")))
        cycles (processCommands input)
        rendered (renderScreen cycles)]
    (println "Sum of signal strengths: " (reduce-kv #(if (or (= %2 19)
                                                             (= %2 59)
                                                             (= %2 99)
                                                             (= %2 139)
                                                             (= %2 179)
                                                             (= %2 219))
                                                       (+ %1 (* (inc %2) %3))
                                                       %1) 0 cycles))
    (println "Rendered screen:")
    (run! println rendered)))
