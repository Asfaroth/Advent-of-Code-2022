(ns advent-of-code-2022-day06.core)
(require '[clojure.string :as str])

(defn findMarker
  "Finds a marker with x unique characters."
  ([markerCount searchList] (findMarker markerCount markerCount searchList))
  ([index markerCount searchList]
   (if (apply distinct? (take markerCount (drop (- index markerCount) searchList))) index (recur (inc index) markerCount searchList))))

(defn -main
  "Main function."
  []
  (let [input (seq (str/replace (slurp "../input.txt") #"\p{C}" ""))]
    (println "Packet Start:  " (findMarker 4 input))
    (println "Message Start: " (findMarker 14 input))))
