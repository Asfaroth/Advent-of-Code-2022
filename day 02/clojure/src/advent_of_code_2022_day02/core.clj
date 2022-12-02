(ns advent-of-code-2022-day02.core)
(require '[clojure.string :as str])

(def playedRock "A")
(def playedPaper "B")
(def playedScissors "C")

(def willPlayRockOrLoose "X")
(def willPlayPaperOrDraw "Y")
(def willPlayScissorsOrWin "Z")

(def rock 0)
(def paper 1)
(def scissor 2)

(def rockPoint 1)
(def paperPoints 2)
(def scissorPoints 3)

(def lostPoints 0)
(def drawPoints 3)
(def winPoints 6)

(defn calcScore
  "Calculates the score for given rounds."
  [rounds]
  (map #(+ (if (= (:willPlay %) rock) rockPoint
               (if (= (:willPlay %) paper) paperPoints
                   (if (= (:willPlay %) scissor) scissorPoints lostPoints)))
           (if (= (:played %) (:willPlay %)) drawPoints
               (if (or (and (= (:played %) rock) (= (:willPlay %) paper))
                       (and (= (:played %) paper) (= (:willPlay %) scissor))
                       (and (= (:played %) scissor) (= (:willPlay %) rock)))
                 winPoints 0))) rounds))

(defn -main
  "Main function."
  []
  (let [parsedLines (map #(let [parsed (str/split % #" ")]
                            {:played (str/replace (get parsed 0) #"\p{C}" "") :willPlay (str/replace (get parsed 1) #"\p{C}" "")})
                         (str/split (slurp "../input.txt") #"\n"))
        firstRounds (map (fn [x] {:played (if (= (:played x) playedRock) rock
                                              (if (= (:played x) playedPaper) paper scissor))
                                  :willPlay (if (= (:willPlay x) willPlayRockOrLoose) rock
                                                (if (= (:willPlay x) willPlayPaperOrDraw) paper scissor))}) parsedLines)
        secondRounds (map (fn [x] {:played (if (= (:played x) playedRock) rock
                                               (if (= (:played x) playedPaper) paper scissor))
                                   :willPlay (if (= (:willPlay x) willPlayRockOrLoose)
                                               (if (= (:played x) playedRock) scissor
                                                   (if (= (:played x) playedPaper) rock paper))
                                               (if (= (:willPlay x) willPlayPaperOrDraw)
                                                 (if (= (:played x) playedRock) rock
                                                     (if (= (:played x) playedPaper) paper scissor))
                                                 (if (= (:played x) playedRock) paper
                                                     (if (= (:played x) playedPaper) scissor rock))))}) parsedLines)]
    (println "Total score 1st: " (reduce + (calcScore firstRounds)))
    (println "Total score 2nd: " (reduce + (calcScore secondRounds)))))
