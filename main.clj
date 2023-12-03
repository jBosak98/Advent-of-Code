(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn lines [filename]
  (with-open [rdr (io/reader filename)]
    (doall (line-seq rdr))))

(defn constraint [color number]
  (cond
    (= "blue" color) (<= number 14)
    (= "green" color) (<= number 13)
    (= "red" color) (<= number 12)))

(defn get-game [line] (drop 1 (str/split (apply str line) #":")))
(defn sum-colors [g]
  (->> (str/split (apply str g) #";|,")
       (map (fn [line]
              (let [[num color] (clojure.string/split (str/trim line) #" ")]
                {:color color :num  (Integer. num)})))
       (group-by :color)
       (reduce (fn [acc [color nums]]
                 (assoc acc color (apply + (map :num nums)))) {})
       (map (fn [c]
              (let [[color number] c] (constraint color number))))))


(defn is-valid-game [colors-summed]
  (reduce (fn [acc is-enough]
            (and  (= true acc) (= true is-enough))) colors-summed))

(def games-validated
  (map (fn [line]
         (def games (apply str (get-game line)))
         (def mini-games (str/split games #";"))
         (def bools (map (fn [game] (sum-colors game)) mini-games))
         (def result (is-valid-game (flatten bools)))
         result)
       (lines "data.txt")))

(defn sum-truthy-positions [bools] (reduce +
                                           (map-indexed (fn [index value] (if (= true value) (+ 1 index) 0)) bools)))

(println (sum-truthy-positions games-validated))
