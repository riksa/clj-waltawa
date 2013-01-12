;  (load-file "valtapeli.clj")
(ns clj-waltawa.valtapeli)

(def size 16)
(defn valid-coord [x]
  (and (integer? x) (< x size) (>= x 0)))

(defn valid-point
  ([] false)
  ([p] (valid-point (first p) (second p)))
  ([x y] (and (valid-coord x) (valid-coord y))))

(use '[clojure.string :only (join split)])
(defn log [& more] (apply println more))
(defn legit-moves [board]
  (for [x (range size) y (range size) :when (not (contains? board [x y]))] [x y]))

;(ns point)

(defn magic [x off]
  (when (valid-point x)
    (let [p (map + off x)]
      (when (valid-point p) p))))

(defn ne [x]
  (magic x [1 -1]))

(defn nw [x]
  (magic x [-1 -1]))

(defn se [x]
  (magic x [1 1]))

(defn sw [x]
  (magic x [-1 1]))

(defn n [x]
  (magic x [0 -1]))

(defn e [x]
  (magic x [1 0]))

(defn w [x]
  (magic x [-1 0]))

(defn s [x]
  (magic x [0 1]))

(defn neighbours [x]
  (when (valid-point x)
    (remove nil? [(n x) (e x) (w x) (s x)])))

(defn surroundings [x]
  (when (valid-point x)
    (remove nil? [(ne x) (nw x) (se x) (sw x) (n x) (e x) (w x) (s x)])))

;(ns player)

(defn angf [x] (* 2 (* Math/PI (+ 0.75 (/ x size)))))
(defn valf [x] (* (+ 1 (Math/sin (angf (first x)))) (+ 1 (Math/sin (angf (second x))))))

(defn read-move [] (map dec (map read-string (clojure.string/split (read-line) #"\s+"))))

(defn score-moves-def [board]
  (let [moves (legit-moves board)]
    (zipmap moves (map valf moves))))

(defn score-moves-off [board]
  (let [moves (legit-moves board)]
    (zipmap moves (map valf moves))))

;(ns clj-waltawa.strategy.simple)
(defn score-moves [board]
  (let [moves (legit-moves board)]
    (zipmap moves (map valf moves))))

(defn do-move-def [board]
  (key (apply max-key val (score-moves board))))

(defn do-move-off [board]
  (key (apply max-key val (score-moves board))))

(defn do-move [side board]
  (let [move (if (= 1 side) (do-move-off board) (do-move-def board))]
    (println (inc (first move)) (inc (second move)))
    move))

;Flood-fill (node, target-color, replacement-color):
;1. Set Q to the empty queue.
;2. Add node to the end of Q.
;4. While Q is not empty:
;5.     Set n equal to the last element of Q.
;7.     Remove last element from Q.
;8.     If the color of n is equal to target-color:
;9.         Set the color of n to replacement-color.
;10.        Add west node to end of Q.
;11.        Add east node to end of Q.
;12.        Add north node to end of Q.
;13.        Add south node to end of Q.
;14. Return.
(defn flood-fill [board point]
  (let [occupied (select-keys board (for [[k v] board :when (= v 1)] k))]
    ;    (when (board point)
    (loop [ret {} q (conj [] point) occupied occupied]
;      (println 'occupied occupied)
;      (println 'q q)
      (if (empty? q)
        (into {} (for [[k v] ret] [k (count ret)]))
        (let [n (last q) q (butlast q)]
;          (println 'n n)
          (if (= 1 (occupied n))
            (recur (assoc ret n 'x) (concat q (neighbours n)) (assoc occupied n 'x))
            (recur ret q occupied)))))))

(defn tile-values [board]
  (loop [board board scores {}]
    (let [point (first (for [[k v] board :when (= v 1)] k))]
      (if (nil? point)
        scores
        (let [area (flood-fill board point)]
          (recur (apply dissoc board (keys area)) (into scores area )))))))




