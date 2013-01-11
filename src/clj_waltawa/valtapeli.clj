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

(defn do-move-def [board]
  (let [moves (legit-moves board)]
    (key (apply max-key val (zipmap moves (map valf moves))))))

(defn do-move-off [board]
  (let [moves (legit-moves board)]
    (key (apply max-key val (zipmap moves (map valf moves))))))

(defn do-move [side board]
  (let [move (if (= 1 side) (do-move-off board) (do-move-def board))]
    (println (inc (first move)) (inc (second move)))
    move))


