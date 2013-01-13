;  (load-file "valtapeli.clj")
;   (require 'clj-waltawa.valtapeli :reload-all)

(ns clj-waltawa.valtapeli)

(def size 16)
(defn valid-coord?
  "tests that a coordinate is valid for the defined board size [0..15]"
  ([] false)
  ([x] (and (integer? x) (< x size) (>= x 0)))
  ([x & rest] false))

(defn valid-point?
  "tests that a point is a valid point in a map (x and y coordinates are valid)"
  ([] false)
  ([p] (valid-point? (first p) (second p)))
  ([x y] (and (valid-coord? x) (valid-coord? y))))

(use '[clojure.string :only (join split)])
(defn log [& more] (apply println more))

(defn legit-moves [board]
  "returns a sequence of legit moves to board"
  (for [x (range size) y (range size) :when (not (contains? board [x y]))] [x y]))

;(ns point)

(defn magic [x off]
  "helper function for n,e,w,s for creating and validation a neighbour"
  (when (valid-point? x)
    (let [p (map + off x)]
      (when (valid-point? p) p))))

(defn ne [x]
  "return north east neighbour for x, or nil if neither point is invalid"
  (magic x [1 -1]))

(defn nw [x]
  "return north west neighbour for x, or nil if neither point is invalid"
  (magic x [-1 -1]))

(defn se [x]
  "return south east neighbour for x, or nil if neither point is invalid"
  (magic x [1 1]))

(defn sw [x]
  "return south west neighbour for x, or nil if neither point is invalid"
  (magic x [-1 1]))

(defn n [x]
  "return north neighbour for x, or nil if neither point is invalid"
  (magic x [0 -1]))

(defn e [x]
  "return east neighbour for x, or nil if neither point is invalid"
  (magic x [1 0]))

(defn w [x]
  "return west neighbour for x, or nil if neither point is invalid"
  (magic x [-1 0]))

(defn s [x]
  "return south neighbour for x, or nil if neither point is invalid"
  (magic x [0 1]))

(defn neighbours [x]
  "return sequence of valid north, east, south and west
   neighbours for x, or nil if x is invalid"
  (when (valid-point? x)
    (remove nil? [(n x) (e x) (w x) (s x)])))

(defn surroundings [x]
  "return sequence of valid N, NE, NW, E, W, SE, S, SW
  surroundings for x, or nil if x is invalid"
  (when (valid-point? x)
    (remove nil? [(ne x) (nw x) (se x) (sw x) (n x) (e x) (w x) (s x)])))

;(ns player)

(defn angf [x]
  "helper angular function for valf. translate [0..1] to an angle whose sin
  results in 0 at 0 and 1 and peaks once between them."
  (* 2 (* Math/PI (+ 0.75 (/ x size)))))

(defn valf [coordinate]
  "calculate 'initial value' for coordinate x. initial value is a sin wave function
  that has a single maximum at the center of the map and has a minimums at the corners"
  (* (+ 1 (Math/sin (angf (first coordinate)))) (+ 1 (Math/sin (angf (second coordinate))))))

(defn read-move []
  "read a move from STDIN and decrease coordinates by one to translate the external 1-based
  coordinate system to internally used 0-based system"
  (map dec (map read-string (clojure.string/split (read-line) #"\s+"))))

;(defn enclose-defend-function [x]
; (let [initial ])
;  (apply max-key val (select-keys values (clj-waltawa.valtapeli/neighbours [1 0])))
;
;  {x (valf x)})

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
  "flood fills the instances of '1 in the board starting from the point.
  returns a map of coordinates that were floodfilled"
  (let [occupied (select-keys board (for [[k v] board :when (= v 1)] k))]
    (loop [ret {} q (conj [] point) occupied occupied]
      (if (empty? q)
        (into {} (for [[k v] ret] [k (count ret)]))
        (let [n (last q) q (butlast q)]
          (if (= 1 (occupied n))
            (recur (assoc ret n 'x) (concat q (neighbours n)) (assoc occupied n 'x))
            (recur ret q occupied)))))))

(defn tile-values [board]
  "finds out all the regions that are occupied by '1 and groups the connected tiles together.
  returns a sequence of sequences of connected areas"
  (loop [board board scores {}]
    (let [point (first (for [[k v] board :when (= v 1)] k))] w
      (if (nil? point)
        scores
        (let [area (flood-fill board point)]
          (recur (apply dissoc board (keys area)) (conj scores area)))))))

(defn score-moves-def [board]
  "uses some black magic to rate all the free tiles for their value as a defensive move.
  factors in:
    - the size of the biggest occupied neighbouring area (squared) (increases value of the tile)
    - initial value (steers the choices towards middle of the map to steer the invader against walls)
    - the amount of neighbours occupied by defender (creates a zig-zag pattern)"
  (into {}
    (let [moves (legit-moves board) values (tile-values board)]
      (for [move moves]
        (let [initial (valf move)
              max-neighbour (val (apply max-key val (into {0 -1} (select-keys values (neighbours move)))))
              friendlies (count (filter #(= 2 %) (vals (select-keys board (clj-waltawa.valtapeli/neighbours move)))))]
          [move (- (+ initial (* max-neighbour max-neighbour)) friendlies)])))))

(defn score-moves-off [board]
  "uses some black magic to rate all the free tiles for their value as an offensive move.
  factors in:
    - the size of the biggest occupied neighbouring area (squared) (increases value of the tile)
    - initial value (steers the choices towards middle of the map to steer the invader against walls)
    - the amount of free tiles as neighbours"
  (into {}
    (let [moves (legit-moves board) values (tile-values board)]
      (for [move moves]
        (let [initial (valf move)
              max-neighbour (val (apply max-key val (into {0 0} (select-keys values (neighbours move)))))
              empties (count (for [m (clj-waltawa.valtapeli/neighbours move) :when (nil? (board m))] m))]
          [move (+ initial (* empties (* max-neighbour max-neighbour)))])))))

;(ns clj-waltawa.strategy.simple)
;(defn score-moves [board]
;  (let [moves (legit-moves board)]
;    (zipmap moves (map valf moves))))

(defn do-move-def [board]
  "rates the tiles and chooses the one with the biggest value"
  (key (apply max-key val (score-moves-def board))))

(defn do-move-off [board]
  "rates the tiles and chooses the one with the biggest value"
  (key (apply max-key val (score-moves-off board))))

(defn do-move [side board]
  "picks a move for the ai and prints it out. if the ai side is 1, choose offensive move,
  otherwise choose defensive move"
  (let [move (if (= 1 side) (do-move-off board) (do-move-def board))]
    (println (inc (first move)) (inc (second move)))
    move))





