;(load-file "valtapeli.clj")

; begin
(def side (read-string (read-line)))
(println "waltawac")

(loop [round 0 turn 1 board {}]
  (when (< turn (* clj-waltawa.valtapeli/size clj-waltawa.valtapeli/size))
    (let [move (if (= turn side) (do-move side board) (read-move))]
      (recur (inc round) (if (= turn 1) 2 1) (into board {move turn})))))



