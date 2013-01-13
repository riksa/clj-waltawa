; for running without leiningen
(load-file "valtapeli.clj")

;read side from stdin
(def side (read-string (read-line)))
;print the name of the AI to stdout
(println "waltawac")

;loop starting with round=0, turn=1 (attacker), board=empty
(loop [round 0 turn 1 board {}]
  ; .. while current round < 16*16
  (when (< turn (* clj-waltawa.valtapeli/size clj-waltawa.valtapeli/size))
    ; .. if it is our turn, choose the move based on board. if it's opponents turn, read his move
    (let [move (if (= turn side) (clj-waltawa.valtapeli/do-move side board) (clj-waltawa.valtapeli/read-move))]
      ; ..increase round count, flip turn, insert move to board
      (recur (inc round) (if (= turn 1) 2 1) (into board {move turn})))))


