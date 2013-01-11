(require 'clj-waltawa.valtapeli)

(ns clj-waltawa.core
  (:gen-class ))

(defn -main
  "I don't do a whole lot."
  [& args]
  (def side (read-string (read-line)))
  (println "waltawac")

  (loop [round 0 turn 1 board {}]
    (when (< turn (* clj-waltawa.valtapeli/size clj-waltawa.valtapeli/size))
      (let [move (if (= turn side) (clj-waltawa.valtapeli/do-move side board) (clj-waltawa.valtapeli/read-move))]
        (recur (inc round) (if (= turn 1) 2 1) (into board {move turn}))))))
