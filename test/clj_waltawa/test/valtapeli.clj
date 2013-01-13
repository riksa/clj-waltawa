(ns clj-waltawa.test.valtapeli
  (:use [clj-waltawa.valtapeli])
  (:use [clojure.test]))

(deftest test-valid-coord
  (is (valid-coord 0)) ; valid coord [0..15]
  (is (valid-coord 15))
  (not (valid-coord -1)) ; invalid coord
  (not (valid-coord 16))
  (not (valid-coord nil)) ; wront type
  (not (valid-coord 'x))
  (not (valid-coord)) ; wrong arity
  (not (valid-coord 5 5)))

(deftest test-valid-point
  (is (valid-point '(0 0))) ; valid point as a seq
  (is (valid-point '(15 15)))
  (is (valid-point [0 0])) ; valid point as a list
  (not (valid-point '(-1 15))) ; invalid point
  (not (valid-point '(15 16)))
  (not (valid-point)) ; wrong arity
  (not (valid-point '(5 5) '(5 5)))
  (not (valid-point '(0 'x))) ; wrong types
  (not (valid-point '(0 nil))))

(deftest test-legit-moves
  (is (= 256 (count (legit-moves {})))) ; empty board should have 256 legit moves ..
  (not (nil? (some #{'(10 1)} (legit-moves {})))) ; .. one of them should be '(10 1)
  (is (= 255 (count (legit-moves {'(10 1) 1})))) ; board with move made to '(10 1) must have 255 legit moves ..
  (is (nil? (some #{'(10 1)} (legit-moves {'(10 1) 1}))))) ; .. '(10 1) is not one of them
