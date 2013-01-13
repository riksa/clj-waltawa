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

(deftest test-n
  (is (nil? (n '(110 0)))) ; illegal point -> nil
  (is (nil? (n '(0 0)))) ; out of bounds -> nil
  (is (= '(0 0) (n '(0 1)))))

(deftest test-e
  (is (nil? (e '(110 0)))) ; illegal point -> nil
  (is (nil? (e '(15 15)))) ; out of bounds -> nil
  (is (= '(1 0) (e '(0 0))))) ; east of '(0 0) = '(1 0)

(deftest test-w
  (is (nil? (w '(110 0)))) ; illegal point -> nil
  (is (nil? (w '(0 0)))) ; out of bounds -> nil
  (is (= '(0 0) (w '(1 0))))) ; west of '(1 0) = '(0 0)

(deftest test-s
  (is (nil? (s '(110 0)))) ; illegal point -> nil
  (is (nil? (s '(15 15)))) ; out of bounds -> nil
  (is (= '(0 1) (s '(0 0)))))

(deftest test-nw
  (let [p '(1 1)]
    (is (= (nw p) (n (w p)))))) ; nw of '(1 1) is north of west of ('1 1)

(deftest test-ne
  (let [p '(1 1)]
    (is (= (ne p) (n (e p))))))

(deftest test-se
  (let [p '(1 1)]
    (is (= (se p) (s (e p))))))

(deftest test-sw
  (let [p '(1 1)]
    (is (= (sw p) (s (w p))))))

(deftest test-neighbours
  (is (nil? (neighbours '(-1 1)))) ; no neighbours for invalid point
  (is (= (set (neighbours '(0 0))) (set '((0 1) (1 0))))) ; corner point has 2 neighbours
  (is (= (set (neighbours '(1 1))) (set '((0 1) (1 0) (1 2) (2 1)))))) ; more central point has 4 neighbours

(deftest test-surroundings
  (is (nil? (surroundings '(-1 1)))) ; no surroundings for invalid point
  (is (= (set (surroundings '(0 0))) (set '((0 1) (1 0) (1 1))))) ; corner point has 3
  (is (= (set (surroundings '(1 1))) (set '((0 0) (0 1) (0 2) (1 0) (1 2) (2 0) (2 1) (2 2)))))) ; more central point has 8 surroundings
