(ns clj-waltawa.test.valtapeli
  (:use [clj-waltawa.valtapeli])
  (:use [clojure.test]))

(deftest test-valid-coord
  (is (valid-coord 0))
  (is (valid-coord 15))
  (not (valid-coord -1))
  (not (valid-coord 16))
  (not (valid-coord nil))
  (not (valid-coord))
  (not (valid-coord 'x))
  (not (valid-coord 5 5)))

(deftest test-valid-point
  (is (valid-point '(0 0)))
  (is (valid-point '(15 15)))
  (not (valid-point '(-1 15)))
  (not (valid-point '(15 16)))
  (not (valid-point))
  (not (valid-point '(0 'x)))
  (not (valid-point '(0 nil)))
  (not (valid-point '(5 5) '(5 5))))

(deftest test-legit-moves
  (is (= 256 (count (legit-moves {}))))
  (is (contains? (legit-moves {}) '(0 0)))
  (is (= 255 (count (legit-moves {'(0 0) 1}))))
  (not (contains? (legit-moves {'(0 0) 1}) '(0 0) )))
