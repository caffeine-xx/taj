(ns taj.test.gp
  (:use [taj.gp] :reload)
  (:use [clojure.test])
  (:require [infer [matrix :as m]]))

(def x0 [1 2])
(def x1 [2 3])
(def x2 [3 4])

(def X [x0 x1 x2])

(def x0-dot-x1 8)
(def x0-dot-X [[5 8 11]])

(def K [[5 8 11]
        [8 13 18]
        [11 18 25]])

(def tol 0.01)

(deftest test-sum-squared-dist
  (let [xm (m/column-matrix x0)
        XM (m/matrix X)]
  (is (= [[0 2 8]] (vec (m/from-matrix (sum-squared-dist xm XM)))))))

(defn- is-within [err x0 x1]
  (is (< (Math/abs (- x0 x1)) err)))

(deftest test-dot-kernel
  (is (= x0-dot-x1 (dot-kernel [x0] [x1]))))

(deftest test-gauss-kernel
  (is-within tol 0.367 (el (gauss-kernel [x0] [x1]))))


(deftest test-kernel-row
  (is (= (vec (m/from-matrix 
                (dot-kernel [x0] X)))
         x0-dot-X)))

(deftest test-kernel-matrix
  (is (= (vec (m/from-matrix
                (dot-kernel X X)))
         K)))

(deftest test-gp-linear
  (let [f (fn [[x]] (* x 3.0))
        X (vec (map #(into [] [(double %)]) (range 100)))
        y (map f X)
        h (gp dot-kernel X y 1.0)
        pt [200.0]
        fp (f pt)
        hp (h pt)]
    (is-within tol (f pt) (h pt))))

(deftest test-gp-sin
  (let [npts 300
        f (fn [[x]] (Math/sin (* 4 3.14159 (/ x npts))))
        X (vec (map #(into [] [(double %)]) (range npts)))
        y (map f X)
        h (gp gauss-kernel X y 1)
        hp (map h X)
        hc (map #(h % %) X)]
    ;(println (m/trans (m/matrix [y hp hc])))
    ))
