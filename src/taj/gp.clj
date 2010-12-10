(ns taj.gp
  ^{:author "Daniel Cook"
    :doc "Gaussian process regression"}
  (:require [infer [core :as c] 
                   [matrix :as m] 
                   [neighbors :as n]])
  (:use clojure.contrib.math)
  (:import [org.ujmp.core Matrix
        MatrixFactory
        Ops]))

(defn el [m] (double (m/get-at m 0 0)))

(defn ones [& dimensions]
  "Matrix of ones"
  (MatrixFactory/ones (long-array dimensions)))

(defn exp-elem [M]
  (.exp M m/new-matrix))

(defn pow-elem [M p]
  (.power M m/new-matrix (double p)))

(defn sum-squared-dist 
  "Takes
      x = N x 1 point
      y = M x N matrix
      weight = N x N weight matrix
   Return 
      d = 1 x M of sum-squared-distances between
                 x and data"
  ([x data weights]
    (let [[M N] (vec (.getSize data))]
      (reduce m/plus
        (m/each-row
          (pow-elem
            (m/times (m/minus (m/trans data)
                              (m/times x (ones 1 M)))
                     weights)
            2)))))
  ([x data]
    (let [[M N] (vec (.getSize data))]
      (sum-squared-dist x data (m/I M M)))))

(defn dot-kernel [xs1 xs2] 
  "Dot product kernel
   Arguments: 
     xs1 = coll or matrix, size N
     xs2 = coll or matrix, size M
   Always returns a kernel matrix size N x M"
  (m/times (m/matrix xs1) (m/trans (m/matrix xs2))))

(defn gauss-kernel [xs1 xs2]
  "Squared exponential (Gaussian) kernel
   For p in rows(xs1), q in rows(xs2), return
      alpha exp(-1/2 (p-q)' W (p-q))
   Defaults:  alpha = 1, W = I"
  (let [xs1   (m/matrix xs1)
        xs2   (m/matrix xs2)
        cs1   (m/each-column (m/trans xs1))
        dists (apply m/row-concat (map #(sum-squared-dist % xs2) cs1))
        link  (fn [D] (exp-elem (m/times D -0.5)))]
    (link dists)))

(defn gp-vec [kernel data obs sigma]
  "Gaussian process is a function h : x -> y distributed GP(m(x),m(x,x)) where
   Arguments:
      kernel -> kernel function
      data   -> seq of data [x_0 .. x_N]
      obs    -> seq of function outputs [y_0 .. y_N] such that y_i = h(x_i)
   Returns function m: 
      (m [x])       -> E[h(x)]          = k(x,X) C y
      (m [x1] [x2]) -> Cov(h(x1),h(x2)) = k(x1,x2) - k(x1,X) C k(X,x2)"
  (let 
    [ndata      (count data)
     kernel-mat (kernel data data)
     noise-mat  (m/times (m/I ndata ndata) sigma) 
     cov-mat    (m/inv   (m/plus kernel-mat noise-mat))
     obs-column (m/column-matrix obs)]
    (fn
      ([x] 
       (let [x-covs (kernel x data)]
            (m/times x-covs cov-mat obs-column)))
      ([x1 x2]
       (let [x1-covs (kernel x1 data)
             x2-covs (cond (= x1 x2)  x1-covs
                           :otherwise (kernel x2 data))]
            (m/minus 
              (kernel x1 x2) 
              (m/times x1-covs cov-mat (m/trans x2-covs))))))))

(defn gp [kernel data obs sigma]
  (let [f (gp-vec kernel data obs sigma)]
    (comp el
      (fn ([x]     (f [x]))
          ([x1 x2] (f [x1] [x2]))))))
