(ns taj.gp
  ^{:author "Daniel Cook"
    :doc "Gaussian process regression"}
  (:require [infer [core :as c] 
                   [matrix :as m] 
                   [neighbors :as n]])
  (:use clojure.contrib.math))

(defn el [m] (double (m/get-at m 0 0)))

(defn dot-kernel [xs1 xs2] 
  "Dot product kernel
   Arguments: 
     xs1 = coll or matrix, size N
     xs2 = coll or matrix, size M
   Always returns a kernel matrix size N x M"
  (m/times (m/matrix xs1) (m/trans (m/matrix xs2))))

(defn gauss-kernel [xs1 xs2]
  (let [xs1 (m/matrix xs1)
        xs2 (m/matrix xs2)]
    (m/matrix 
      (for [x1 (m/each-row xs1)]
        (for [x2 (m/each-row xs2)]
          (let [diff (m/minus x1 x2)
                dist (sqrt (el (m/times diff (m/trans diff))))]
            (n/gaussian dist)))))))

(defn gp [kernel data obs sigma]
  "Gaussian process is a function h : x -> y distributed GP(m(x),m(x,x)) where
   Arguments:
      kernel -> kernel function
      data   -> seq of data [x_0 .. x_N]
      obs    -> seq of function outputs [y_0 .. y_N] such that y_i = h(x_i)
   Returns function m: 
      (m [x]) ->     E[h(x)]          = k(x,X) C y
      (m [x1,x2]) -> Cov(h(x1),h(x2)) = k(x1,x2) - k(x1,X) C k(X,x2)"
  (let 
    [ndata      (count data)
     kernel-mat (kernel data data)
     noise-mat  (m/times (m/I ndata ndata) sigma) 
     cov-mat    (m/inv   (m/plus kernel-mat noise-mat))
     obs-column (m/column-matrix obs)]
    (fn
      ([x] 
       (let [x-covs (kernel [x] data)]
         (el 
           (m/times x-covs cov-mat obs-column))))
      ([x1 x2]
       (let [x1-covs (kernel [x1] data)
             x2-covs (cond (= x1 x2)  x1-covs
                           :otherwise (kernel [x2] data))]
         (el
           (m/minus 
             (kernel [x1] [x2]) 
             (m/times x1-covs cov-mat (m/trans x2-covs)))))))))

