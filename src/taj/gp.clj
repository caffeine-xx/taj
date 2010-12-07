(ns taj.gp)

(defn- elem-prod [x1 x2]
  (map * x1 x2))
(defn- sum [xs]
  (reduce + xs))

(defn dot-kernel 
  ([x1 x2] (sum (elem-prod x1 x2)))
  ([xs]
   (vec (for [x1 xs] 
          (vec (for [x2 xs] 
                 (dot-kernel x1 x2)))))))

