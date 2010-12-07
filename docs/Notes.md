= Gaussian Process Regression,

== Basics

- Data $X=[x_0^T \cdots x_N^T]^T, X \in \mathcal{R}^{N \times D}$, where vector $x_i \in \mathcal{R}^{D \times 1}$ is a single data point.
- Kernel function $k(x_i,x_j) \in \mathcal{R}$ is an inner product on $\mathcal{X}$, the space of data points.
- Kernel matrix $K \in \mathcal{R}^{N \times N}$ where element $K_{ij} = k(x_i, x_j)$ is positive semi-definite
- Define the notation $k(x,X) = [k(x,x_0) \cdots k(x,x_N)] = k(X,x)^T$
- Training observations $y = [y_0 \cdots y_N]^T$, are observations of a function $h$, so that $h(x_i) = y_i$, and
  $h \sim \mathcal{GP}(m,k)$
- Then, for some point $x$, define:
  $$ \mu_h (x) = E[h(x)]   = k(x,X) (K + \sigma_{e}^2 I)^{-1} y $$
  $$ \sigma^2_h (x) = Var[h(x)] = k(x,x) - k(x,X)(K + \sigma_{e}^2 I)^{-1} k(X,x) $$

== API

- Gaussian process model takes covariance, data, noise variance and 
  returns a function

(gp k X y sigma?) returns: (fn [x]      -> mu_h(x)
                               [x1, x2] -> cov_h(x1, x2))

- X is just a seq (in the example above, elements would be row vectors).

- Kernel fns are simply

(k x0 x1 ... )   -> scalar

- Lazy seq for kernel matrix. Use (first (k [x & X])) to get k(x,X)

(k [x0 x1 ..])   -> [[(k x0 x0) (k x0 x1) .. ]
                     [(k x1 x0) (k x1 x1) .. ] ..]

- Kernel constructor

(foo-kernel params) -> k
 
