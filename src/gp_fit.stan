// Fit the hyperparameters of a Gaussian process with an 
// exponentiated quadratic kernel

data {
  int<lower=1> N;
  row_vector[2] x[N]; // coords
  vector[N] y; // land_price, etc.
}
transformed data {
  vector[N] mu = rep_vector(0, N);
}
parameters {
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
}
model {
  matrix[N, N] L_K;
  // real sq_sigma = square(sigma);
  matrix[N, N] K = cov_exp_quad(x, alpha, rho) + diag_matrix(rep_vector(square(sigma), N));

  // diagonal elements
  // for (n in 1:N)
  //   K[n, n] = K[n, n] + sq_sigma;
  
  L_K = cholesky_decompose(K);
  
  rho ~ inv_gamma(5, 5);
  alpha ~ normal(0, 1);
  sigma ~ normal(0, 1);

  y ~ multi_normal_cholesky(mu, L_K);
}