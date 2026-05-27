functions {
  real partial_sum(array[] int slice_obs, int start, int end,
                   array[] int person, array[] int pair, array[] int y,
                   array[] int item_i, array[] int item_k, array[] int trait,
                   vector alpha, vector lambda, vector denom, matrix theta) {
    
    real lp = 0.0;
    
    for (obs in start:end) {
      int p = pair[obs];
      int nn = person[obs];
      int i = item_i[p];
      int k = item_k[p];
      
      // Calculate the mean of the latent response
      real mu_y = (alpha[p] + lambda[i] * theta[nn, trait[i]] - lambda[k] * theta[nn, trait[k]]) / denom[p];
      
      // Phi() is the standard normal CDF in Stan. 
      // This calculates the log-probability of the Bernoulli outcome safely.
      lp += bernoulli_lpmf(y[obs] | Phi(mu_y));
    }
    
    return lp;
  }
}

data {
  int<lower=1> N; int<lower=1> K; int<lower=1> P; int<lower=1> D; int<lower=1> N_obs;
  array[N_obs] int<lower=1, upper=N> person;
  array[N_obs] int<lower=1, upper=P> pair;
  array[N_obs] int<lower=0, upper=1> y;
  array[P] int<lower=1, upper=K> item_i;
  array[P] int<lower=1, upper=K> item_k;
  array[K] int<lower=1, upper=D> trait;
  vector[K] item_sign;
  int<lower=0> K_free;
  array[K] int<lower=0, upper=1> is_fixed_psi;
  vector[K] fixed_psi_val;
  array[K_free] int<lower=1, upper=K> free_psi_idx;
}

transformed data {
  array[N_obs] int obs_idx_array;
  for (i in 1:N_obs) obs_idx_array[i] = i;
}

parameters {
  matrix[D, N] z_theta;
  cholesky_factor_corr[D] L_Omega;
  vector[P] alpha;
  vector<lower=0>[K] lambda_raw;
  vector<lower=0>[K_free] psi2_free;
}

transformed parameters {
  matrix[N, D] theta = (L_Omega * z_theta)';
  vector[K] lambda = lambda_raw .* item_sign;
  vector[K] psi2;
  for (k in 1:K) {
    if (is_fixed_psi[k] == 1) psi2[k] = fixed_psi_val[k];
  }
  for (f in 1:K_free) {
    psi2[free_psi_idx[f]] = psi2_free[f];
  }
  vector[P] denom;
  for (p in 1:P) {
    denom[p] = sqrt(psi2[item_i[p]] + psi2[item_k[p]]);
  }
}

model {
  to_vector(z_theta) ~ std_normal();
  L_Omega ~ lkj_corr_cholesky(2.0);
  alpha ~ normal(0, 3);
  lambda_raw ~ lognormal(0, 0.5);
  psi2_free ~ lognormal(0, 0.5);
  
  int grainsize = 250; 
  target += reduce_sum(partial_sum, obs_idx_array, grainsize,
                       person, pair, y, item_i, item_k, trait,
                       alpha, lambda, denom, theta);
}

generated quantities {
  corr_matrix[D] Omega = multiply_lower_tri_self_transpose(L_Omega);
}


