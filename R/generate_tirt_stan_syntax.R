#' Generate Stan Syntax and Data for TIRT Models
#'
#' Automatically reduces logical dependencies for full rank data (Heister et al. 2025),
#' handles missing/MOLE data natively, and generates Stan syntax capable of
#' within-chain parallelization (reduce_sum).
#'
#' @param pairwise_data Dataframe of binary outcomes.
#' @param n_blocks Integer. Number of blocks.
#' @param block_size Integer. Number of items per block.
#' @param n_traits Integer. Number of traits.
#' @param key_matrix A data.frame with two columns, one indicating item trait,
#'        another indicating item sign.
#' @param trait_col Character string. The name of the column in \code{key_matrix} indicating the trait
#'        measured by the item.
#' @param key_col Character string. The name of the column in \code{key_matrix} indicating the keying
#'        direction of the item (e.g., positive/negative).
#' @param apply_heister Logical. Apply logical dependency reduction? Defaults to TRUE.
#'
#' @return A list containing `syntax` (Stan code) and `data` (List for rstan).
#' @export
generate_tirt_stan_syntax <- function(pairwise_data,
                                      n_blocks,
                                      block_size,
                                      n_traits,
                                      key_matrix,
                                      trait_col,
                                      key_col,
                                      apply_heister = TRUE) {

  # ==========================================
  # 1. PREPARE THE DATA (With Heister Reduction)
  # ==========================================
  N <- nrow(pairwise_data)
  K <- n_blocks * block_size
  P <- n_blocks * (block_size * (block_size - 1) / 2)

  # Pair mappings
  item_i <- c(); item_k <- c()
  for (b in 1:n_blocks) {
    for (i in 1:(block_size - 1)) {
      itm_i <- (b - 1) * block_size + i
      for (k in (i + 1):block_size) {
        itm_k <- (b - 1) * block_size + k
        item_i <- c(item_i, itm_i)
        item_k <- c(item_k, itm_k)
      }
    }
  }

  # Convert wide pairwise data to long format (ignoring NAs for MOLE data)
  long_person <- c(); long_pair <- c(); long_y <- c()
  for (n in 1:N) {
    for (b in 1:n_blocks) {

      start_pair <- (b - 1) * (block_size * (block_size - 1) / 2) + 1
      end_pair <- b * (block_size * (block_size - 1) / 2)
      block_pairs <- start_pair:end_pair

      y_block <- as.numeric(pairwise_data[n, block_pairs])

      # Condition A: Block has NAs (MOLE data) OR block_size is 2.
      # Action: Keep all non-NA pairs.
      if (any(is.na(y_block)) || !apply_heister || block_size <= 2) {
        valid_idx <- which(!is.na(y_block))
        if (length(valid_idx) > 0) {
          long_person <- c(long_person, rep(n, length(valid_idx)))
          long_pair   <- c(long_pair, block_pairs[valid_idx])
          long_y      <- c(long_y, y_block[valid_idx])
        }

        # Condition B: Full Rank Data (No NAs).
        # Action: Apply Heister et al. (2025) logical reduction
      } else {
        # Calculate 'wins' to deduce the rank of each item
        wins <- rep(0, block_size)
        idx <- 1
        for (i in 1:(block_size - 1)) {
          for (k in (i + 1):block_size) {
            if (y_block[idx] == 1) wins[i] <- wins[i] + 1
            else wins[k] <- wins[k] + 1
            idx <- idx + 1
          }
        }

        # Check if data is perfectly transitive (no tied ranks)
        if (length(unique(wins)) == block_size) {
          # Keep ONLY adjacent pairs (difference in wins == 1)
          idx <- 1
          for (i in 1:(block_size - 1)) {
            for (k in (i + 1):block_size) {
              if (abs(wins[i] - wins[k]) == 1) {
                long_person <- c(long_person, n)
                long_pair   <- c(long_pair, block_pairs[idx])
                long_y      <- c(long_y, y_block[idx])
              }
              idx <- idx + 1
            }
          }
        } else {
          # Fallback: If respondent gave intransitive/tied data, keep all pairs
          long_person <- c(long_person, rep(n, length(y_block)))
          long_pair   <- c(long_pair, block_pairs)
          long_y      <- c(long_y, y_block)
        }
      }
    }
  }

  # Uniqueness (psi^2) Identification Rules
  is_fixed_psi <- rep(0, K); fixed_psi_val <- rep(0, K)
  if (block_size == 2) {
    is_fixed_psi <- rep(1, K); fixed_psi_val <- rep(0.5, K)
  } else {
    for (b in 1:n_blocks) {
      first_item <- (b - 1) * block_size + 1
      is_fixed_psi[first_item] <- 1
      fixed_psi_val[first_item] <- 1.0
    }
  }
  free_psi_idx <- which(is_fixed_psi == 0)
  K_free <- length(free_psi_idx)

  stan_data <- list(
    N = N, K = K, P = P, D = n_traits, N_obs = length(long_y),
    person = long_person, pair = long_pair, y = long_y,
    item_i = item_i, item_k = item_k,
    trait = key_matrix[[trait_col]], item_sign = key_matrix[[key_col]],
    K_free = K_free, is_fixed_psi = is_fixed_psi,
    fixed_psi_val = fixed_psi_val, free_psi_idx = free_psi_idx
  )

  # ==========================================
  # 2. BUILD THE STAN SYNTAX
  # ==========================================
  stan_code <- "
  // Bayesian TIRT Model
  // Logical Dependency Reduction based on Heister, Doebler, & Frick (2025)
  // Within-chain parallelization via reduce_sum
  functions {
    real partial_sum(array[] int slice_obs, int start, int end,
                     array[] int person, array[] int pair, array[] int y,
                     array[] int item_i, array[] int item_k, array[] int trait,
                     vector alpha, vector lambda, vector denom, matrix theta) {

      // Vectorizing the chunk speeds up C++ computation significantly
      vector[end - start + 1] mu_y;

      for (obs in start:end) {
        int p = pair[obs];
        int nn = person[obs];
        int i = item_i[p];
        int k = item_k[p];

        mu_y[obs - start + 1] = (alpha[p] + lambda[i] * theta[nn, trait[i]] - lambda[k] * theta[nn, trait[k]]) / denom[p];
      }

      // bernoulli_probit is mathematically stabilized to prevent log(0) errors!
      return bernoulli_probit_lpmf(y[start:end] | mu_y);
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
    // Create an index array required by reduce_sum
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

    // Within-chain parallelization (Multithreading)
    int grainsize = 100; // Chunk size for cores to process
    target += reduce_sum(partial_sum, obs_idx_array, grainsize,
                         person, pair, y, item_i, item_k, trait,
                         alpha, lambda, denom, theta);
  }
  generated quantities {
    corr_matrix[D] Omega = multiply_lower_tri_self_transpose(L_Omega);
  }
  "

  # ==========================================
  # 3. RETURN AS A LIST
  # ==========================================
  return(list(
    syntax = stan_code,
    data = stan_data
  ))
}
