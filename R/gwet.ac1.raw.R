#' @importFrom stats na.omit
gwet.ac1.raw <- function(ratings, weights = "unweighted", categ.labels = NULL, 
                         conflev = 0.95, N = Inf) {
  
  ratings.mat <- as.matrix(ratings)
  
  # Ensure character matrices are trimmed and uppercased
  if (is.character(ratings.mat)) {
    ratings.mat <- trimws(toupper(ratings.mat))
    ratings.mat[ratings.mat == ""] <- NA_character_
  }
  
  n <- nrow(ratings.mat)
  r <- ncol(ratings.mat)
  
  # Identify categories
  if (is.null(categ.labels)) {
    categ <- sort(unique(na.omit(as.vector(ratings.mat))))
  } else {
    categ <- toupper(categ.labels)
  }
  q <- length(categ)
  
  # Build weights matrix
  if (is.character(weights)) {
    w.name <- weights
    if (weights == "quadratic") {
      weights.mat <- quadratic.weights(categ)
    } else if (weights == "linear") {
      weights.mat <- linear.weights(categ)
    } else {
      weights.mat <- identity.weights(categ)
    }
  } else {
    w.name <- "Custom Weights"
    weights.mat <- as.matrix(weights)
  }
  
  # Build Agreement Matrix (Optimized rowSums approach)
  agree.mat <- matrix(0, nrow = n, ncol = q)
  for (k in seq_len(q)) {
    categ.is.k <- (ratings.mat == categ[k])
    categ.is.k[is.na(categ.is.k)] <- FALSE
    agree.mat[, k] <- rowSums(categ.is.k)
  }
  
  # Calculate Agreement Metrics
  agree.mat.w <- t(weights.mat %*% t(agree.mat))
  ri.vec <- rowSums(agree.mat)
  sum.q <- rowSums(agree.mat * (agree.mat.w - 1))
  
  n2more <- sum(ri.vec >= 2)
  if (n2more == 0) return(list(est = data.frame(NA, NA, NA, NA), weights = weights.mat, categories = categ)) # Safety catch
  
  # Percent Agreement (Pa)
  pa <- sum(sum.q[ri.vec >= 2] / (ri.vec[ri.vec >= 2] * (ri.vec[ri.vec >= 2] - 1))) / n2more
  
  # Marginal Probabilities (pi.vec) - Optimized using colSums
  pi.vec <- colSums(agree.mat / ri.vec) / n
  
  # Percent Expected (Pe) - Gwet's AC1/AC2 specific logic
  if (q >= 2) {
    pe <- sum(weights.mat) * sum(pi.vec * (1 - pi.vec)) / (q * (q - 1))
  } else {
    pe <- (1 - 1e-15)
  }
  
  # Gwet's AC1/AC2 Coefficient
  gwet.ac1 <- (pa - pe) / (1 - pe)
  gwet.ac1.est <- round(gwet.ac1, 5)
  
  # ---------------------------------------------------------------------
  # THE OPTIMIZATION: 
  # All SE, p-value, t-test, and confidence interval math has been deleted.
  # We return dummy NAs to maintain exact compatibility with SA algorithm.
  # ---------------------------------------------------------------------
  
  coeff.name <- ifelse(sum(weights.mat) == q, "AC1", "AC2")
  
  df.out <- data.frame(
    coeff.name = coeff.name, 
    pa = pa, 
    pe = pe, 
    coeff.val = gwet.ac1.est, 
    coeff.se = NA, 
    conf.int = NA, 
    p.value = NA, 
    w.name = w.name,
    stringsAsFactors = FALSE
  )
  
  return(list(est = df.out, weights = weights.mat, categories = categ))
}