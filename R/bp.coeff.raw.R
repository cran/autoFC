#' @importFrom stats na.omit pt qt

bp.coeff.raw <- function (ratings, weights = "unweighted", categ.labels = NULL,
          conflev = 0.95, N = Inf) {
  ratings.mat <- as.matrix(ratings)
  if (is.character(ratings.mat)) {
    ratings.mat <- trimws(toupper(ratings.mat))
    ratings.mat[ratings.mat == ""] <- NA_character_
  }
  n <- nrow(ratings.mat)
  r <- ncol(ratings.mat)
  if (is.null(categ.labels)) {
    categ.init <- unique(na.omit(as.vector(ratings.mat)))
    categ <- sort(categ.init)
  }
  else categ <- toupper(categ.labels)
  q <- length(categ)
  if (is.character(weights)) {
    w.name <- weights
    if (weights == "quadratic")
      weights.mat <- quadratic.weights(categ)
    else if (weights == "linear")
      weights.mat <- linear.weights(categ)
    else weights.mat <- identity.weights(categ)
  }
  else {
    w.name <- "Custom Weights"
    weights.mat = as.matrix(weights)
  }
  agree.mat <- matrix(0, nrow = n, ncol = q)
  for (k in 1:q) {
    categ.is.k <- (ratings.mat == categ[k])
    categ.is.k[is.na(categ.is.k)] <- FALSE
    agree.mat[, k] <- rowSums(categ.is.k)
  }
  agree.mat.w <- t(weights.mat %*% t(agree.mat))
  agree.mat.w <- t(weights.mat %*% t(agree.mat))
  ri.vec <- rowSums(agree.mat)
  sum.q <- rowSums(agree.mat * (agree.mat.w - 1))

  n2more <- sum(ri.vec >= 2)
  if (n2more == 0) return(list(est = data.frame(NA, NA, NA, NA), weights = weights.mat, categories = categ))
  pa <- sum(sum.q[ri.vec >= 2] / (ri.vec[ri.vec >= 2] * (ri.vec[ri.vec >= 2] - 1))) / n2more

  pe <- sum(weights.mat)/(q^2)
  bp.coeff <- (pa - pe)/(1 - pe)
  bp.coeff.est <- round(bp.coeff, 5)

  df.out <- data.frame(
    coeff.name = "Brennan-Prediger",
    pa = pa,
    pe = pe,
    coeff.val = bp.coeff.est,
    coeff.se = NA,
    conf.int = NA,
    p.value = NA,
    w.name = w.name,
    stringsAsFactors = FALSE
  )
  return(list(est = df.out, weights = weights.mat, categories = categ))
}
