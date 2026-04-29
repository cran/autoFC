#' @importFrom stats na.omit pt qt

bp.coeff.raw <- function (ratings, weights = "unweighted", categ.labels = NULL, 
          conflev = 0.95, N = Inf) {
  ratings.mat <- as.matrix(ratings)
  if (is.character(ratings.mat)) {
    ratings.mat <- trim(toupper(ratings.mat))
    ratings.mat[ratings.mat == ""] <- NA_character_
  }
  n <- nrow(ratings.mat)
  r <- ncol(ratings.mat)
  f <- n/N
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
    agree.mat[, k] <- (replace(categ.is.k, is.na(categ.is.k), 
                               FALSE)) %*% rep(1, r)
  }
  agree.mat.w <- t(weights.mat %*% t(agree.mat))
  ri.vec <- agree.mat %*% rep(1, q)
  sum.q <- (agree.mat * (agree.mat.w - 1)) %*% rep(1, q)
  n2more <- sum(ri.vec >= 2)
  pa <- sum(sum.q[ri.vec >= 2]/((ri.vec * (ri.vec - 1))[ri.vec >= 
                                                          2]))/n2more
  pi.vec <- t(t(rep(1/n, n)) %*% (agree.mat/(ri.vec %*% t(rep(1, 
                                                              q)))))
  pe <- sum(weights.mat)/(q^2)
  bp.coeff <- (pa - pe)/(1 - pe)
  bp.coeff.est <- round(bp.coeff, 5)
  den.ivec <- ri.vec * (ri.vec - 1)
  den.ivec <- den.ivec - (den.ivec == 0)
  pa.ivec <- sum.q/den.ivec
  pe.r2 <- pe * (ri.vec >= 2)
  bp.ivec <- (n/n2more) * (pa.ivec - pe.r2)/(1 - pe)
  var.bp <- NA
  stderr <- NA
  stderr.est <- NA
  p.value <- NA
  lcb <- NA
  ucb <- NA
  if (n >= 2) {
    var.bp <- ((1 - f)/(n * (n - 1))) * sum((bp.ivec - bp.coeff)^2)
    stderr <- sqrt(var.bp)
    stderr.est <- round(stderr, 5)
    p.value <- 2 * (1 - pt(abs(bp.coeff/stderr), n - 1))
    lcb <- bp.coeff - stderr * qt(1 - (1 - conflev)/2, n - 
                                    1)
    ucb <- min(1, bp.coeff + stderr * qt(1 - (1 - conflev)/2, 
                                         n - 1))
  }
  conf.int <- paste0("(", round(lcb, 3), ",", round(ucb, 3), 
                     ")")
  coeff.name <- "Brennan-Prediger"
  coeff.val <- bp.coeff.est
  coeff.se <- stderr.est
  df.out <- data.frame(coeff.name, pa, pe, coeff.val, coeff.se, 
                       conf.int, p.value, w.name)
  return(list(est = df.out, weights = weights.mat, categories = categ))
}