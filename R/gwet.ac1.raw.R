#' @importFrom stats na.omit pt qt

gwet.ac1.raw <- function (ratings, weights = "unweighted", categ.labels = NULL, 
                          conflev = 0.95, N = Inf)  {
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
  if (q >= 2) {
    pe <- sum(weights.mat) * sum(pi.vec * (1 - pi.vec))/(q * 
                                                           (q - 1))
  }
  else pe = (1 - 1e-15)
  gwet.ac1 <- (pa - pe)/(1 - pe)
  gwet.ac1.est <- round(gwet.ac1, 5)
  den.ivec <- ri.vec * (ri.vec - 1)
  den.ivec <- den.ivec - (den.ivec == 0)
  pa.ivec <- sum.q/den.ivec
  pe.r2 <- pe * (ri.vec >= 2)
  ac1.ivec <- (n/n2more) * (pa.ivec - pe.r2)/(1 - pe)
  pe.ivec <- (sum(weights.mat)/(q * (q - 1))) * (agree.mat %*% 
                                                   (1 - pi.vec))/ri.vec
  ac1.ivec.x <- ac1.ivec - 2 * (1 - gwet.ac1) * (pe.ivec - 
                                                   pe)/(1 - pe)
  var.ac1 <- NA
  stderr <- NA
  stderr.est <- NA
  p.value <- NA
  lcb <- NA
  ucb <- NA
  if (n >= 2) {
    var.ac1 <- ((1 - f)/(n * (n - 1))) * sum((ac1.ivec.x - 
                                                gwet.ac1)^2)
    stderr <- sqrt(var.ac1)
    stderr.est <- round(stderr, 5)
    p.value <- 2 * (1 - pt(abs(gwet.ac1/stderr), n - 1))
    lcb <- gwet.ac1 - stderr * qt(1 - (1 - conflev)/2, n - 
                                    1)
    ucb <- min(1, gwet.ac1 + stderr * qt(1 - (1 - conflev)/2, 
                                         n - 1))
  }
  conf.int <- paste0("(", round(lcb, 3), ",", round(ucb, 3), 
                     ")")
  if (sum(weights.mat) == q) 
    coeff.name <- "AC1"
  else coeff.name <- "AC2"
  coeff.val <- gwet.ac1.est
  coeff.se <- stderr.est
  df.out <- data.frame(coeff.name, pa, pe, coeff.val, coeff.se, 
                       conf.int, p.value, w.name)
  return(list(est = df.out, weights = weights.mat, categories = categ))
}