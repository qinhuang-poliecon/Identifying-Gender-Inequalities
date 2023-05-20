fix_clust_diag <- function (x, y, unit, cluster, necessity = FALSE, wicons = FALSE) 
{
  X <- xtabs(x ~ unit + cluster)
  Y <- xtabs(y ~ unit + cluster)
  p <- 2
  con.ragin <- function(X, Y) {
    sum(apply(cbind(X, Y), 1, min))/sum(X)
  }
  con.pool <- function(x, y) {
    z <- cbind(as.vector(x), as.vector(y))
    sum(apply(z, 1, min))/sum(x)
  }
  con.betw <- function(X, Y) {
    unlist(lapply(1:ncol(X), function(j) con.ragin(X[, j], 
                                                   Y[, j])))
  }
  con.with <- function(X, Y) {
    unlist(lapply(1:nrow(X), function(i) con.ragin(X[i, 
                                                     ], Y[i, ])))
  }
  cvr.ragin <- function(X, Y) {
    sum(apply(cbind(X, Y), 1, min))/sum(Y)
  }
  cvr.pool <- function(x, y) {
    z <- cbind(as.vector(x), as.vector(y))
    sum(apply(z, 1, min))/sum(y)
  }
  cvr.betw <- function(X, Y) {
    J <- ncol(X)
    unlist(lapply(1:J, function(j) cvr.ragin(X[, j], Y[, 
                                                       j])))
  }
  cvr.with <- function(X, Y) {
    N <- nrow(X)
    unlist(lapply(1:N, function(i) cvr.ragin(X[i, ], Y[i, 
                                                       ])))
  }
  if (!necessity) {
    dBP <- function(X, Y) {
      J <- ncol(X)
      bc <- con.betw(X, Y)
      bc <- bc[!is.nan(bc)]
      sqrt(sum(((bc/sum(bc)) - (1/J))^p))
    }
    dWP <- function(X, Y) {
      N <- nrow(X)
      wc <- con.with(X, Y)
      wc <- wc[!is.nan(wc)]
      sqrt(sum(((wc/sum(wc)) - (1/N))^p))
    }
  }
  else {
    dBP <- function(X, Y) {
      J <- ncol(X)
      bc <- cvr.betw(X, Y)
      sqrt(sum(((bc/sum(bc)) - (1/J))^p))
    }
    dWP <- function(X, Y) {
      N <- nrow(X)
      wc <- cvr.with(X, Y)
      sqrt(sum(((wc/sum(wc)) - (1/N))^p))
    }
  }
  unit <- as.character(unit)
  cluster <- as.character(cluster)
  CNRC <- data.frame(table(cluster))
  cnrc <- paste(as.character(CNRC[, 1]), " (", as.character(CNRC[, 
                                                                 2]), ") ", sep = "")
  CNRU <- data.frame(table(unit))
  cnru <- paste(as.character(CNRU[, 1]), " (", as.character(CNRU[, 
                                                                 2]), ") ", sep = "")
  if (!necessity) {
    r1 <- con.pool(x, y)
    r2 <- con.betw(X, Y)
    r3 <- dBP(X, Y)
    r4 <- con.with(X, Y)
    r5 <- dWP(X, Y)
    r6 <- list(pooled = cvr.pool(x, y), between = cvr.betw(X, 
                                                           Y), within = cvr.with(X, Y))
    r7 <- cnrc
    r8 <- cnru
  }
  else {
    r1 <- cvr.pool(x, y)
    r2 <- cvr.betw(X, Y)
    r3 <- dBP(X, Y)
    r4 <- cvr.with(X, Y)
    r5 <- dWP(X, Y)
    r6 <- list(pooled = con.pool(x, y), between = con.betw(X, 
                                                           Y), within = con.with(X, Y))
    r7 <- cnrc
    r8 <- cnru
  }
  r <- list(POCOS = r1, BECOS = r2, dBP = r3, WICONS = r4, 
            dWP = r5, Coverages = r6, wiconsprint = wicons, cluster_ids = r7, 
            unit_ids = r8)
  class(r) <- "clusterdiagnostics"
  return(r)
}

## fixInNamespace("cluster.diagnostics", "SetMethods")
old_clust_diag <- get("cluster.diagnostics", envir = asNamespace("SetMethods"))
environment(fix_clust_diag) <- environment(old_clust_diag)
attributes(fix_clust_diag) <- attributes(old_clust_diag)  # don't know if this is really needed
assignInNamespace("cluster.diagnostics", fix_clust_diag, ns="SetMethods")