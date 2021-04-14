#'Computes harvesting rates at the Nash Equilibrium
#'
#'Function that takes a numeric vector `\code{par}` of initial harvesting rates
#' to be optimised by evaluating the objective function `\code{fn}` to be
#' maximised. The argument structure is very much alike \code{\link{optim}}.
#'
#'@param par Vector of harvesting rates of length equal to the number of
#' harvested species.
#'@param fn Function that runs the ODE model taking harvesting rates as input
#' and returning simulated yields at equilibrium.
#'@param ... Further arguments to be passed to \code{fn}.
#'@param method method utilised to compute Nash Equilibrium harvesting rates:
#' (i) `\code{LV}` or (ii) `\code{dummy}` method.
#'@param yield.cruves Logical if equilibrium yields are to be computed for each
#' \eqn{i} species whilst keeping the rest at the optimised \code{par} levels
#' (\emph{i.e.} at the Nash Equilibrium).
#'@param conv.criterion Absolute convergence tolerance set by default to
#' \eqn{< 0.001}.
#'
#'@details Something
#'@return something
#'@export
nash <- function(par, fn, ..., method = "LV", yield.curves = FALSE,
                 conv.criterion = 0.001){
  # VALIDATOR
  if (!is.vector(par)) {
    stop("`par` is not a vector.")
  }
  if (length(par) < 1) {
    stop("`par` must be a vector of length >= 1")
  }
  if (!method%in%list("LV", "dummy")){
    stop("Sorry but this method is not implemented")
  }
  if (method == "LV") {
    # LOCAL VARIABLES
    nSpp <- length(par)
    nash_fncalls <- 0
    n.iter <- 100
    F.increase <- 0.1
    Nash_Fs <- array(dim = c(n.iter, nSpp))
    Nash_Bs <- array(dim = c(n.iter, nSpp))
    Nash_Rs <- array(dim = c(n.iter, nSpp))
    yields <- fn(par, ...)
    B.eq <- as.numeric(yields) / par
    F.eq <- par
    M <- matrix(nrow = nSpp, ncol = nSpp)
    # ALGORITHM
    # `...` is for arguments that our algorithm does not recognise but `fn` does
    for (iter in 1:n.iter) {
      for (i in 1:nSpp) {
        #Parameters
        par.p <- par
        par.m <- par
        par.p[i] <- par.p[i] + F.increase
        par.m[i] <- par.m[i] - F.increase
        #Running model
        yields.p <- fn(par.p, ...)
        B.p <- as.numeric(yields.p) / par.p
        nash_fncalls <- nash_fncalls + 1
        yields.m <- fn(par.m, ...)
        B.m <- as.numeric(yields.m) / par.m
        nash_fncalls <- nash_fncalls + 1
        #Populating M
        M[,i] <- (B.p - B.m) / (F.increase * 2)
      }
      if (det(M)==0){
        extinct.name   <- names(which(colSums(M) == 0))
        print("Initial M is singular and hence non invertible")
        print(extinct.name)
      }
      G <- -1 * solve(M)
      r <- (G %*% B.eq) + F.eq
      G_hat <- diag(1 / (diag(solve(G))))
      B_new <- solve(G + G_hat, r)
      F_new <- G_hat %*% B_new
      ##	Saving
      Nash_Fs[iter,] <- F_new
      Nash_Bs[iter,] <- B_new
      Nash_Rs[iter,] <- r
      # Re-running the model to equilibrium with new Nash Fs
      par <- as.numeric(F_new)
      # print(max(abs(F.eq / Nash_Fs[(iter),] -1)))
      yields <- fn(par, ...)
      B.eq <- as.numeric(yields) / par
      F.eq <- par
      nash_fncalls <- nash_fncalls + 1
      # Convergence statement
      if (iter>1) {
        if (max(abs(F_new / Nash_Fs[(iter-1),] -1)) < conv.criterion) {
          # print(paste("Nash equilibrium found after ", iter,
          #             " iterations with", nash_fncalls, " function calls."))
          break
        }
      }
    }
    # OUTPUT
    outlist <- list(Nash_Fs = Nash_Fs[1:iter,], Nash_Bs = Nash_Bs[1:iter,],
                    Nash_Rs = Nash_Rs[1:iter,], func.evals = nash_fncalls)
  }
  if (method == "dummy") {
    # LOCAL VARIABLES
    nSpp <- length(par)
    nash_fncalls <- 0
    n.iter <- 100
    Nash_Hs <- array(dim = c(n.iter, nSpp))
    F.eq <- par
    # Compute Spp yields one at a time
    Yield <- function(par, Hvec, j){
      Hvec[j] <- par
      nash_fncalls <- nash_fncalls + 1
      as.numeric(fn(Hvec, ...))[j]
    }
    for (iter in 1:n.iter) {
      for (j in 1:nSpp) {#Length of Spp vector
        output <- optim(par = par[j], fn = Yield, Hvec = par, j = j,
                        control = list(fnscale = -1, abstol = conv.criterion,
                                       factr = 1e12),
                        method = "BFGS", hessian = TRUE)
        par[j] = output$par
        nash_fncalls <- nash_fncalls + output$counts[1]
      }
      Nash_Hs[iter,] <- par
      # print(max(abs(F.eq / Nash_Hs[(iter),] -1)))
      F.eq <- par
      if (iter>1) {
        if (max(abs(Nash_Hs[iter,] / Nash_Hs[(iter-1),] -1)) < conv.criterion){
          # print(paste("Nash equilibrium found after ", iter,
          #             " iterations with", nash_fncalls, " function calls."))
          break
          }
      }
    }
    # OUTPUT
    outlist <- list(Nash_Fs = Nash_Hs[1:iter,], func.evals = nash_fncalls)
  }
  if (yield.curves == TRUE) {
    Yield <- function(par, Hvec, j){
      Hvec[j] <- par
      as.numeric(fn(Hvec, ...))[j]
    }
    Fvec <- seq(0,1.5,0.025)
    Yieldeq <- array(dim = c(length(Fvec), length(par)))
    par <- c()
    if (method=="dummy" | method=="LV") {
      par <- as.numeric(tail(outlist$Nash_Fs, n = 1))
    }
    for (i in 1:length(par)) {
      Yieldeq[,i] <- sapply(X = Fvec, FUN = Yield, Hvec = par, j = i)
    }
    outlist$Yieldeq <- Yieldeq
  }
  # Return
  return(outlist)
}
