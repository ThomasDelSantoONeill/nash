#'Computes harvesting rates at the Nash Equilibrium
#'
#'Function that takes a numeric vector `\code{par}` of initial harvesting rates
#' to be optimised by evaluating the objective function `\code{fn}` that is to
#' be maximised. Its structure in terms of arguments and outputs is analogous
#' to that of \code{\link{optim}}.
#'
#'@param par Numeric vector of harvesting rates of length equal to the number of
#' harvested species.
#'@param fn Function that runs the ODE model with \code{par} as input and
#' returns simulated yields at equilibrium.
#'@param ... Further arguments to be passed to \code{fn}.
#'@param method Method utilised to compute the Nash Equilibrium:
#' (i) `\code{LV}` or (ii) `\code{round-robin}` method.
#'@param yield.cruves Logical TRUE/FALSE if equilibrium yield curves for each
#' species are desired.
#'@param conv.criterion Absolute convergence tolerance set by default to
#' \eqn{< 0.001}.
#'@param Bcons Biomass conservation threshold. Default set to \eqn{0}.
#'@param F.increase Double type numeric vector indicating the step size used
#' to compute the interaction matrix.
#'
#'@details For ecosystem models where there is some interest in keeping some or
#' all harvested species above a certain biomass state limit, \code{Bcons}
#' should be populated with non zero biomass values. The length of this vector
#' must be the same as \code{par} and set to non zero where relevant. In the
#' literature, is common practice to fixed such \emph{constraint of biodiversity
#' conservation} \insertCite{Matsuda2006}{nash} to \eqn{10%} of the virgin
#' biomass; level at which a stock is considered collapsed
#' \insertCite{Worm2009}{nash}.
#'
#'\loadmathjax
#' Equilibrium yield curves are obtained for each \eqn{i} species by applying
#' different harvesting values to \eqn{i} whilst keeping the other species
#' \eqn{j} at the optimised \code{par} levels (\emph{i.e.} at the Nash
#' Equilibrium). These harvesting values applied to \eqn{i} run from \eqn{0}
#' to \mjeqn{F_{\text{Nash},i}\times 2}{ascii} in increments of \eqn{0.025}.
#' As raised by \insertCite{Thorpe2017;textual}{nash}, this is
#' one of the advantages of using the Nash Equilibrium as a representation of
#' the \emph{Maximum Sustainable Yield} concept.
#'
#' To compute the interaction matrix a second order central difference quotient
#' is used to approximate derivatives. \code{F.increase} is employed during
#' this calculation as a step-size set by default to \eqn{0.1} to avoid
#' truncation and/or rounding errors \insertCite{Pope2019}{nash}.
#'
#' The `\code{LV}` method is set by default given its performance advantage
#' over the `\code{round-robin}` method and is based on the protocol devised by
#' \insertCite{Farcas2016}{nash}. For each species \eqn{i} in turn,
#' \code{round-robin} iteratively maximises the yield by adjusting the harvesting
#' rates whereas \code{LV} does the same for all species at once per iteration.
#'
#'@return The function \code{nash} returns a list with the following components:
#'\item{par}{Harvesting rates at the Nash Equilibrium.}
#'\item{value}{Value of \code{fn} corresponding to the optimised \code{par}.}
#'\item{counts}{Number of calls to \code{fn}.}
#'\item{convergence}{Statement indicating the number of iterations for
#'\code{conv.criterion} to be reached.}
#'
#'@references
#'\insertRef{Matsuda2006}{nash}
#'
#'\insertRef{Worm2009}{nash}
#'
#'\insertRef{Thorpe2017}{nash}
#'
#'\insertRef{Pope2019}{nash}
#'
#'\insertRef{Farcas2016}{nash}
#'
#'\insertRef{Lucey2020}{nash}
#'
#'@export
nash <- function(par, fn, ..., method = "LV", yield.curves = FALSE,
                 conv.criterion = 0.001, Bcons = 0, F.increase = 0.1,
                 progress = TRUE){
  ### VALIDATOR
  if (!is.vector(par)) {
    stop("`par` is not a vector.")
  }
  if (length(par) < 1) {
    stop("`par` must be a vector of length >= 1")
  }
  if (!method%in%list("LV", "round-robin")){
    stop("Sorry but this method is not implemented")
  }
  if (method == "LV") {
    ### LOCAL VARIABLES
    nSpp <- length(par)
    nash_fncalls <- 0
    n.iter <- 200
    Nash_Fs <- array(dim = c(n.iter, nSpp))
    Nash_Bs <- array(dim = c(n.iter, nSpp))
    Nash_Rs <- array(dim = c(n.iter, nSpp))
    output <- fn(par, ...)
    yields <- output
    # B0 <- output$B0 # Initial biomass used for conservation constraints
    # Avoid numerical errors computing M due to par elements being exactly =
    #   to F.increase
    if (any(par==F.increase)==TRUE) {
      elements <- which(par==F.increase)
      par[elements] <- par[elements] + 1e-5
    }
    B.eq <- as.numeric(yields) / par
    F.eq <- par
    M <- matrix(nrow = nSpp, ncol = nSpp)
    ### ALGORITHM
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
      # Interaction Matrix and growth rates
      G <- -1 * solve(M)
      r <- (G %*% B.eq) + F.eq
      G_hat <- diag(1 / (diag(solve(G))))
      # Bnash
      B_new <- solve(G + G_hat, r)
      ### CONSERVATION CONSTRAINTS
      # Define Bcons state
      # Bcons <- B0*(Bper/100)
      # B vector if any i is to be conserved
      Bcomplete <- rep(NA, nSpp)
      # Targeted with all TRUE entries
      targeted <- !vector(mode = "logical", length = nSpp)
      Bcomplete[targeted] <- as.numeric(B_new)
      # Loop
      while (any(targeted)) {
        ratios <- Bcons/as.numeric(Bcomplete)
        if (all(ratios > 1)) {
          # Which i is in the worst state; max ratio indicates that
          # the new Bnash computed is well below Bcons and therefore
          # in a very bad state.
          consspp <- which.max(ratios)
          # Reduce targeted TRUE vector by 1 (i.e. the species being
          # conserved)
          targeted[consspp] <- FALSE
          # Remove consspp from the system and compute new Bnash
          Gff <- G[targeted,targeted,drop=F]
          Gfn <- G[targeted,!targeted,drop=F]
          # If all species are below Bcons then Gff will be empty
          if (det(Gff)==1) {
            Bcomplete[!targeted] <- Bcons[!targeted]
            break
          }
          r_new <- (r[targeted,drop=F] - Gfn %*% Bcons[!targeted,drop=F])
          G_hat <- diag(1 / (diag(solve(Gff))))
          # Bnash
          B_new <- solve(Gff + G_hat, r_new)
          Bcomplete[targeted] <- B_new
          Bcomplete[!targeted] <- Bcons[!targeted]
        } else {
          break
        }
      }
      # Fnash
      F_new <- r - G %*% Bcomplete
      # Saving
      Nash_Fs[iter,] <- F_new
      Nash_Bs[iter,] <- B_new
      Nash_Rs[iter,] <- r
      # Re-running the model to equilibrium with new Nash Fs
      par <- as.numeric(F_new)
      if (progress == TRUE) {
        print(max(abs(F.eq / Nash_Fs[(iter),] -1)))
      }
      yields <- fn(par, ...)
      B.eq <- as.numeric(yields) / par
      F.eq <- par
      nash_fncalls <- nash_fncalls + 1
      # Convergence statement
      if (iter>1) {
        if (max(abs(F_new / Nash_Fs[(iter-1),] -1)) < conv.criterion) {
          # print(paste("Nash equilibrium found after ", iter,
          #             " iterations with", nash_fncalls,
          #             " function calls."))
          break
        }
      }
    }
    ### OUTPUT
    # outlist <- list(Nash_Fs = Nash_Fs[1:iter,], Nash_Bs = Nash_Bs[1:iter,],
    #                 Nash_Rs = Nash_Rs[1:iter,], func.evals = nash_fncalls)
    outlist <- list(par = tail(na.omit(Nash_Fs), n = 1),
                    value = fn(as.numeric(tail(na.omit(Nash_Fs),
                                               n = 1)), ...),
                    counts = nash_fncalls,
                    convergence = paste("Nash equilibrium found after ", iter,
                                        " iterations."))
  }
  if (method == "round-robin") {
    ### LOCAL VARIABLES
    nSpp <- length(par)
    nash_fncalls <- 0
    conv.criterion <- 0.001
    n.iter <- 200
    Nash_Hs <- array(dim = c(n.iter, nSpp))
    F.eq <- par
    ### COMPUTE YIELDS ONE AT A TIME
    Yield <- function(par, Hvec, j){
      Hvec[j] <- par
      nash_fncalls <- nash_fncalls + 1
      as.numeric(fn(Hvec, ...))[j]
    }
    ### ALGORITHM
    for (iter in 1:n.iter) {
      for (j in 1:nSpp) {
        output <- optim(par = par[j], fn = Yield, Hvec = par, j = j,
                        control = list(fnscale = -1, abstol = 0.001,
                                       factr = 1e12),
                        method = "BFGS", hessian = TRUE)
        par[j] = output$par
        nash_fncalls <- nash_fncalls + output$counts[1]
      }
      Nash_Hs[iter,] <- par
      if (progress == TRUE) {
        print(max(abs(F.eq / Nash_Hs[(iter),] -1)))
      }
      F.eq <- par
      if (iter>1) {
        if (max(abs(Nash_Hs[iter,] / Nash_Hs[(iter-1),] -1)) < 0.001) {
          # print(paste("Nash equilibrium found after ", iter,
          #             " iterations with", nash_fncalls,
          #             " function calls."))
          break
        }
      }
    }
    ### OUTPUT
    # outlist <- list(Nash_Fs = Nash_Hs[1:iter,], func.evals = nash_fncalls)
    outlist <- list(par = tail(na.omit(Nash_Hs), n = 1),
                    value = fn(as.numeric(tail(na.omit(Nash_Hs),
                                               n = 1)), ...),
                    counts = nash_fncalls,
                    convergence = paste("Nash equilibrium found after ", iter,
                                        " iterations."))
  }
  ### EQUILIBRIUM YIELD CURVES
  if (yield.curves == TRUE) {
    Yield <- function(par, Hvec, j){
      Hvec[j] <- par
      nash_fncalls <- nash_fncalls + 1
      as.numeric(fn(Hvec, ...))[j]
    }
    Yieldeq <- list()
    par <- c()
    if (method=="round-robin" | method=="LV") {
      par <- as.numeric(outlist$par)
    }
    for (i in 1:length(par)) {
      Fvec <- seq(0,par[i]*2,0.025)
      Yieldspp <- sapply(X = Fvec, FUN = Yield, Hvec = par, j = i)
      outyield <- cbind(Fvec, Yieldspp)
      Yieldeq[[i]] <- outyield
    }
    outlist$YieldEQ <- Yieldeq
  }
  ### RETURN
  return(outlist)
}
