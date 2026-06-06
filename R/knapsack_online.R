## GPL-3 License
## Copyright (c) 2026 Mahir Dursunoglu

#' Online knapsack with a pluggable threshold function
#'
#' Streaming solver: items arrive in order, decide accept/reject immediately.
#' Accept item i iff it fits and density d = v/w >= Psi(z), where
#' z = used_weight/W. O(n). Use the psi_* helpers to build Psi.
#'
#' @param W Capacity.
#' @param w Weights (arrival order).
#' @param v Values (arrival order).
#' @param Psi Function of z in [0,1] returning the acceptance threshold.
#' @return List with value, selected, and trace (data frame for plotting).
#' @examples
#' set.seed(1)
#' inst <- generate_knapsack(50, type = "uncorrelated")
#' L <- min(inst$v / inst$w); U <- max(inst$v / inst$w)
#' knapsack_online(inst$W, inst$w, inst$v, psi_exponential(L, U))$value
#' @export
knapsack_online <- function(W, w, v, Psi)
{
  n <- length(w)
  used <- 0
  value <- 0
  selected <- integer(0)

  z_vec <- numeric(n)
  thr_vec <- numeric(n)
  dens_vec <- numeric(n)
  fits_vec <- logical(n)
  acc_vec <- logical(n)

  for (i in seq_len(n))
  {
    z <- used / W
    d <- v[i] / w[i]
    thr <- Psi(z)
    fits <- (used + w[i] <= W)
    accept <- fits && (d >= thr)

    z_vec[i] <- z
    dens_vec[i] <- d
    thr_vec[i] <- thr
    fits_vec[i] <- fits
    acc_vec[i] <- accept

    if (accept)
    {
      used <- used + w[i]
      value <- value + v[i]
      selected <- c(selected, i)
    }
  }

  trace <- data.frame(z = z_vec, density = dens_vec, threshold = thr_vec,
                      fits = fits_vec, accepted = acc_vec)
  list(value = value, selected = selected, trace = trace)
}


#' Linear potential: Psi(z) = L + (U-L)*z
#' @param L Threshold at z=0.
#' @param U Threshold at z=1.
#' @return Function of z.
#' @examples
#' f <- psi_linear(1, 10); f(0); f(0.5); f(1)
#' @export
psi_linear <- function(L, U)
{
  function(z) L + (U - L) * z
}


#' Power potential: Psi(z) = L + (U-L)*z^p
#' @param L,U Density bounds.
#' @param p Exponent (> 0). p<1 is generous early, p>1 stays lenient then rises sharply.
#' @return Function of z.
#' @examples
#' f <- psi_power(1, 10, p = 2); f(0); f(0.5); f(1)
#' @export
psi_power <- function(L, U, p = 2)
{
  function(z) L + (U - L) * z^p
}


#' Exponential potential: Psi(z) = (L/e)*(Ue/L)^z
#'
#' Worst-case optimal for online knapsack (Zhou et al. 2008); competitive
#' ratio ln(U/L)+1. Used as the benchmark in our comparisons.
#'
#' @param L Lower density bound (> 0).
#' @param U Upper density bound.
#' @return Function of z.
#' @examples
#' f <- psi_exponential(1, 10); f(0); f(0.5); f(1)
#' @export
psi_exponential <- function(L, U)
{
  e <- exp(1)
  function(z) (L / e) * (U * e / L)^z
}
