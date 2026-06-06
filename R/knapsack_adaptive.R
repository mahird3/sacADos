## GPL-3 License
## Copyright (c) 2026 Mahir Dursunoglu

# Pearson rho from six running sums. Returns 0 if too few points or no variance.
cor_running <- function(Sw, Sv, Swv, Sww, Svv, cnt)
{
  if (cnt < 3) return(0)
  denom <- sqrt(max(0, Sww - Sw^2 / cnt) * max(0, Svv - Sv^2 / cnt))
  if (denom == 0) 0 else (Swv - Sw * Sv / cnt) / denom
}


#' Self-tuning online knapsack heuristic (R, O(n))
#'
#' Streaming solver. Accept item i iff d = v/w >= threshold p, where
#'   p = clamp(L + (U-L)*(z + k*(z-t)), L, U)
#' z = fill fraction, t = stream progress. The pace term k*(z-t) relaxes
#' the threshold when we're behind schedule (z < t) and tightens it when ahead.
#'
#' k is learned online from the running w-v correlation rho:
#'   k = k_lo + (k_hi - k_lo) * max(0, rho)
#' High rho (strongly correlated) -> large k -> bag fills easily.
#' Low rho (uncorrelated) -> small k -> selective about high-density items.
#'
#' L, U are the 5th/95th density percentiles from the warm-up window, then
#' frozen. If spread (U-L)/U is tiny (subset-sum), just fill whatever fits.
#'
#' @param w Weights (arrival order).
#' @param v Values (arrival order).
#' @param W Capacity.
#' @param k_lo Selectivity for uncorrelated case (default 4).
#' @param k_hi Selectivity for perfectly correlated case (default 30).
#' @param warmup Items used to estimate L, U (default max(10, 0.1*n)).
#' @return List with value, selected, used_weight.
#' @examples
#' set.seed(1)
#' inst <- generate_knapsack(200, type = "uncorrelated")
#' knapsack_adaptive(inst$w, inst$v, inst$W)$value
#' @export
knapsack_adaptive <- function(w, v, W, k_lo = 4, k_hi = 30, warmup = NULL)
{
  n <- length(w)
  if (is.null(warmup)) warmup <- max(10L, ceiling(0.1 * n))
  warmup <- min(warmup, n)

  used <- 0
  value <- 0
  taken <- logical(n)

  # Running sums for O(1) correlation update
  Sw <- 0; Sv <- 0; Swv <- 0; Sww <- 0; Svv <- 0; cnt <- 0

  warm_d <- numeric(0)
  L <- 1e-6; U <- 1; frozen <- FALSE

  for (i in seq_len(n))
  {
    d <- v[i] / w[i]
    z <- min(used / W, 0.999)
    t <- min((i - 1) / n, 0.999)

    # Build density bounds from warm-up window, then freeze
    if (!frozen)
    {
      warm_d <- c(warm_d, d)
      if (length(warm_d) >= 3)
      {
        q <- stats::quantile(warm_d, c(0.05, 0.95), names = FALSE)
        L <- max(q[1], 1e-6)
        U <- max(q[2], L * 1.0001)
      }
      if (i >= warmup) frozen <- TRUE
    }

    # Learned selectivity
    rho <- cor_running(Sw, Sv, Swv, Sww, Svv, cnt)
    k   <- k_lo + (k_hi - k_lo) * max(0, min(1, rho))

    # Decision
    spread <- (U - L) / U
    if (spread < 1e-3)
    {
      accept <- (used + w[i] <= W)
    }
    else
    {
      p      <- min(U, max(L, L + (U - L) * (z + k * (z - t))))
      accept <- (used + w[i] <= W) && (d >= p)
    }

    # Observe item (online: allowed), then commit
    Sw <- Sw + w[i]; Sv <- Sv + v[i]; Swv <- Swv + w[i] * v[i]
    Sww <- Sww + w[i]^2; Svv <- Svv + v[i]^2; cnt <- cnt + 1

    if (accept)
    {
      used    <- used  + w[i]
      value   <- value + v[i]
      taken[i] <- TRUE
    }
  }

  list(value = value, selected = which(taken), used_weight = used)
}
