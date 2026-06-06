## GPL-3 License
## Copyright (c) 2026 Mahir Dursunoglu

#' Exhaustive solver for the 0-1 knapsack (R)
#'
#' Enumerates all 2^n subsets via bit masking. O(2^n * n); only practical
#' for n <= 25 or so. n must be <= 30 (bitwAnd is 32-bit).
#'
#' @param W Capacity.
#' @param w Integer vector of weights.
#' @param v Numeric vector of values.
#' @return List with value and selected (item indices).
#' @examples
#' inst <- generate_knapsack(15, type = "uncorrelated")
#' knapsack_naive(inst$W, inst$w, inst$v)
#' @export
knapsack_naive <- function(W, w, v)
{
  n <- length(w)
  if (n > 30) stop("knapsack_naive only supports n <= 30 (2^n enumeration).")
  best_value <- 0
  best_set <- integer(0)
  pow2 <- 2L^(0:(n - 1))

  for (i in 0:(2^n - 1))
  {
    take <- bitwAnd(i, pow2) > 0L          # logical vector of taken items
    total_w <- sum(w[take])
    if (total_w <= W)
    {
      total_v <- sum(v[take])
      if (total_v > best_value)
      {
        best_value <- total_v
        best_set <- which(take)
      }
    }
  }
  list(value = best_value, selected = best_set)
}
