## GPL-3 License
## Copyright (c) 2026 Mahir Dursunoglu

# Recover the optimal item set by walking back through the DP table.
traceback_dp <- function(M, w, W)
{
  n <- nrow(M) - 1
  selected <- integer(0)
  cap <- W
  for (i in n:1)
  {
    if (M[i + 1, cap + 1] != M[i, cap + 1])
    {
      selected <- c(i, selected)
      cap <- cap - w[i]
    }
  }
  selected
}


#' DP solver for the 0-1 knapsack (R)
#'
#' Fills the (n+1) x (W+1) Bellman table and recovers the optimal set by
#' traceback. O(n*W) time and space (pseudo-polynomial).
#'
#' @param W Capacity.
#' @param w Integer vector of weights.
#' @param v Numeric vector of values.
#' @return List with value and selected.
#' @examples
#' inst <- generate_knapsack(20, type = "uncorrelated")
#' knapsack_dp(inst$W, inst$w, inst$v)
#' @export
knapsack_dp <- function(W, w, v)
{
  n <- length(w)
  M <- matrix(0, nrow = n + 1, ncol = W + 1)

  for (i in 1:n)
  {
    wi <- w[i]
    for (cap in 0:W)
    {
      if (wi <= cap)
        M[i + 1, cap + 1] <- max(M[i, cap + 1], v[i] + M[i, cap - wi + 1])
      else
        M[i + 1, cap + 1] <- M[i, cap + 1]
    }
  }

  list(value = M[n + 1, W + 1], selected = traceback_dp(M, w, W))
}
