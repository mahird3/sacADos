## GPL-3 License
## Copyright (c) 2026 Mahir Dursunoglu

#' Online knapsack with a time-and-space potential (intermediate design)
#'
#' Accept item i iff d = v/w >= clamp(L + (U-L)*z*C, L, U), where
#' z = fill fraction, t = stream progress, C = (1-t)/(1-z). C large means
#' many items remain relative to space left -> threshold rises. O(n).
#'
#' @param weights Weights (arrival order).
#' @param values Values (arrival order).
#' @param capacity Capacity W.
#' @param L Lower density bound.
#' @param U Upper density bound.
#' @return List with selected, value, used_weight.
#' @examples
#' set.seed(1)
#' inst <- generate_knapsack(50, type = "uncorrelated")
#' L <- min(inst$v / inst$w); U <- max(inst$v / inst$w)
#' time_space_knapsack(inst$w, inst$v, inst$W, L, U)
#' @export
time_space_knapsack <- function(weights, values, capacity, L, U)
{
  n <- length(weights)
  used <- 0
  total_value <- 0
  selected <- integer(0)

  for (i in seq_len(n))
  {
    density <- values[i] / weights[i]

    # State "so far", before deciding item i; capped to avoid division by zero.
    z <- min(used / capacity, 0.999)
    t <- min((i - 1) / n, 0.999)

    C <- (1 - t) / (1 - z)
    p <- min(U, max(L, L + (U - L) * z * C))

    if (used + weights[i] <= capacity && density >= p)
    {
      used <- used + weights[i]
      total_value <- total_value + values[i]
      selected <- c(selected, i)
    }
  }

  list(selected = selected, value = total_value, used_weight = used)
}
