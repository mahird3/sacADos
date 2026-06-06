## GPL-3 License
## Copyright (c) 2026 Mahir Dursunoglu

#' Generate a random 0-1 knapsack instance
#'
#' Draws (w, v) from one of the four Pisinger families. "strongly" and
#' "subsetsum" are the hardest for ratio-based heuristics (Q3).
#'
#' @param n Number of items.
#' @param type One of "uncorrelated", "weakly", "strongly", "subsetsum".
#' @param R Range of the uniform draws.
#' @param cap_ratio Capacity as fraction of total weight (0.5 = hardest).
#' @param c_corr Offset for "strongly" (default R/10).
#' @return List with w, v, W, type.
#' @examples
#' set.seed(1)
#' inst <- generate_knapsack(20, type = "uncorrelated")
#' str(inst)
#' @export
generate_knapsack <- function(n, type = c("uncorrelated", "weakly", "strongly", "subsetsum"),
                              R = 1000, cap_ratio = 0.5, c_corr = NULL)
{
  type <- match.arg(type)
  w <- sample.int(R, n, replace = TRUE)

  v <- switch(type,
    uncorrelated = sample.int(R, n, replace = TRUE),
    weakly       = pmax(1L, w + sample((-R %/% 10):(R %/% 10), n, replace = TRUE)),
    strongly     = w + (if (is.null(c_corr)) R %/% 10 else c_corr),
    subsetsum    = w
  )

  W <- max(1L, as.integer(floor(cap_ratio * sum(w))))
  list(w = as.integer(w), v = as.integer(v), W = W, type = type)
}