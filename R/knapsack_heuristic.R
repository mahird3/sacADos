## GPL-3 License
## Copyright (c) 2026 Mahir Dursunoglu

#' Fractional (LP) upper bound for the 0-1 knapsack
#'
#' Sort by density v/w, fill greedily, split the critical item fractionally.
#' Gives UB >= OPT, so (UB - H)/UB bounds heuristic H's gap without knowing OPT.
#' O(n log n).
#'
#' @param W Capacity.
#' @param w Weights.
#' @param v Values.
#' @return A single numeric: the LP upper bound.
#' @examples
#' inst <- generate_knapsack(20, type = "uncorrelated")
#' knapsack_lp_bound(inst$W, inst$w, inst$v)
#' @export
knapsack_lp_bound <- function(W, w, v)
{
  ord <- order(v / w, decreasing = TRUE)
  cap <- W
  val <- 0
  for (i in ord)
  {
    if (w[i] <= cap)
    {
      cap <- cap - w[i]
      val <- val + v[i]
    }
    else
    {
      val <- val + v[i] * (cap / w[i])   # fractional split of the critical item
      break
    }
  }
  val
}


#' Greedy ratio heuristic for the 0-1 knapsack (1/2-approximation)
#'
#' Sort by v/w, take items while they fit, then compare with the best single
#' fitting item (this repair step guarantees value >= OPT/2). O(n log n).
#'
#' @param W Capacity.
#' @param w Weights.
#' @param v Values.
#' @return List with value and selected.
#' @examples
#' inst <- generate_knapsack(20, type = "uncorrelated")
#' knapsack_greedy(inst$W, inst$w, inst$v)
#' @export
knapsack_greedy <- function(W, w, v)
{
  ord <- order(v / w, decreasing = TRUE)
  cap <- W
  val <- 0
  sel <- integer(0)
  for (i in ord)
  {
    if (w[i] <= cap)
    {
      cap <- cap - w[i]
      val <- val + v[i]
      sel <- c(sel, i)
    }
  }

  # Safety net: best single item that fits (guarantees 1/2-approximation)
  fits <- which(w <= W)
  if (length(fits) > 0)
  {
    j <- fits[which.max(v[fits])]
    if (v[j] > val)
    {
      val <- v[j]
      sel <- j
    }
  }

  list(value = val, selected = sort(sel))
}


#' Greedy + local search heuristic for the 0-1 knapsack
#'
#' Start from the greedy ratio solution, then improve it by repeatedly:
#' (1,0) inserting any fitting rejected item, and (1,1) swapping items
#' when the exchange improves value. Stop when no improving move is found.
#' O(n log n) construction + O(n^2) per pass; passes is small in practice.
#'
#' @param W Capacity.
#' @param w Weights.
#' @param v Values.
#' @param max_pass Max local-search passes (safety cap).
#' @return List with value, selected, passes.
#' @examples
#' inst <- generate_knapsack(30, type = "strongly")
#' knapsack_heuristic(inst$W, inst$w, inst$v)
#' @export
knapsack_heuristic <- function(W, w, v, max_pass = 50L)
{
  n <- length(w)
  g <- knapsack_greedy(W, w, v)

  in_set <- logical(n)
  in_set[g$selected] <- TRUE
  cur_w <- sum(w[in_set])
  cur_v <- sum(v[in_set])

  passes <- 0L
  repeat
  {
    improved <- FALSE
    passes <- passes + 1L

    # (1,0) additions: insert any fitting rejected item
    for (j in which(!in_set))
    {
      if (cur_w + w[j] <= W)
      {
        in_set[j] <- TRUE
        cur_w <- cur_w + w[j]
        cur_v <- cur_v + v[j]
        improved <- TRUE
      }
    }

    # (1,1) swaps: drop a, add b if it fits and strictly improves value
    inside <- which(in_set)
    outside <- which(!in_set)
    for (a in inside)
    {
      for (b in outside)
      {
        if (in_set[a] && !in_set[b] &&
            cur_w - w[a] + w[b] <= W &&
            v[b] - v[a] > 0)
        {
          in_set[a] <- FALSE
          in_set[b] <- TRUE
          cur_w <- cur_w - w[a] + w[b]
          cur_v <- cur_v - v[a] + v[b]
          improved <- TRUE
        }
      }
    }

    if (!improved || passes >= max_pass) break
  }

  list(value = cur_v, selected = which(in_set), passes = passes)
}
