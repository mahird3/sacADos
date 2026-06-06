// GPL-3 License
// Copyright (c) 2026 Mahir Dursunoglu

#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;

// Walk back through the DP table to find which items were taken.
static std::vector<int> traceback(const std::vector<std::vector<double>>& M,
                                   const IntegerVector& w, int W)
{
  int n = (int)M.size() - 1;
  std::vector<int> sel;
  int c = W;
  for (int i = n; i >= 1; --i)
  {
    if (M[i][c] != M[i - 1][c])
    {
      sel.push_back(i);   // 1-based
      c -= w[i - 1];
    }
  }
  std::reverse(sel.begin(), sel.end());
  return sel;
}


//' DP solver for the 0-1 knapsack (C++ / Rcpp)
//'
//' Same algorithm as knapsack_dp but in C++. O(n*W) time and space.
//' Used for the R-vs-C++ speed comparison.
//'
//' @param W Capacity.
//' @param w Integer vector of weights.
//' @param v Numeric vector of values.
//' @return List with value and selected (1-based indices).
//' @examples
//' inst <- generate_knapsack(20, type = "uncorrelated")
//' knapsack_dp_Rcpp(inst$W, inst$w, inst$v)
//' @export
// [[Rcpp::export]]
List knapsack_dp_Rcpp(int W, IntegerVector w, NumericVector v)
{
  int n = w.size();
  std::vector<std::vector<double>> M(n + 1, std::vector<double>(W + 1, 0.0));

  for (int i = 1; i <= n; ++i)
  {
    int wi    = w[i - 1];
    double vi = v[i - 1];
    for (int c = 0; c <= W; ++c)
    {
      if (wi <= c)
        M[i][c] = std::max(M[i - 1][c], vi + M[i - 1][c - wi]);
      else
        M[i][c] = M[i - 1][c];
    }
  }

  return List::create(_["value"]    = M[n][W],
                      _["selected"] = wrap(traceback(M, w, W)));
}
