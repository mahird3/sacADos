// GPL-3 License
// Copyright (c) 2026 Mahir Dursunoglu

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>
using namespace Rcpp;

// Type-7 (R default) quantile of a small vector, by linear interpolation.
static double quantile7(std::vector<double> x, double p)
{
  std::sort(x.begin(), x.end());
  int n = (int)x.size();
  if (n == 1) return x[0];
  double h = (n - 1) * p;
  int lo = (int)std::floor(h);
  double frac = h - lo;
  if (lo + 1 < n) return x[lo] + frac * (x[lo + 1] - x[lo]);
  return x[n - 1];
}

// Pearson rho from six running sums. Returns 0 if too few points or no variance.
static double cor_running(double Sw, double Sv, double Swv,
                           double Sww, double Svv, int cnt)
{
  if (cnt < 3) return 0.0;
  double vw    = std::max(0.0, Sww - Sw * Sw / cnt);
  double vv    = std::max(0.0, Svv - Sv * Sv / cnt);
  double denom = std::sqrt(vw * vv);
  return denom > 0 ? (Swv - Sw * Sv / cnt) / denom : 0.0;
}


//' Self-tuning online knapsack heuristic (C++ / Rcpp)
//'
//' C++ port of knapsack_adaptive. Single pass, O(n).
//' See knapsack_adaptive for the full description of the algorithm.
//'
//' @param w Weights (arrival order).
//' @param v Values (arrival order).
//' @param W Capacity.
//' @param k_lo Selectivity for uncorrelated case (default 4).
//' @param k_hi Selectivity for perfectly correlated case (default 30).
//' @param warmup Items used to estimate L, U (-1 = auto).
//' @return List with value, selected (1-based), used_weight.
//' @examples
//' inst <- generate_knapsack(200, type = "uncorrelated")
//' knapsack_adaptive_Rcpp(inst$w, inst$v, inst$W)$value
//' @export
// [[Rcpp::export]]
List knapsack_adaptive_Rcpp(NumericVector w, NumericVector v, double W,
                            double k_lo = 4.0, double k_hi = 30.0, int warmup = -1)
{
  int n = w.size();
  if (warmup < 0) warmup = std::max(10, (int)std::ceil(0.1 * n));
  if (warmup > n) warmup = n;

  double used = 0.0, value = 0.0;
  std::vector<int> sel;

  // Running sums for O(1) correlation update
  double Sw = 0, Sv = 0, Swv = 0, Sww = 0, Svv = 0;
  int cnt = 0;

  std::vector<double> warm_d;
  double L = 1e-6, U = 1.0;
  bool frozen = false;

  for (int i = 0; i < n; ++i)
  {
    double wi = w[i], vi = v[i];
    double d  = vi / wi;
    double z  = used / W;    if (z > 0.999) z = 0.999;
    double t  = (double)i / n; if (t > 0.999) t = 0.999;

    // Build density bounds from warm-up window, then freeze
    if (!frozen)
    {
      warm_d.push_back(d);
      if ((int)warm_d.size() >= 3)
      {
        L = std::max(quantile7(warm_d, 0.05), 1e-6);
        U = std::max(quantile7(warm_d, 0.95), L * 1.0001);
      }
      if (i + 1 >= warmup) frozen = true;
    }

    // Learned selectivity
    double rho = cor_running(Sw, Sv, Swv, Sww, Svv, cnt);
    double rc  = rho < 0 ? 0 : (rho > 1 ? 1 : rho);
    double k   = k_lo + (k_hi - k_lo) * rc;

    // Decision
    bool accept;
    double spread = (U - L) / U;
    if (spread < 1e-3)
    {
      accept = (used + wi <= W);
    }
    else
    {
      double p = L + (U - L) * (z + k * (z - t));
      if (p < L) p = L; else if (p > U) p = U;
      accept = (used + wi <= W) && (d >= p);
    }

    // Observe item (online: allowed), then commit
    Sw += wi; Sv += vi; Swv += wi * vi; Sww += wi * wi; Svv += vi * vi; ++cnt;
    if (accept)
    {
      used  += wi;
      value += vi;
      sel.push_back(i + 1);   // 1-based
    }
  }

  return List::create(_["value"]       = value,
                      _["selected"]    = wrap(sel),
                      _["used_weight"] = used);
}
