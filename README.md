# sacADos

Projet d'algorithmique (M2) : le problème du sac à dos 0-1 appliqué à la
sélection de variables sous contrainte de budget.

Le package regroupe plusieurs solveurs :

- recherche exhaustive naïve (R)
- programmation dynamique (R et Rcpp)
- heuristique adaptative auto-réglée `knapsack_adaptive` (R et Rcpp)
- borne LP pour mesurer l'écart à l'optimum

Il contient aussi les générateurs d'instances (familles de Pisinger) servant
à comparer les algorithmes.

## Installation

```r
devtools::install_local("sacADos")
```

## Rapport

Le rapport complet (méthodes, simulations, figures) est dans
`inst/report/rapport.Rmd`.
