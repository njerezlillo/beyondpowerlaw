
# Beyond the Power Law Repository 🚀

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](./LICENSE)
![Lifecycle: experimental](https://img.shields.io/badge/Lifecycle-Experimental-steelblue)
[![ArXiv](https://img.shields.io/badge/ArXiv-2311.11200-red)](https://arxiv.org/abs/2311.11200)
<!-- badges: end -->

This repository provides users with the tools and methods implemented in the study:

> [**Beyond the Power Law: Estimation, Goodness-of-Fit, and a Semiparametric Extension in Complex Networks**](https://arxiv.org/abs/2311.11200).  
*Jerez-Lillo N., Rodrigues F. A., Ferreira P. H., Ramos P. L.*  
arXiv preprint arXiv:2311.11200 

This repository includes functions for parameter estimation, goodness-of-fit testing, and the implementation of a semiparametric extension by adding change points. This repository facilitate the reproduction of results.

## Packages

This repository is based on two packages located on GitHub repository:

- [pldis](https://github.com/njerezlillo/pldis): This package offers a collection of tools for fitting the discrete power-law model, utilizing both frequentist and Bayesian approaches for parameter estimation.
- [pwpldis](https://github.com/njerezlillo/pwpldis): This package provides tools for fitting the piecewise discrete power-law model, a flexible statistical framework for modeling data that exhibits power-law behavior in different segments.

## Content

The content is divided in 

- [altdistr.R](./altdistr.R): Implement the probability mass function and generator of psudosamples based on the inverse function of the following discrete models: Poisson, Exponential, Yule, and Log-Normal. All of the models have been truncated to include a lower bound (see Clauset *et al.* 2009 for details). 
- [clauset.R](./clauset.R): Import the functions to fitted and evalute the goodness-of-fit in the discrete power-law model see (Clauset *et al.* 2009). Often these metholodogies have referenced in the manuscript as "traditional methologies".
- [application](./application): Contains all the datasets and codes used in the application section. They are divided in:

  <details>
  <summary> Click to expand </summary>

  [ICON](./application/icon)

  [MobyDick](./application/mobydick)

  </details>  
  
- [output](./output): Contains all the codes to export the results obtained in the simulation study section.

- [simulation](./simulation): Contains all the codes to export the results obtained in the simulation study section. They contain:

  <details>
  <summary> Click to expand </summary>

  [results](./application/results)

  [R Codes](./application/icon/code_icon.R): blablabla

  </details>  

## References  

[**Power-law distributions in empirical data**](https://doi.org/10.1137/070710111)  
*Clauset A., Shalizi C. R., Newman M. E. J.*  
SIAM Review, 51(4), 661–703 (2009)
