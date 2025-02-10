
# Beyond the Power Law Repository 🚀

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](./LICENSE)
![Lifecycle: experimental](https://img.shields.io/badge/Lifecycle-Experimental-steelblue)
[![ArXiv](https://img.shields.io/badge/ArXiv-2311.11200-red)](https://arxiv.org/abs/2311.11200)
<!-- badges: end -->

This repository provides tools and methods developed in the study:

> [**Beyond the Power Law: Estimation, Goodness-of-Fit, and a Semiparametric Extension in Complex Networks**](https://arxiv.org/abs/2311.11200).  
*Jerez-Lillo N., Rodrigues F. A., Ferreira P. H., Ramos P. L.*  
arXiv preprint arXiv:2311.11200 

It includes functions for parameter estimation, goodness-of-fit testing, and a semiparametric extension incorporating change points. The repository is designed to facilitate the reproduction of results.

## Dependencies

This repository relies on two GitHub packages:

- [pldis](https://github.com/njerezlillo/pldis): Provides tools for estimating the scaling parameter and lower bound of the discrete power-law model using both frequentist and Bayesian methods.
- [pwpldis](https://github.com/njerezlillo/pwpldis): Implements the piecewise discrete power-law model, allowing for segmented power-law behavior.

## Repository Structure

### Scripts

- [altdistr.R](./altdistr.R): Implements probability mass functions and inverse transform sampling for alternative discrete models (Poisson, Exponential, Yule, Log-Normal). All models have been truncated to include a lower bound (see Clauset *et al.* 2009 for details). 
- [clauset.R](./clauset.R): Imports functions for fitting and evaluating the goodness-of-fit of the discrete power-law model using the methods of Clauset *et al.* (2009). Often referenced as "traditional methods". [(reference)](https://aaronclauset.github.io/powerlaws/)

### Applications

Located in the [application](./application) directory, these subfolders contain datasets and scripts used in the application section of the manuscript:

- [ICON](./application/icon) (Section 6.1):
  
  <details>
  <summary> Click to expand </summary>
  - `code_icon.R`: Containt the codes used in the 6.1 section of the manuscript.
  - `data`: Containt the degree sequences analyzed [(reference)](https://github.com/adbroido/SFAnalysis)
  - `run`: Containts 10 R code to apply the traditional methodologies to all the degree sequences.
  - `results`: Containts the results of to apply the last 10 R codes.
  - `tbl_results.R`: Create a object with all the information, including average degree, etc.
  </details>  
  
- [MobyDick](./application/mobydick) (Section 6.2):
  <details>
  <summary> Click to expand </summary>
  - `book.txt`: Novel of Mobydick in `txt` format.
  - `code_mobydick.R`: Containt the codes used in the 6.2 section of the manuscript.
  - `frequency_words.txt`: Frequent of words   [(reference)](https://aaronclauset.github.io/powerlaws/data.htm)
  </details>  

### Output

Located in the [output](./output) directory, contains scripts to generate the tables and figures in the simulation studies. The name of the files reference their utility.

### Simulation

Located in the [simulation](./simulation) directory, includes scripts to run the simulation studies.

- [`results`](./simulation/results): Stores RData files with simulation results.
- [.RData](./simulation/icon/code_icon.R): Scripts used to perform simulations and analyze results.

## References  

[**Power-law distributions in empirical data**](https://doi.org/10.1137/070710111)  
*Clauset A., Shalizi C. R., Newman M. E. J.*  
SIAM Review, 51(4), 661–703 (2009)
