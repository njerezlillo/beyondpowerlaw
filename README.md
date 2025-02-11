
# Beyond the Power Law Repository 🚀

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](./LICENSE)
![Lifecycle: experimental](https://img.shields.io/badge/Lifecycle-Experimental-steelblue)
[![ArXiv](https://img.shields.io/badge/ArXiv-2311.11200-red)](https://arxiv.org/abs/2311.11200)
<!-- badges: end -->

This repository provides tools and methods developed in the manuscript:

> [**Beyond the Power Law: Estimation, Goodness-of-Fit, and a Semiparametric Extension in Complex Networks**](https://arxiv.org/abs/2311.11200).  
*Jerez-Lillo N., Rodrigues F. A., Ferreira P. H., Ramos P. L.*  
arXiv preprint arXiv:2311.11200 

It includes functions for parameter estimation, goodness-of-fit testing, and a semiparametric extension of the discrete power-law model, facilitating the reproduction of results.

## Dependencies

This repository relies on two GitHub packages:

- [pldis](https://github.com/njerezlillo/pldis): Provides tools for estimating the scaling parameter and lower bound of the discrete power-law model using both frequentist and Bayesian methods.
- [pwpldis](https://github.com/njerezlillo/pwpldis): Provides tools for estimating the change points and the scaling parameters of the discrete piecewise power-law model.

## Repository Structure

### R scripts

- [altdistr.R](./altdistr.R): Implements probability mass functions and inverse transform sampling for alternative discrete models (Poisson, Exponential, Yule, Log-Normal). All models have been truncated to include a lower bound (see Clauset *et al.* 2009 for details). 
- [clauset.R](./clauset.R): Imports functions for fitting and evaluating the goodness-of-fit of the discrete power-law model using the methods of Clauset *et al.* (2009), commonly referred to as "traditional methods" in the manuscript [(reference)](https://aaronclauset.github.io/powerlaws/).

### Folder: application

Located in the [application](./application) directory, this folder contain datasets and scripts used in the application section of the manuscript. Each subfolder corresponds to a specific case study and includes all necessary resources for reproducing the analyses.

- [ICON](./application/icon) (Section 6.1): This subfolder contains scripts and datasets related to the analysis of over 3,669 degree distributions of the Index of Complex Networks (ICON).
  
  <details>
  <summary> Click to expand </summary>
  
  - **code_icon.R**: Contains the R scripts for analyzing degree distributions.
  
  - **data**: Contains the degree sequences analyzed in this study. These datasets were sourced from this [GitHub repository](https://github.com/adbroido/SFAnalysis).
  
  - **run**: Contains 10 R scripts implementing methods for estimating and testing power-law behavior across all degree sequences.
  
  - **results**: Contains the output generated by the 10 R scripts in the `./application/icon/run/` subfolder.
  
  - **tbl_results.R**: Aggregates all results into a structured object, summarizing key statistics such as number of nodes, average degree, etc.
  
  </details>  
  
- [MobyDick](./application/mobydick) (Section 6.2): This subfolder contains resources for analyzing word frequency distributions in the novel of *Moby Dick* as a case study
  
  <details>
  <summary> Click to expand </summary>
  
  - **book.txt**: The full text of *Moby Dick* in plain text format. This is used to create a word cloud of the most frequent words. 
  
  - **code_mobydick.R**: Contains the `R` scripts to analyze word frequency distribution of *Moby Dick*.  
  
  - **frequency_words.txt**: A precomputed dataset listing word frequencies in Moby Dick. This file was sourced from this [website](https://aaronclauset.github.io/powerlaws/data.htm)
  
  </details>  

### Folder: output

Located in the [output](./output) directory, this folder contains scripts for generating tables and figures from the simulation studies. File names indicate their specific study.

### Folder: simulation

Located in the [simulation](./simulation) directory, this folder contains scripts to run the simulation studies.

- [.R](./simulation/icon/code_icon.R): Scripts used to perform the simulation studies.  

- [results](./simulation/results): Folder with `.RData` files with simulation results.

## References  

[**Power-law distributions in empirical data**](https://doi.org/10.1137/070710111)  
*Clauset A., Shalizi C. R., Newman M. E. J.*  
SIAM Review, 51(4), 661–703 (2009)
