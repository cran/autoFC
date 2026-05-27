# autoFC: Automatic tools for Forced-Choice Scale Assembly, Optimization, Scoring and Simulation
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/autoFC)](https://cran.r-project.org/package=autoFC)

## Overview

autoFC is a comprehensive, fully automatic R package designed to support the entire lifecycle of forced-choice (FC) scale development. 
It provides robust tools for automated test assembly, advanced psychometric scoring (using R, Mplus, or Stan), 
and data simulation based on the Thurstonian IRT model (Brown & Maydeu-Olivares, 2011).

## What's New in the May 2026 Major Update
This major update introduces critical performance, usability, and mathematical enhancements. These features are added to allow for
much easier and convenient use of FC scales, than ever before.

- Thurstonian Factor Model (TFM) and Thurstonian IRT (IRT) Support: Native syntax generation and scoring for highly stable first-order and second-order models 
in lavaan and Mplus, completely bypassing tedious manual constraint declarations.

- Vectorized, C-Level Algorithmic Speedups: Restructured simulated annealing energy calculators and agreement metrics (bp.coeff.raw, gwet.ac1.raw) to achieve significant performance improvements.

- High-Performance Stan Integration: A unified Stan pipeline featuring within-chain parallel computing (reduce_sum) and automatic logical dependency reduction (Heister, Doebler, & Frick, 2025) to easily scale to larger blocks.

- Blazing Fast Scoring: Fast, vectorized MAP scoring in R using analytical gradients to score participants and calculate Standard Errors in seconds (R and Mplus models).


## Installation

You can install autoFC from CRAN:

``` r
install.packages("autoFC")
```

You can install the development version of autoFC from GitHub:

``` r
devtools::install_github("mtlpsych/autoFC")
```

## Core Functions

**autoFC** divies the FC workflow into several logical stages:

### 1. Scale Assembly and Optimization

**create_blueprint_template()**: A starter function building up csv templates for users to fill in their own blueprint.

**build_blueprint_blocks()**: A block-by-block initializer that constructs starting scales matching the exact trait/sign
blueprints that user specifies, while also allowing for optimizing item characteristics.

**optimize_blocks()** (Previously known as **sa_pairing_generalized()**): The CORE optimization engine based on the 
Simulated Annealing (SA) algorithm. It is capable of balancing within-block matching on 1 or more criteria (local fit) and overall
trait pair distributions (global fit), while preventing within-block trait overlap.

### 2. Data Preparation and Diagnostics

**convert_ranks_to_pairwise(), convert_mole_to_pairwise()**: Converts raw fully ranked responses or "Most-Least Like Me" (MOLE) partial rankings
into binary pairwise columns, handling unobserved comparisons using NAs.

**summarize_trait_pairs()**: Provides a tabular diagnostic summary of how evenly equally-keyed and mixed-keyed trait pairs are distributed
across all trait combinations in your FC scale.

### 3. Syntax Generation

**generate_tirt_lavaan_syntax(), generate_tirt_mplus_syntax(), generate_tirt_stan_syntax()**: For R and Mplus, generates syntax for either
First-Order or Second-Order Thurstonian IRT models, also natively supporting character trait names and automated Heywood-case prevention (i.e., negative residual variances).
For Stan, generates a unified, pre-compiled Stan model with multithread capabilities and logical dependency reduction.

### 4. Scoring and Estimation
All three functions below now allow score estimation for not only original calibration sample, **BUT ALSO for new samples**.

**score_tirt_lavaan()**: Fast MAP scoring and Standard Error (Yes, now available) estimation in R for lavaan models 
estimated from the model produced by **generate_tirt_lavaan_syntax()**.
Also supports both first- and second-order parameterizations.

**score_tirt_mplus()**: Fast parameter extraction from Mplus .out files and participant scoring using R, bypassing Mplus's FSCORES engine.

**score_tirt_stan()**: Used in conjunction with **prepare_tirt_stan_data()**. Based on the modern features in cmdstan package, 
now it allows very fast scoring and prediction in Stan (at least 10x faster than stan prediction methods in earlier versions).
In the case of Stan, use **predict_tirt_stan()** for predicting trait scores of new samples.

**empirical_reliability()**: Calculates the test-level empirical reliability of estimated trait scores (Brown & Maydeu-Olivares, 2018).

### 5. Parameter Extraction & Response Simulation

**get_CFA_estimates()**: Fits CFA model and/or extracts item parameters from raw Likert data or pre-fitted lavaan objects.

**get_simulation_matrices()**: Simulates latent traits, item parameters, and continuous utilities for Thurstonian IRT modeling, based on fitted CFA models.

**plot_scores()**: Visual diagnostics for comparing true vs. estimated scores, complete with reference lines.

**RMSE_range()**: Evaluates trait recovery accuracy across specified intervals of the latent continuum.



## References
Brown, A., & Maydeu-Olivares, A. (2018). Ordinal factor analysis of graded-preference questionnaire data. *Structural Equation Modeling: A Multidisciplinary Journal, 25*(4), 516-529. https://doi.org/10.1080/10705511.2017.1392247

Li, M., Zhang, B., Li, L., Sun, T., & Brown, A. (2025). Mixed-keying or desirability-matching in the construction of forced-choice measures? An empirical investigation and practical recommendations. *Organizational Research Methods, 28*(2), 296-329. https://doi.org/10.1177/10944281241229784


