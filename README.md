# Thesis Study Code and Data Repository

Welcome to the code and data repository for the thesis study titled "Coming out of their shells: Repeatable social preferences in Green sea turtles, Chelonia mydas". This repository contains the necessary scripts and data files to replicate the results presented in the thesis. The aim of this repository is to provide transparency and reproducibility. 

## Code

### Script: block_matrix.R
**Description:** This script calculates the distribution across blocks of each individual, including attribute data.

### Script: group_size.R
**Description:** This script extracts the size of groups and their frequency from the database and generates corresponding plots. Refer to Figure 4 in the thesis for visualization.

### Script: basic_models.R
**Description:** This script contains metadata, descriptive statistics, and basic models. The results presented in Figure 3 of the thesis.

### Script: edgelist_1
**Description:** This script complies the edge list of an inputted encounter database, to record assocations using thr Gambit of the Group approach.

### Script: MCMC_glmm_models.R
**Description:** This script utilizes the MCMCglmm package to understand variations in degree based on life-stage and sex attributes. The results are presented in Table 2.

### Script: calculate_sri.R
**Description:** This script calculates the Simple Ratio Index (SRI) using edge lists.

### Script: permute_blocks.R
**Description:** This script performs permutations to test the repeatability of social preferences. The results are visualized in Figure 5.


## Data

### individuals_apr_jun.csv
This CSV file contains individual-level data for the months of April to June. It serves as the primary dataset for the analysis performed in the scripts.


## Contact Information

If you have any questions, or need further assistance in replicating the results, please feel free to contact the author of the thesis, Amy Feakes, at [amy.feakes222@imperial.ac.uk].
