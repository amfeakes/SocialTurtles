# Coming out of their shells: Repeatable social preferences in Green sea turtles (Chelonia mydas)

## Authors

Amy Feakes <sup>1,2,3</sup>, Julian Gervolino<sup>1</sup>, Isha Afeef<sup>1,4</sup>, Stephanie Köhnk <sup>1</sup>, Jamie Dunning* <sup>2,5</sup>, Emma Cavan* <sup>2</sup>

<sup>1</sup>Olive Ridley Project, 91 Padiham Road, Sabden, Clitheroe, Lancashire, BB7 9EX, UK  
<sup>2</sup>Department of Life Sciences, Imperial College London, Silwood Park Campus, Ascot, United Kingdom  
<sup>3</sup>School of Ocean and Earth Science, University of Southampton, National Oceanography Center Southampton, University Way, Southampton, SO14 3ZH, UK  
<sup>4</sup>Olive Ridley Project Maldives, Kaneerumaage, Dhonhuraa Goalhi, K. Malé, 20037, Maldives  
<sup>5</sup>Faculty of Biological Sciences, University of Leeds, Leeds, United Kingdom  


*Joint last authors  
*<sup>3</sup> & <sup>5</sup> are new host institutions of authors since the study was first started.

---

## Repository Overview

Welcome to the code and data repository for the study titled *"Coming out of their shells: Repeatable social preferences in Green sea turtles, Chelonia mydas."* This repository provides the necessary scripts and data to replicate the results presented in the paper. Our goal is to ensure transparency and reproducibility for all analyses discussed in the study.

---

## Code

### Scripts:
- **block_matrix.R**  
  *Description*: This script calculates the distribution across blocks of each individual, including attribute data.

- **group_size.R**  
  *Description*: Extracts the size of groups and their frequency from the dataset and generates corresponding plots. Refer to Figure 4 in the manuscript for visualization.

- **basic_models.R**  
  *Description*: Contains metadata, descriptive statistics, and basic models. The results presented in Figure 3 of the manuscript.

- **edgelist_1.R**  
  *Description*: Compiles the edge list of an inputted encounter database to record associations using the Gambit of the Group approach.

- **MCMC_glmm_models.R**  
  *Description*: Utilizes the MCMCglmm package to analyze variations in degree based on life-stage and sex attributes. The results are presented in Table 2.

- **calculate_sri.R**  
  *Description*: Calculates the Simple Ratio Index (SRI) using edge lists.

- **permute_blocks.R**  
  *Description*: Performs permutations to test the repeatability of social preferences. The results are visualized in Figure 5.

---

## Data

### Data Files:
- **individuals_apr_jun.csv**  
  This CSV file contains individual-level data for the months of April to June. It serves as the primary dataset for the analysis performed in the scripts.

---

## Contact Information

If you have any questions or require further assistance in replicating the results, please feel free to contact the corresponding author:

- Amy Feakes: [amy.feakes@oliveridleyproject.org](mailto:amy.feakes@oliveridleyproject.org)  
