# Unveiling Early Responses of Mouse β-Cells to High Fat Diet feeding through Single-Cell Multiome Analysis

## Phenotyping of mice

To investigate early adaptive changes to short-term HFD, we fed eleven-week-old male C57BL/6JBomTac mice with a HFD (60% energy from fat) or purified low-fat diet (LFD) (10% energy from fat) as a control, for one and three weeks.

To capture the early genomic changes within islets of Langerhans following short-term HFD exposure, we utilized a single-nucleus method known as chromium single-cell multiome ATAC + Gene expression from 10X Genomics (snMultiome) using nuclei from isolated islets. snMultiome enables simultaneous capture of gene expression data and chromatin accessibility data from the same single nuclei through RNA-sequencing (snRNA-seq) and transposase-accessible chromatin with sequencing (snATAC-seq), respectively.

![Illustration of work](https://github.com/Isabellvse/phenotyping_multiome/blob/main/illustrations/github_image.png)

###### *A) Schematic overview of the experimental setup: 8-week-old male C57BL/6JBomTac mice underwent a three-week acclimatization period during which they were fed a low-fat-diet (LFD). Subsequently, mice were divided into groups receiving either LFD (orange, as a control) or high-fat-diet (blue, HFD) for one or three weeks.* 
###### *B) Macronutrient composition of LFD (7 % energy from sucrose 10 % energy from fat) and HFD (7 % energy from sucrose, 60 % energy from fat) presented in % Kcal.* Following one or three weeks of diet the metabolic phenotype of these mice was characterized on several parameters

# Brief description of folder

The following folders contain:

-   `R/`: This folder contains scripts for quality control and analysis performed in R
-   `illustrations/`: Illustrations used for this readme file

# Description of scripts

-   `R/`: *r_library.txt*
    -   `weight.R`: Calculation of weight gain from week 0 
    -   `GTT.R`: Analysis of Intraperitoneal glucose tolerance test 
    -   `ITT.R`: Analysis of Intraperitoneal insulin tolerance test
    -   `fasted_glucose.R`: Fasted blood glucose levels.
    -   `beta_cell_function.R`: Quantification of insulin levels and beta-cell function

#### Other scripts

-   `R/`:
    -   `Package_load.R`: to load packages
