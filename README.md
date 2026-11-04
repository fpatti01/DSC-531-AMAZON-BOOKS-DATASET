# DSC-531 – Amazon Data Science Books Dataset

Semester project for **DSC531 – Statistical Simulation and Analysis**, University of Cyprus.

**Authors:** Florentia Pattidou, Valeriia Shirshina, Marios Makrygiannis

## Project overview

This project presents an exploratory statistical analysis of the Amazon Data Science Books dataset (946 books, 11 variables) using R. The analysis covers:

- **Data cleaning:** blank strings converted to missing values, removal of one exact duplicate, conversion of weights from ounces/pounds to kilograms, outlier detection with the IQR method, a weight-per-page plausibility check, and a consistency check between average ratings and star-rating proportions.
- **Descriptive statistics and visualization:** summary statistics, skewness, histograms, boxplots, scatterplots, correlation heatmaps and log transformations.
- **Hypothesis testing:** five hypotheses on price, page count, review volume and ratings, using Pearson and Spearman correlation tests, Welch's t-test and the Wilcoxon rank-sum test, with LOESS and Nadaraya–Watson smoothers for comparison.
- **Regression:** a multiple linear regression model for log price, with VIF and residual diagnostics.

## Repository structure

```
DSC-531-AMAZON-BOOKS-DATASET/
├── data/
│   ├── Amazon_Data_Science_Books.csv         # original dataset (input)
│   └── Amazon_Data_Science_Books_final.csv   # cleaned dataset (created by the Rmd)
├── plots/                                    # all figures (created by the Rmd)
├── reports/
│   ├── DSC531_Statistical_Analysis.pdf       # final report
│   └── main.tex                              # LaTeX source of the report
├── scripts/
│   ├── DSC531_project_complete.Rmd           # full analysis code (final version)
│   ├── DSC531_project_complete.html          # knitted output with all figures
│   └── old/                                  # earlier drafts, kept for reference
├── DSC-531-AMAZON-BOOKS-DATASET.Rproj
└── README.md
```

The final report is `reports/DSC531_Statistical_Analysis.pdf`. All code used in the report is contained in `scripts/DSC531_project_complete.Rmd`; the files in `scripts/old/` are earlier individual drafts whose content has been merged into the final file.

## How to reproduce the analysis

1. Clone the repository and open `DSC-531-AMAZON-BOOKS-DATASET.Rproj` in RStudio.
2. Install the required packages (once):
   ```r
   install.packages(c("tidyverse", "corrplot", "car", "knitr", "rmarkdown"))
   ```
3. Open `scripts/DSC531_project_complete.Rmd` and click **Knit**.

The Rmd file sets the project root as its working directory, so it reads the data from `data/` and saves all figures to `plots/` automatically. The knitted HTML file contains the code, the output and all figures.

The analysis was run with R 4.6.1, tidyverse 2.0.0, corrplot 0.95 and car 3.1.5.

## Main findings

- Page count is the strongest predictor of price (r = 0.486 with log price).
- Books with more reviews tend to be cheaper, and books without reviews have a higher mean price.
- Average ratings are not associated with price; there is no evidence of a quality premium.
- The regression model explains about 29% of the variation in log price.

The full discussion, including limitations, is available in the final report.

## Dataset

The dataset was provided by the course instructor.