# ==========================================
# DSC531 - SEMESTER PROJECT
# Amazon Data Science Books Dataset
# ==========================================


# ==========================================
# 1. DATA UNDERSTANDING
# ==========================================

# 1.1 Load the original dataset
books <- read.csv(
  "data/Amazon_Data_Science_Books.csv",
  stringsAsFactors = FALSE
)

# 1.2 Check the dimensions of the dataset
print(dim(books))

# 1.3 Display the first six observations
print(head(books))

# 1.4 Examine the structure and data types
str(books)

# 1.5 Display summary statistics
print(summary(books))
print()

# 1.6 Check missing values (NA)
print(colSums(is.na(books)))

# 1.7 Check blank strings in character variables
character_vars <- names(books)[
  sapply(books, is.character)
]

blank_counts <- sapply(
  books[, character_vars, drop = FALSE],
  function(x) sum(!is.na(x) & trimws(x) == "")
)

print(blank_counts)

# 1.8 Check the total number of observations
print(nrow(books))

# 1.9 Check the total number of variables
print(ncol(books))


# ==========================================
# 2. DATA CLEANING
# ==========================================

# 2.1 Create a copy of the original dataset
# Keep the original dataset unchanged

books_clean <- books


# 2.2 Convert blank strings to NA
# Blank strings represent missing information

books_clean$weight[
  !is.na(books_clean$weight) &
    trimws(books_clean$weight) == ""
] <- NA

books_clean$language[
  !is.na(books_clean$language) &
    trimws(books_clean$language) == ""
] <- NA


# 2.3 Check missing values after conversion

print(colSums(is.na(books_clean)))


# 2.4 Count exact duplicate observations

n_duplicates <- sum(duplicated(books_clean))

print(n_duplicates)


# 2.5 Remove exact duplicate observations
# Keep the first occurrence of each duplicated row

books_clean <- books_clean[
  !duplicated(books_clean), ,
  drop = FALSE
]

# Reset row numbers
rownames(books_clean) <- NULL


# 2.6 Verify the cleaned dataset

print(dim(books_clean))

print(sum(duplicated(books_clean)))


# 2.7 Inspect weight column to find the units of measurement
# Extract measurement units from the weight column
weight_units <- gsub("[0-9. ]", "", books_clean$weight)

# Count observations for each weight unit
print(table(weight_units, useNA = "ifany"))


# 2.8 Convert weight to kilograms
# Extract the numeric part of the weight
weight_numeric <- as.numeric(
  sub(" .*", "", books_clean$weight)
)

# Convert ounces and pounds to kilograms
books_clean$weight_kg <- ifelse(
  weight_units == "ounces",
  weight_numeric * 0.028349523125,
  ifelse(
    weight_units == "pounds",
    weight_numeric * 0.45359237,
    NA_real_
  )
)

# Check the first six original and converted weights
print(head(books_clean[, c("weight", "weight_kg")]))

# Check summary statistics
print(summary(books_clean$weight_kg))

# Check missing values
print(sum(is.na(books_clean$weight_kg)))


# 2.9 Investigate extreme weight values

# Display the 10 books with the lowest weights
print(
  head(
    books_clean[
      order(books_clean$weight_kg),
      c("price", "pages", "weight", "weight_kg")
    ],
    10
  )
)

# Display the 10 books with the highest weights
print(
  head(
    books_clean[
      order(books_clean$weight_kg, decreasing = TRUE),
      c("price", "pages", "weight", "weight_kg")
    ],
    10
  )
)


# 2.10 Detect weight outliers using the IQR method

# Calculate the first and third quartiles
Q1 <- quantile(books_clean$weight_kg, 0.25, na.rm = TRUE)
Q3 <- quantile(books_clean$weight_kg, 0.75, na.rm = TRUE)

# Calculate the interquartile range
IQR_weight <- Q3 - Q1

# Calculate the lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR_weight
upper_bound <- Q3 + 1.5 * IQR_weight

# Display the calculated values
print("1st quartile:")
print(Q1)
print("3rd quartile:")
print(Q3)
print("IQR:")
print(IQR_weight)
print("Lower bound:")
print(lower_bound)
print("Upper bound:")
print(upper_bound)

# Identify weight outliers
weight_outliers <- books_clean[
  !is.na(books_clean$weight_kg) &
    (books_clean$weight_kg < lower_bound |
      books_clean$weight_kg > upper_bound),
]

# Count weight outliers
print(nrow(weight_outliers))

# Display detected outliers
print("Detected weight outliers:")
print(weight_outliers[, c(
  "price", "pages",
  "weight", "weight_kg"
)])


# 2.11 Investigate potentially implausible weights

# Calculate grams per page
books_clean$g_per_page <- (
  books_clean$weight_kg * 1000
) / books_clean$pages

# Display summary statistics
print(summary(books_clean$g_per_page))

# Exploratory thresholds for suspicious values
# These are diagnostic thresholds, not proven error limits
lower_gpp <- 0.3
upper_gpp <- 10

# Flag potentially suspicious observations
implausible_weight <- !is.na(books_clean$g_per_page) &
  (books_clean$g_per_page < lower_gpp |
    books_clean$g_per_page > upper_gpp)

# Count flagged observations
print(sum(implausible_weight))

# Inspect flagged observations
print(
  books_clean[
    implausible_weight,
    c(
      "price", "pages", "weight",
      "weight_kg", "g_per_page"
    )
  ]
)

# Keep the original weights until further investigation
# Do not automatically replace flagged values with NA
# Remove the temporary diagnostic variable
books_clean$g_per_page <- NULL

# Verify the dataset dimensions
print(dim(books_clean))


# 2.12 Detect outliers in other numeric variables (IQR method)

# Function that returns the number of IQR outliers of a variable
count_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  sum(x < q1 - 1.5 * iqr | x > q3 + 1.5 * iqr, na.rm = TRUE)
}

# Apply the function to the main numeric variables
outlier_vars <- c("price", "pages", "avg_reviews", "n_reviews", "weight_kg")
print(sapply(books_clean[, outlier_vars], count_outliers))


# 2.13 Consistency checks for review variables

star_vars <- c("star5", "star4", "star3", "star2", "star1")

# Books without reviews have undefined star proportions
books_clean[books_clean$n_reviews == 0, star_vars] <- NA

# Check star proportions only for books with reviews
reviewed_books <- books_clean$n_reviews > 0

star_sum <- rowSums(
  books_clean[reviewed_books, star_vars]
)

# Summary of star proportions
print(summary(star_sum))

# Count inconsistencies (tolerance: 0.05)
print(sum(abs(star_sum - 1) > 0.05, na.rm = TRUE))

# Check average rating range
print(range(books_clean$avg_reviews, na.rm = TRUE))

# Check invalid price and page values
print(sum(books_clean$price <= 0, na.rm = TRUE))
print(sum(books_clean$pages <= 0, na.rm = TRUE))

# Count books without reviews
print(sum(books_clean$n_reviews == 0))


# 2.14 Convert language to a factor
books_clean$language <- factor(books_clean$language)
print(table(books_clean$language, useNA = "ifany"))
# Almost all books are in English, so language is not useful
# for group comparisons.


# 2.15 Final check of the cleaned dataset
# Missing values are kept; na.rm = TRUE is used in the calculations
# so that no books are removed unnecessarily.
print(colSums(is.na(books_clean)))
str(books_clean)


# 2.16 Estimate typical grams per page

# Calculate grams per page
g_per_page <- books_clean$weight_kg * 1000 / books_clean$pages

# Flag potentially implausible values
suspicious_weight <- !is.na(g_per_page) &
  (g_per_page < 0.3 | g_per_page > 10)

# Use only observations with available, non-suspicious ratios
valid_gpp <- g_per_page[
  !is.na(g_per_page) & !suspicious_weight
]

# Calculate the mean and median
print(mean(valid_gpp))
print(median(valid_gpp))

# Check how much the values vary
print(sd(valid_gpp))
print(summary(valid_gpp))


# 2.17 Compare weight statistics with and without suspicious values

# Create a temporary copy of the weight variable
weight_without_suspicious <- books_clean$weight_kg

# Exclude suspicious weights from the comparison only
weight_without_suspicious[suspicious_weight] <- NA

# Compare descriptive statistics
comparison <- data.frame(
  Statistic = c("Mean", "Median", "SD"),
  Original = c(
    mean(books_clean$weight_kg, na.rm = TRUE),
    median(books_clean$weight_kg, na.rm = TRUE),
    sd(books_clean$weight_kg, na.rm = TRUE)
  ),
  Without_Suspicious = c(
    mean(weight_without_suspicious, na.rm = TRUE),
    median(weight_without_suspicious, na.rm = TRUE),
    sd(weight_without_suspicious, na.rm = TRUE)
  )
)

# Display the comparison
print(comparison)


# 2.18 Create a separate weight variable for analysis

# Preserve original weight measurements
books_clean$weight_analysis <- books_clean$weight_kg

# Mark suspicious measurements as missing for analysis
books_clean$weight_analysis[suspicious_weight] <- NA

# Check the number of excluded measurements
print(sum(is.na(books_clean$weight_analysis)))

# Compare available observations
print(sum(!is.na(books_clean$weight_kg)))
print(sum(!is.na(books_clean$weight_analysis)))


# 2.19 Compare correlations before and after excluding suspicious weights

# Correlation using original weight measurements
cor_original <- cor(
  books_clean$pages,
  books_clean$weight_kg,
  use = "complete.obs"
)

# Correlation excluding suspicious weight measurements
cor_filtered <- cor(
  books_clean$pages,
  books_clean$weight_analysis,
  use = "complete.obs"
)

# Display results
print(cor_original)
print(cor_filtered)


# 2.20 Final feature selection and export

# Remove language because it has very low variability
# The language distribution was already examined in Step 2.14
books_clean$language <- NULL

# Remove the original text weight variable
# The converted weight_kg variable is retained
books_clean$weight <- NULL

# Check final dimensions and variables
print(dim(books_clean))
print(names(books_clean))

# Save the final cleaned dataset
write.csv(
  books_clean,
  "data/Amazon_Data_Science_Books_clean.csv",
  row.names = FALSE
)

# Verify the exported dataset
books_check <- read.csv(
  "data/Amazon_Data_Science_Books_clean.csv"
)

print(dim(books_check))
print(names(books_check))


# ==========================================
# 3. SUMMARY STATISTICS
# ==========================================

# 3.1 Select the numeric variables
# Use weight_analysis for the filtered weight summary

num_vars <- c(
  "price", "pages", "avg_reviews", "n_reviews",
  "star5", "star4", "star3", "star2", "star1",
  "weight_analysis"
)


# 3.2 Define a function to calculate skewness
# Positive: right-skewed distribution
# Negative: left-skewed distribution
# Close to zero: approximately symmetric

skewness <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) < 3 || sd(x) == 0) {
    return(NA_real_)
  }

  mean((x - mean(x))^3) / sd(x)^3
}


# 3.3 Define a function for descriptive statistics

describe <- function(x) {
  c(
    n = sum(!is.na(x)),
    missing = sum(is.na(x)),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    variance = var(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    Q1 = unname(quantile(x, 0.25, na.rm = TRUE)),
    Q3 = unname(quantile(x, 0.75, na.rm = TRUE)),
    max = max(x, na.rm = TRUE),
    range = diff(range(x, na.rm = TRUE)),
    IQR = IQR(x, na.rm = TRUE),
    CV = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE),
    skewness = skewness(x)
  )
}


# 3.4 Create the descriptive statistics table

summary_table <- t(
  sapply(books_clean[, num_vars, drop = FALSE], describe)
)

print(round(summary_table, 3))
png("plots/3_4_weight_outliers.png",
    width = 8, height = 6,
    units = "in", res = 150)

boxplot(
  books_clean$weight_kg,
  main = "Outlier Detection in Book Weight",
  ylab = "Book Weight (kg)",
  col = "lightblue",
  border = "darkblue",
  pch = 16
)

dev.off()

# 3.5 Compare original and filtered weight statistics

weight_comparison <- rbind(
  weight_kg = describe(books_clean$weight_kg),
  weight_analysis = describe(books_clean$weight_analysis)
)

print(round(weight_comparison, 3))


# 3.6 Check missing values in the final dataset

print(colSums(is.na(books_clean)))


# 3.7 Verify the final dataset dimensions

print(dim(books_clean))

# Step 3.8 - Calculate Coefficient of Variation (%)

cv_percent <- function(x) {
  x <- x[!is.na(x)]
  100 * sd(x) / mean(x)
}

sapply(
  books_clean[c("price", "pages", "avg_reviews",
                "n_reviews", "weight_analysis")],
  cv_percent
)


# ==========================================
# 4. DATA VISUALIZATION
# ==========================================

# Create a folder for the plots
dir.create("plots", showWarnings = FALSE)


# 4.1 Histograms of the main numeric variables

main_vars <- c(
  "price",
  "pages",
  "avg_reviews",
  "n_reviews",
  "weight_analysis"
)

png(
  "plots/4_1_histograms.png",
  width = 12,
  height = 8,
  units = "in",
  res = 150
)

# Arrange plots in 2 rows and 3 columns
par(
  mfrow = c(2, 3),
  cex.main = 1.3,
  cex.lab = 1.2,
  cex.axis = 1.0
)

# Create a histogram for each variable
for (v in main_vars) {
  hist(
    books_clean[[v]],
    main = paste("Histogram of", v),
    xlab = v,
    col = "lightblue",
    border = "white",
    breaks = 30
  )
}

# Reset graphical parameters
par(mfrow = c(1, 1))

# Save and close the image file
dev.off()


# Observations:
# - Price is right-skewed, with a few expensive books.
# - Pages is right-skewed, with some very large books.
# - Average ratings are left-skewed, with most ratings
#   concentrated between 4 and 5.
# - Number of reviews is extremely right-skewed.
# - Filtered weight is right-skewed, but extreme
#   suspicious measurements have been excluded.


# 4.2 Boxplots of the main numeric variables

png(
  "plots/4_2_boxplots.png",
  width = 12,
  height = 8,
  units = "in",
  res = 150
)

# Arrange plots in 2 rows and 3 columns
par(
  mfrow = c(2, 3),
  cex.main = 1.3,
  cex.lab = 1.2,
  cex.axis = 1.0
)

# Create a boxplot for each variable
for (v in main_vars) {
  boxplot(
    books_clean[[v]],
    main = paste("Boxplot of", v),
    ylab = v,
    col = "lightblue",
    border = "darkblue",
    pch = 16,
    cex = 0.6
  )
}

# Reset graphical parameters
par(mfrow = c(1, 1))

# Save and close the image file
dev.off()


# Observations:
# - Price contains several high-value outliers.
# - Pages contains books with unusually high page counts.
# - Average ratings contain some unusually low values.
# - Number of reviews has many extreme high values.
# - Filtered weight still contains some outliers.
# - Outliers are not automatically removed.


# 4.3 Histogram of log-transformed number of reviews

# Apply log(1 + x) to handle zero reviews
log_reviews <- log1p(books_clean$n_reviews)

# Save the histogram
png(
  "plots/4_3_log_reviews.png",
  width = 9,
  height = 6,
  units = "in",
  res = 150
)

hist(
  log_reviews,
  breaks = 30,
  main = "Histogram of Log-Transformed Reviews",
  xlab = "log(1 + Number of Reviews)",
  ylab = "Frequency",
  col = "lightblue",
  border = "white"
)

dev.off()


# Compare skewness before and after transformation
print(skewness(books_clean$n_reviews))
print(skewness(log_reviews))


# Observation:
# The logarithmic transformation reduces the influence
# of very large review counts and makes the distribution
# easier to visualize.



# 4.4 Bar plot of average star rating proportions

# Select the star rating variables
star_vars <- c(
  "star5", "star4", "star3", "star2", "star1"
)

# Calculate the mean proportion for each rating
# Books without reviews are excluded using na.rm = TRUE
star_means <- colMeans(
  books_clean[, star_vars],
  na.rm = TRUE
)

# Display the mean proportions
print(round(star_means, 3))

# Save the bar plot
png(
  "plots/4_4_star_ratings.png",
  width = 9,
  height = 6,
  units = "in",
  res = 150
)

barplot(
  star_means,
  names.arg = c("5 Stars", "4 Stars", "3 Stars",
                "2 Stars", "1 Star"),
  main = "Average Star Rating Proportions",
  xlab = "Star Rating",
  ylab = "Mean Proportion",
  col = "lightblue",
  border = "darkblue",
  ylim = c(0, 0.8)
)

dev.off()


# Observation:
# Five-star ratings account for the largest average
# proportion of reviews across reviewed books.
# Books without reviews are excluded from the calculation.



# 4.5 Scatterplot: Pages vs Price

# Calculate Pearson correlation
cor_pages_price <- cor(
  books_clean$pages,
  books_clean$price,
  use = "complete.obs"
)

print(cor_pages_price)

# Fit a simple linear regression model
model_pages_price <- lm(
  price ~ pages,
  data = books_clean
)

# Save the scatterplot
png(
  "plots/4_5_pages_vs_price.png",
  width = 9,
  height = 6,
  units = "in",
  res = 150
)

plot(
  books_clean$pages,
  books_clean$price,
  main = "Scatterplot: Pages vs Price",
  xlab = "Number of Pages",
  ylab = "Price",
  col = "steelblue",
  pch = 16,
  cex = 0.6
)

# Add the regression line
abline(
  model_pages_price,
  col = "red",
  lwd = 2
)

dev.off()


# Observation:
# The scatterplot is used to examine whether
# books with more pages tend to have higher prices.
# The correlation coefficient measures the strength
# and direction of the linear relationship.


# 4.6 Scatterplots: Pages vs Weight
# Compare original and filtered weight measurements

png(
  "plots/4_6_pages_vs_weight.png",
  width = 12,
  height = 6,
  units = "in",
  res = 150
)

par(mfrow = c(1, 2))

# Original weight measurements
plot(
  books_clean$pages,
  books_clean$weight_kg,
  main = "Pages vs Original Weight",
  xlab = "Number of Pages",
  ylab = "Weight (kg)",
  col = "steelblue",
  pch = 16,
  cex = 0.6
)

# Filtered weight measurements
plot(
  books_clean$pages,
  books_clean$weight_analysis,
  main = "Pages vs Filtered Weight",
  xlab = "Number of Pages",
  ylab = "Weight (kg)",
  col = "darkgreen",
  pch = 16,
  cex = 0.6
)

par(mfrow = c(1, 1))

dev.off()

# Compare Pearson correlations
print(cor(
  books_clean$pages,
  books_clean$weight_kg,
  use = "complete.obs"
))

print(cor(
  books_clean$pages,
  books_clean$weight_analysis,
  use = "complete.obs"
))


# Observation:
# Compare the original and filtered relationships.
# Filtering based on grams per page may itself
# influence the observed correlation.



# 4.7 Scatterplot: Average Rating vs Log(Number of Reviews)

# Calculate log-transformed number of reviews
log_reviews <- log1p(books_clean$n_reviews)

# Calculate Pearson correlation
cor_reviews_rating <- cor(
  log_reviews,
  books_clean$avg_reviews,
  use = "complete.obs"
)

print(cor_reviews_rating)

# Fit a simple linear regression model
model_reviews_rating <- lm(
  avg_reviews ~ log_reviews,
  data = books_clean
)

# Save the scatterplot
png(
  "plots/4_7_reviews_vs_rating.png",
  width = 9,
  height = 6,
  units = "in",
  res = 150
)

plot(
  log_reviews,
  books_clean$avg_reviews,
  main = "Average Rating vs Log(Number of Reviews)",
  xlab = "log(1 + Number of Reviews)",
  ylab = "Average Rating",
  col = "darkorange",
  pch = 16,
  cex = 0.6
)

# Add the regression line
abline(
  model_reviews_rating,
  col = "red",
  lwd = 2
)

dev.off()


# Observation:
# This plot examines whether books with more reviews
# tend to have higher or lower average ratings.
# Correlation does not imply causation.
