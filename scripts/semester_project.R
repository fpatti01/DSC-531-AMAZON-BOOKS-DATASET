# DSC531 - Semester Project
# Amazon Data Science Books

# ==========================================
# 1. DATA UNDERSTANDING
# ==========================================

# 1.1 Load the dataset
books <- read.csv("data/Amazon_Data_Science_Books.csv")

# 1.2 Check the dimensions
print(dim(books))

# 1.3 Display the first six rows
print(head(books))

# 1.4 Check the structure of the dataset
str(books)

# 1.5 Summary statistics
print(summary(books))

# 1.6 Check missing values
print(colSums(is.na(books)))


# ==========================================
# 2. DATA CLEANING
# ==========================================

# 2.1 Create a copy of the original dataset
books_clean <- books

# 2.2 Convert blank strings to NA
books_clean$weight[
  trimws(books_clean$weight) == ""
] <- NA

books_clean$language[
  trimws(books_clean$language) == ""
] <- NA

# 2.3 Check missing values after conversion
print(colSums(is.na(books_clean)))

# 2.4 Count exact duplicate observations
print(sum(duplicated(books_clean)))

# 2.5 Remove exact duplicates
books_clean <- books_clean[!duplicated(books_clean), ]

# 2.6 Verify the cleaned dataset
print(dim(books_clean))
print(sum(duplicated(books_clean)))

#2.7 Inspect weight column to find the units of measurement
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
    books_clean[order(books_clean$weight_kg),
                c("price", "pages", "weight", "weight_kg")],
    10
  )
)

# Display the 10 books with the highest weights
print(
  head(
    books_clean[order(books_clean$weight_kg, decreasing = TRUE),
                c("price", "pages", "weight", "weight_kg")],
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
print(weight_outliers[, c("price", "pages",
                          "weight", "weight_kg")])


# 2.11 Detect implausible weights using weight per page
# Weight depends on the number of pages, so a heavy book is not
# necessarily an error. Instead, we check the weight per page
# (grams per page). Books with an extreme ratio are considered
# data errors (e.g. wrong unit or a bundle of books).

# Calculate grams per page
books_clean$g_per_page <- books_clean$weight_kg * 1000 / books_clean$pages

print(summary(books_clean$g_per_page))

# Bounds using 3 * IQR (extreme outliers only)
Q1_gpp <- quantile(books_clean$g_per_page, 0.25, na.rm = TRUE)
Q3_gpp <- quantile(books_clean$g_per_page, 0.75, na.rm = TRUE)
IQR_gpp <- Q3_gpp - Q1_gpp

lower_gpp <- Q1_gpp - 3 * IQR_gpp
upper_gpp <- Q3_gpp + 3 * IQR_gpp

print(c(lower = lower_gpp, upper = upper_gpp))

# Identify books with implausible weight
implausible_weight <- !is.na(books_clean$g_per_page) &
  (books_clean$g_per_page < lower_gpp |
     books_clean$g_per_page > upper_gpp)

print(sum(implausible_weight))

# Display these books before changing anything
print(books_clean[implausible_weight,
                  c("price", "pages", "weight", "weight_kg", "g_per_page")])

# Set their weight to NA (the rest of the row is kept, because
# price, pages and reviews are still valid)
books_clean$weight_kg[implausible_weight] <- NA

# Remove the helper variable
books_clean$g_per_page <- NULL

# Check the weight after the correction
print(summary(books_clean$weight_kg))



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



# 2.13 Consistency checks

# Star proportions should sum to approximately 1
star_sum <- rowSums(books_clean[, c("star5", "star4", "star3",
                                    "star2", "star1")])
print(summary(star_sum))

# Number of books whose star proportions do not sum to 1 (±0.05)
print(sum(abs(star_sum - 1) > 0.05, na.rm = TRUE))

# Average rating should be between 1 and 5
print(range(books_clean$avg_reviews, na.rm = TRUE))

# Price and pages should be positive
print(sum(books_clean$price <= 0, na.rm = TRUE))
print(sum(books_clean$pages <= 0, na.rm = TRUE))

# Books with zero reviews
print(sum(books_clean$n_reviews == 0, na.rm = TRUE))


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

# 2.16 Save the cleaned dataset as a new CSV file
# The original weight column (text) is removed because
# weight_kg replaces it.
books_export <- books_clean[, setdiff(names(books_clean), "weight")]

write.csv(books_export,
          "data/Amazon_Data_Science_Books_clean.csv",
          row.names = FALSE)

# Verify that the file was saved correctly
books_check <- read.csv("data/Amazon_Data_Science_Books_clean.csv")
print(dim(books_check))
str(books_check)


# ==========================================
# 3. SUMMARY STATISTICS
# ==========================================

# 3.1 Select the numeric variables
num_vars <- c("price", "pages", "avg_reviews", "n_reviews",
              "star5", "star4", "star3", "star2", "star1", "weight_kg")

# 3.2 Function for skewness
# Positive: long right tail, negative: long left tail, ~0: symmetric
skewness <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^3) / sd(x)^3
}

# 3.3 Function that calculates the descriptive statistics of a variable
describe <- function(x) {
  c(n        = sum(!is.na(x)),
    missing  = sum(is.na(x)),
    mean     = mean(x, na.rm = TRUE),
    median   = median(x, na.rm = TRUE),
    sd       = sd(x, na.rm = TRUE),
    variance = var(x, na.rm = TRUE),
    min      = min(x, na.rm = TRUE),
    Q1       = unname(quantile(x, 0.25, na.rm = TRUE)),
    Q3       = unname(quantile(x, 0.75, na.rm = TRUE)),
    max      = max(x, na.rm = TRUE),
    range    = diff(range(x, na.rm = TRUE)),
    IQR      = IQR(x, na.rm = TRUE),
    CV       = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE),
    skewness = skewness(x))
}

# 3.4 Table of descriptive statistics (one row per variable)
summary_table <- t(sapply(books_clean[, num_vars], describe))
print(round(summary_table, 3))

# 3.5 Frequency tables for the categorical information
print(table(books_clean$language, useNA = "ifany"))
print(table(weight_units, useNA = "ifany"))


# ==========================================
# 4. DATA VISUALIZATION
# ==========================================

# Create a folder for the plots (if it does not exist)
dir.create("plots", showWarnings = FALSE)

# 4.1 Histograms of the main numeric variables
main_vars <- c("price", "pages", "avg_reviews", "n_reviews", "weight_kg")

png("plots/4_1_histograms.png", width = 1200, height = 800)
par(mfrow = c(2, 3))   # 2 rows x 3 columns of plots
for (v in main_vars) {
  hist(books_clean[[v]], main = paste("Histogram of", v),
       xlab = v, col = "lightblue", border = "white")
}
par(mfrow = c(1, 1))   # reset the layout
dev.off()

# 4.2 Boxplots of the main numeric variables
png("plots/4_2_boxplots.png", width = 1200, height = 800)
par(mfrow = c(2, 3))
for (v in main_vars) {
  boxplot(books_clean[[v]], main = paste("Boxplot of", v),
          ylab = v, col = "lightgreen")
}
par(mfrow = c(1, 1))
dev.off() 


# 4.3 Log-transformed number of reviews (very right-skewed variable)
png("plots/4_3_log_reviews.png", width = 800, height = 600)
hist(log1p(books_clean$n_reviews),
     main = "Histogram of log(1 + n_reviews)",
     xlab = "log(1 + n_reviews)", col = "lightblue", border = "white")

dev.off()

# 4.4 Average distribution of star ratings across books
png("plots/4_4_star_ratings.png", width = 800, height = 600)
star_means <- colMeans(books_clean[, c("star1", "star2", "star3",
                                       "star4", "star5")], na.rm = TRUE)
barplot(star_means, main = "Average proportion of each star rating",
        ylab = "Proportion", col = "orange")
dev.off()

# 4.5 Scatterplots between pairs of variables
png("plots/4_5_scatterplots.png", width = 1500, height = 500)
par(mfrow = c(1, 3))
plot(books_clean$pages, books_clean$price,
     main = "Price vs Pages", xlab = "Pages", ylab = "Price ($)")
plot(books_clean$pages, books_clean$weight_kg,
     main = "Weight vs Pages", xlab = "Pages", ylab = "Weight (kg)")
plot(log1p(books_clean$n_reviews), books_clean$avg_reviews,
     main = "Rating vs log(Reviews)",
     xlab = "log(1 + n_reviews)", ylab = "Average rating")
par(mfrow = c(1, 1))
dev.off()


# Observations:
# - Weight has a strong positive linear relationship with pages.
# - Price increases with pages, but the relationship is weaker
#   (large spread of prices for the same number of pages).
# - Most ratings are between 4 and 5. Low ratings appear only in
#   books with very few reviews; ratings stabilise around 4.5
#   as the number of reviews increases.
