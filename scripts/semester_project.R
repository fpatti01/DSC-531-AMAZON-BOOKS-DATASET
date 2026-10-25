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
print('Summary statistics:')
print(summary(books))
print()

# 1.6 Check missing values
print('Missing values:')  
print(colSums(is.na(books)))
print()


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




# 2.8 Convert weight to pounds

# Extract the numeric part of the weight
weight_numeric <- as.numeric(
  sub(" .*", "", books_clean$weight)
)

# Create a new variable with all weights in pounds

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


