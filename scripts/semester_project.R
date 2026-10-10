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
