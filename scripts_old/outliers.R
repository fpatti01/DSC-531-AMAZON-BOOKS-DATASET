books <- read.csv("data/Amazon_Data_Science_Books.csv")

sum_columns_na = sum(is.na(books))
sum_rows_na = sum(rowSums(is.na(books)) > 0)

missing_per_row <- rowSums(is.na(books))

# Rows with more than 1 NA
which(missing_per_row > 1 )

# Number of such rows
sum(missing_per_row > 1)

# Display those rows
books[missing_per_row > 1, ]


for (i in 1:nrow(books)) {

  missing_count <- sum(
    is.na(books[i, ]) |
    trimws(as.character(books[i, ])) == "",
    na.rm = TRUE
  )

  if (missing_count > 1) {
    cat("Row", i, "has", missing_count,
        "missing values.\n")
  }
}
books[c(207, 508, 570, 821, 823, 941), ]


for (i in 1:nrow(books)) {

  missing <- is.na(books[i, ]) |
             trimws(as.character(books[i, ])) == ""

  missing[is.na(missing)] <- TRUE

  if (sum(missing) > 1) {
    cat("Row", i, "has", sum(missing), "missing values:",
        names(books)[missing], "\n")
  }
}


books$calculated_rating <- 5 * books$star5 +
                           4 * books$star4 +
                           3 * books$star3 +
                           2 * books$star2 +
                           1 * books$star1

books$rating_difference <- abs(
  books$avg_reviews - books$calculated_rating
)

# Books with reviews and a difference greater than 0.2
suspicious_ratings <- books[
  books$n_reviews > 0 &
  !is.na(books$rating_difference) &
  books$rating_difference > 0.2,
]

suspicious_ratings



# Number of books with different levels of discrepancy
sum(books$rating_difference > 0.2, na.rm = TRUE)
sum(books$rating_difference > 0.5, na.rm = TRUE)
sum(books$rating_difference > 1.0, na.rm = TRUE)

# Show the 10 largest discrepancies
head(
  books[order(-books$rating_difference),
        c("avg_reviews", "n_reviews",
          "calculated_rating", "rating_difference")],
  10
)




books$rating_issue <- !is.na(books$rating_difference) &
                      books$rating_difference > 0.2

books$review_group <- cut(
  books$n_reviews,
  breaks = c(-1, 0, 5, 20, 100, Inf),
  labels = c("0", "1-5", "6-20", "21-100", "100+")
)

# Number of inconsistencies by review group
table(books$review_group, books$rating_issue)

books_clean <- read.csv("data/Amazon_Data_Science_Books_modified.csv")
books_clean$calculated_rating <-
  5 * books_clean$star5 +
  4 * books_clean$star4 +
  3 * books_clean$star3 +
  2 * books_clean$star2 +
  books_clean$star1

books_clean$rating_difference <- abs(
  books_clean$avg_reviews - books_clean$calculated_rating
)

valid <- !is.na(books_clean$rating_difference)

sum(valid)
sum(valid & books_clean$rating_difference > 0.2)




cor(
  books_clean$price,
  books_clean$pages,
  use = "complete.obs",
  method = "pearson"
)

cor(
  log1p(books_clean$price),
  books_clean$pages,
  use = "complete.obs",
  method = "pearson"
)



# Original dataset
cat("Original rows:", nrow(books), "\n")

# Duplicates before cleaning
cat("Duplicates BEFORE cleaning:",
    sum(duplicated(books)), "\n")

# Duplicates after blank-to-NA conversion
test_clean <- books

for (col in names(test_clean)[sapply(test_clean, is.character)]) {
  x <- test_clean[[col]]
  x[!is.na(x) & trimws(x) == ""] <- NA
  test_clean[[col]] <- x
}

cat("Duplicates AFTER cleaning:",
    sum(duplicated(test_clean)), "\n")

# Identify all rows involved in duplicate groups
duplicate_rows <- which(
  duplicated(test_clean) |
  duplicated(test_clean, fromLast = TRUE)
)

print(duplicate_rows)
print(test_clean[duplicate_rows, , drop = FALSE])