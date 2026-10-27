#Plotting the histograms for understanding the variability of the data
par(mfrow = c(3, 2))
hist(books_clean$price, main = "Distribution of Price", xlab = "Price", col = "skyblue")
hist(books_clean$pages, main = "Distribution of Pages", xlab = "Pages", col = "lightgreen")
hist(books_clean$n_reviews, main = "Distribution of reviews", xlab = "Number of reviews", col = "pink")
hist(books_clean$avg_reviews, main = "Distribution of average reviews", xlab = "Average reviews", col = "yellow")
hist(books_clean$weight_kg, main = "Distribution of weight", xlab = "Weight", col = "grey")

#Create log-transformed columns (using + 1 to handle zeros)
books_clean$log_price     <- log(books_clean$price + 1)
books_clean$log_pages     <- log(books_clean$pages + 1)
books_clean$log_n_reviews <- log(books_clean$n_reviews + 1)
books_clean$log_weight    <- log(books_clean$weight_kg + 1)

#Plot the variables against avg_reviews
par(mfrow = c(2, 2))
plot(books_clean$price, books_clean$avg_reviews,
     xlab = "Price", ylab = "avg_reviews",
     main = "Price vs Avg Reviews", col = "darkblue", pch = 16, cex = 0.6)
plot(books_clean$pages, books_clean$avg_reviews,
     xlab = "Pages", ylab = "avg_reviews",
     main = "Pages vs Avg Reviews", col = "darkblue", pch = 16, cex = 0.6)
plot(books_clean$n_reviews, books_clean$avg_reviews,
     xlab = "N_Reviews", ylab = "avg_reviews",
     main = "N_Reviews vs Avg Reviews", col = "darkblue", pch = 16, cex = 0.6)
plot(books_clean$weight_kg, books_clean$avg_reviews,
     xlab = "Weight", ylab = "avg_reviews",
     main = "Weight vs Avg Reviews", col = "darkblue", pch = 16, cex = 0.6)
par(mfrow = c(1,1))

#Plot the log-transformed variables against avg_reviews for comparison
par(mfrow = c(2, 2))
plot(books_clean$log_price, books_clean$avg_reviews,
     xlab = "Log(Price + 1)", ylab = "avg_reviews",
     main = "Log Price vs Avg Reviews", col = "darkblue", pch = 16, cex = 0.6)
plot(books_clean$log_pages, books_clean$avg_reviews,
     xlab = "Log(Pages + 1)", ylab = "avg_reviews",
     main = "Log Pages vs Avg Reviews", col = "darkblue", pch = 16, cex = 0.6)
plot(books_clean$log_n_reviews, books_clean$avg_reviews,
     xlab = "Log(N_Reviews + 1)", ylab = "avg_reviews",
     main = "Log N_Reviews vs Avg Reviews", col = "darkblue", pch = 16, cex = 0.6)
plot(books_clean$log_weight, books_clean$avg_reviews,
     xlab = "Log(Weight + 1)", ylab = "avg_reviews",
     main = "Log Weight vs Avg Reviews", col = "darkblue", pch = 16, cex = 0.6)
par(mfrow = c(1,1))

library(corrplot)
#Select only numeric columns for correlation
numeric_vars <- books_clean[, c("price", "pages", "n_reviews", "avg_reviews", "weight_kg")]

#Calculate the correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")
#Draw the basic heatmap
corrplot(cor_matrix, 
         method = "color",      # Uses color blocks
         type = "upper",        # Shows the upper triangle to avoid duplication
         addCoef.col = "black", # Prints the correlation coefficients inside the boxes
         number.cex = 0.7,      # Size of the text numbers
         tl.col = "black",      # Text label color
         tl.srt = 45,           # Rotates text labels 45 degrees for readability
         main = "Correlation Matrix of Book Variables",
         mar = c(0, 0, 2, 0))