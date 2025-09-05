library(scoringTools)
library(caret)
library(dplyr)
library(MASS)

generate_data_MAR_ws <- function(n, d) {
  # n: number of observations
  # d: number of dimensions (x attributes)
  
  y <- rbinom(n, size = 1, prob = 0.5)
  X <- matrix(0, nrow = n, ncol = d)
  X[y == 0,] <- mvrnorm(n = sum(y==0), mu = rep(0,d), Sigma = diag(d)*2)
  X[y == 1,] <- mvrnorm(n = sum(y==1), mu = rep(1,d), Sigma = diag(d)*2)
  
  # Step 3: Create dataframe with proper column names
  df <- data.frame(X)
  colnames(df) <- paste0("x.", 1:d)
  df$y <- y
  return(df)
}

coff <- 0.3
d <- 1
df <- generate_data_MAR_ws(10000, d)

lr.fit <- glm(y ~ .-y,
             data = df, family = binomial(logit))
lr.pred <- predict.glm(lr.fit, newdata = df, type="response")

overcut <- lr.pred > coff
fdf <- df[overcut, ]   # predictions > coff
nfdf <- df[!overcut, ] # predictions <= coff
# Remove column y from nfdf
nfdf <- nfdf[, !names(nfdf) %in% "y"]
# Split fdf into two dataframes
fdf_y <- fdf[, "y", drop = FALSE]  # Only y column (keep as dataframe)
fdf_x <- fdf[, !names(fdf) %in% "y"]  # All columns except y
nfdfm <- as.matrix(nfdf)
fdfym <- as.vector(fdf_y$y)
fdf_x <- as.matrix(fdf_x)
fdfxm <- as.data.frame(fdf_x)
colnames(fdfxm) <- paste0("x.1")

df_lr <- glm(fdfym ~ ., data = fdfxm, family = binomial(logit))
df_rec <- reclassification(fdf_x, nfdfm, fdfym, 0.5)

x <- seq(-4.999, 5, 0.001)
x_as_df <- as.data.frame(x)
colnames(x_as_df) <- paste0("x.1")

# data to plot are from lr.pred (oracle), df_lr (financed), df_rec (reclassification)
#df_sorted <- arrange(df, x.1)

lr_res <- predict.glm(lr.fit, newdata = x_as_df, type="response", family = binomial(logit))
lr_f_res <- scoringTools::predict(df_lr, x_as_df, type = "response")
rec_res <- scoringTools::predict(df_rec, x_as_df, type = "response")

plot(x, xlab="Feature x",
     ylab="p(1|x)", xlim=c(-2, 4), ylim=c(0, 1))
lines(x, lr_res, col="blue", lty=2, lwd=2)

lines(x, lr_f_res, col="orange", lty=3, lwd=2)

lines(x, rec_res, col="red", lwd=2)

legend(-2, 1, legend=c("Oracle", "Financed", "Reclassification"),
       fill=c("blue", "orange", "red"))