library(scoringTools)
library(caret)
library(mclust)
library(MASS)

#Total runtime : 1h 10 min

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

df_list <- list()
lr_list <- list()
for (i in 1:20){
  df <- generate_data_MAR_ws(n = 10000, d = 8)
  df_list[[i]] <- df
  lr.fit <- glm(y ~ .-y,
                data = df, family = binomial(logit))
  lr.pred <- predict.glm(lr.fit, newdata = df, type="response")
  lr_list[[i]] <- lr.pred
}
dftest <- generate_data_MAR_ws(n = 100000, d = 8)
x_max <- 0.9
j_max <- x_max/0.02
gini_avg <- rep(0, j_max)
i_max <- length(df_list)
ginis <- rep(0, i_max)

# first cycle: parcelling
probs <- seq(0, 1, 0.1)
for (j in 1:j_max){
  coff <- 0.02*j
  for (i in 1:i_max){
    df <- df_list[[i]]
    lr.pred <- lr_list[[i]]
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
    fdfxm <- as.matrix(fdf_x)
    df_par <- parcelling(fdfxm, nfdfm, fdfym, probs, rep(1.15, length(probs) - 1))
    testy <- c(dftest[, "y", drop = FALSE])
    predvals <- scoringTools::predict(df_par, dftest, type = "response")
    ginis[i] <- normalizedGini(testy$y, predvals)
  }
  gini_avg[j] <- mean(ginis)
}
plot(c(1:j_max)*0.02, gini_avg, col="magenta", pch=8, xlab="Cut-off Value",
     ylab="Gini on test set", xlim=c(0, 0.9), ylim=c(0.77, 0.85))
lines(c(1:j_max)*0.02, gini_avg, col="magenta")


# second cycle: augmentation
for (j in 1:j_max){
  coff <- 0.02*j
  for (i in 1:i_max){
    df <- df_list[[i]]
    lr.pred <- lr_list[[i]]
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
    fdfxm <- as.matrix(fdf_x)
    df_aug <- augmentation(fdfxm, nfdfm, fdfym)
    testy <- c(dftest[, "y", drop = FALSE])
    predvals <- scoringTools::predict(df_aug, dftest, type = "response")
    ginis[i] <- normalizedGini(testy$y, predvals)
  }
  gini_avg[j] <- mean(ginis)
}
points(c(1:j_max)*0.02, gini_avg, pch=17, col="green")
lines(c(1:j_max)*0.02, gini_avg, col="green")

# third cycle: reclassification
for (j in 1:j_max){
  coff <- 0.02*j
  for (i in 1:i_max){
    df <- df_list[[i]]
    lr.pred <- lr_list[[i]]
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
    fdfxm <- as.matrix(fdf_x)
    df_rec <- reclassification(fdfxm, nfdfm, fdfym, 0.5)
    testy <- c(dftest[, "y", drop = FALSE])
    predvals <- scoringTools::predict(df_rec, dftest, type = "response")
    ginis[i] <- normalizedGini(testy$y, predvals)
  }
  gini_avg[j] <- mean(ginis)
}
points(c(1:j_max)*0.02, gini_avg, pch=7, col="red")
lines(c(1:j_max)*0.02, gini_avg, col="red")

# fourth cycle: logistic regression
for (j in 1:j_max){
  coff <- 0.02*j
  for (i in 1:i_max){
    df <- df_list[[i]]
    lr.pred <- lr_list[[i]]
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
    fdfxm <- as.matrix(fdf_x)
    df_lr <- glm(fdfym ~ x.1+x.2+x.3+x.4+x.5+x.6+x.7+x.8,
                       data = as.data.frame(fdfxm), family = binomial(logit))
    testy <- c(dftest[, "y", drop = FALSE])
    predvals <- scoringTools::predict(df_lr, dftest, type = "response")
    ginis[i] <- normalizedGini(testy$y, predvals)
  }
  gini_avg[j] <- mean(ginis)
}
points(c(1:j_max)*0.02, gini_avg, pch=15, col="black")
lines(c(1:j_max)*0.02, gini_avg, col="black")

# fifth cycle: gaussian mixture
for (j in 1:j_max){
  coff <- 0.02*j
  for (i in 1:i_max){
    df <- df_list[[i]]
    lr.pred <- lr_list[[i]]
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
    fdfxm <- as.matrix(fdf_x)
    X_test <- as.matrix(dftest[, !names(dftest) %in% "y"])
    testy <- c(dftest[, "y", drop = FALSE])
    X_all <- rbind(fdfxm, nfdfm)
    y_all <- factor(c(fdfym, rep(NA, nrow(nfdfm))))
    mclust_supervised <- MclustSSC(X_all, class=y_all)
    predvals <- predict(mclust_supervised, X_test)
    ginis[i] <- normalizedGini(testy$y, predvals$z[,2])
  }
  gini_avg[j] <- mean(ginis)
}
points(c(1:j_max)*0.02, gini_avg, pch=9, col="grey")
lines(c(1:j_max)*0.02, gini_avg, col="grey")

legend(0.1, 0.82, legend=c("Logistic Regression", "Augmentation", 
                           "Reclassification", "Parcelling", "Gaussian Mixture"),
       fill=c("black", "green", "red", "magenta", "grey"))