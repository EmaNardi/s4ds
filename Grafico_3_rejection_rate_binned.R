library(scoringTools)
library(caret)
library(VGAM)
library(nnet)

# https://www.kaggle.com/datasets/urstrulyvikas/lending-club-loan-data-analysis

# As shown by the plot of frac_fdf_avg, that is the fraction of records for each cut-off
# value that are included in the fdf, the fraction of non-financed records (rejection rate)
# is above 50% up until 0.85 of cut-off value circa.

data <- as.data.frame(read.csv("~/Desktop/Script Progetto/loan_data.csv", header=TRUE))

#                             Preprocessing

data[["purpose"]] <- as.factor(as.character(data[["purpose"]]))

# binning - 4 equal bins
# Results are the same if "installment", dti", log.annual.inc", days.with.cr.line", "revol.bal" and
# "revol.util" are binned or not (maybe slightly better if they are binned)
# Binning "int.rate" makes things worse, fico" makes things a lot worse

#data$int.rate <- as.integer(cut(data$int.rate, breaks=c(0.05,0.104,0.122,0.14,0.22), 
#                labels=c("0.05-0.104","0.104-0.122", "0.122-0.14", "0.14-0.22")))
#data$fico <- as.integer(cut(data$fico, breaks=c(610,682,707,737,828), 
#                labels=c("610-682","682-707","707-737", "737-828")))
data$installment <- as.integer(cut(data$installment, breaks=c(15,164,269,433,941), 
                                   labels=c("15-164","164-269","269-433", "433-941")))
data$dti <- as.integer(cut(data$dti, breaks=c(-1,7.2,12.66,18,31), 
                           labels=c("0-7.2","7.2-12.66","12.66-18", "18-30")))
data$log.annual.inc <- as.integer(cut(data$log.annual.inc, breaks=c(7,10.56,10.93,11.291,15), 
                                      labels=c("7-10.56","10.56-10.93", "10.93-11.291", "11.291-15")))
data$days.with.cr.line <- as.integer(cut(data$days.with.cr.line, breaks=c(178,2820,4140,5730,17641), 
                                         labels=c("179-2820","2820-4140","4140-5730", "5730-17640")))
data$revol.bal <- as.integer(cut(data$revol.bal, breaks=c(-1,3187,8596,18250,1207360), 
                                 labels=c("0-3187","3187-8596","8596-18250", "18250-1207359")))
data$revol.util <- as.integer(cut(data$revol.util, breaks=c(-1,22.6,46.3,70.9,120), 
                                  labels=c("0-22.6","22.6-46.3","46.3-70.9", "70.9-120")))

#                             End of Preprocessing


df_tot <- data
df <- df_tot

df_list <- list()
lr_list <- list()
df_idx_list <- list()

n = ceiling(dim(df_tot)[1]/5)
for (i in 1:5){
  if (dim(df_tot)[1] >= n){
    df_idx <- sample(nrow(df_tot), size = n, replace = FALSE, prob = NULL)
  } else{
    df_idx <- sample(nrow(df_tot), size = dim(df_tot)[1], replace = FALSE, prob = NULL)
  }
  df_list[[i]] <- df_tot[df_idx,]
  df_tot <- df_tot[-df_idx,]
  df_idx_list[[i]] <- df_idx
  lr.fit <- glm(credit.policy ~ .-credit.policy, data = df[-df_idx,], family = binomial(logit))
  lr.pred <- predict.glm(lr.fit, newdata = df[-df_idx,], type="response")
  lr_list[[i]] <- lr.pred
}

coff_step <- 0.06
x_max <- 0.9
j_max <- floor(x_max/coff_step)
#j_max <- 9
#coff_step <- x_max/j_max
gini_avg <- rep(0, j_max+1)
i_max <- length(df_list)
ginis <- rep(0, i_max)
frac_fdf <- rep(0, i_max)
frac_fdf_avg <- rep(0, j_max)

# first cycle: logistic regression
for (j in 0:j_max){
  coff <- coff_step*j
  for (i in 1:i_max){
    df_test <- df_list[[i]]
    df_train <- df[-df_idx_list[[i]],]
    lr.pred <- lr_list[[i]]
    overcut <- lr.pred >= coff
    fdf <- df_train[overcut, ]   # predictions > coff
    nfdf <- df_train[!overcut, ] # predictions <= coff
    frac_fdf[i] <- dim(fdf)[1]/(dim(fdf)[1]+dim(nfdf)[1])
    # Remove column y from nfdf
    nfdf <- nfdf[, !names(nfdf) %in% "credit.policy"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "credit.policy", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "credit.policy"]  # All columns except y
    fdfym <- as.vector(fdf_y$credit.policy)
    df_lr <- glm(fdf_y$credit.policy ~ ., data=fdf_x, family=binomial(logit))
    testy <- c(df_test[, "credit.policy", drop = FALSE])
    predvals <- scoringTools::predict(df_lr, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$credit.policy, predvals)
  }
  gini_avg[j+1] <- mean(ginis)
  frac_fdf_avg[j+1] <- mean(frac_fdf)
  #plot(seq(0, 0.9, 0.1), frac_fdf_avg)
}
rejection_rate <- rep(1, length(frac_fdf_avg)) - frac_fdf_avg
plot(rejection_rate, gini_avg, pch=15, col="black", xlab="Rejection Rate",
     ylab="Gini on test set", xlim=c(0, 0.5), ylim=c(0.2, 0.8))
lines(rejection_rate, gini_avg, col="black")

# second cycle: augmentation
for (j in 1:j_max){
  coff <- coff_step*j
  for (i in 1:i_max){
    df_test <- df_list[[i]]
    df_train <- df[-df_idx_list[[i]],]
    lr.pred <- lr_list[[i]]
    overcut <- lr.pred > coff
    fdf <- df_train[overcut, ]   # predictions > coff
    nfdf <- df_train[!overcut, ] # predictions <= coff
    # Remove column y from nfdf
    nfdf <- nfdf[, !names(nfdf) %in% "credit.policy"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "credit.policy", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "credit.policy"]  # All columns except y
    fdfym <- as.vector(fdf_y)
    df_aug <- augmentation(fdf_x, nfdf, fdfym$credit.policy)
    testy <- c(df_test[, "credit.policy", drop = FALSE])
    predvals <- scoringTools::predict(df_aug, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$credit.policy, predvals)
  }
  gini_avg[j+1] <- mean(ginis)
}
points(rejection_rate, gini_avg, pch=17, col="green")
lines(rejection_rate, gini_avg, col="green")

# third cycle: reclassification
for (j in 1:j_max){
  coff <- coff_step*j
  for (i in 1:i_max){
    df_test <- df_list[[i]]
    df_train <- df[-df_idx_list[[i]],]
    lr.pred <- lr_list[[i]]
    overcut <- lr.pred > coff
    fdf <- df_train[overcut, ]   # predictions > coff
    nfdf <- df_train[!overcut, ] # predictions <= coff
    # Remove column y from nfdf
    nfdf <- nfdf[, !names(nfdf) %in% "credit.policy"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "credit.policy", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "credit.policy"]  # All columns except y
    fdfym <- as.vector(fdf_y)
    df_rec <- reclassification(fdf_x, nfdf, fdfym$credit.policy, 0.5) # or 0.02*j ?
    testy <- c(df_test[, "credit.policy", drop = FALSE])
    predvals <- scoringTools::predict(df_rec, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$credit.policy, predvals)
  }
  gini_avg[j+1] <- mean(ginis)
}
points(rejection_rate, gini_avg, pch=7, col="red")
lines(rejection_rate, gini_avg, col="red")

# fourth cycle: parcelling
probs <- seq(0, 1, 0.1)
for (j in 1:j_max){
  coff <- coff_step*j
  for (i in 1:i_max){
    df_test <- df_list[[i]]
    df_train <- df[-df_idx_list[[i]],]
    lr.pred <- lr_list[[i]]
    overcut <- lr.pred > coff
    fdf <- df_train[overcut, ]   # predictions > coff
    nfdf <- df_train[!overcut, ] # predictions <= coff
    # Remove column y from nfdf
    nfdf <- nfdf[, !names(nfdf) %in% "credit.policy"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "credit.policy", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "credit.policy"]  # All columns except y
    fdfym <- as.vector(fdf_y)
    df_par <- parcelling(fdf_x, nfdf, fdfym$credit.policy, probs, rep(1.15, length(probs) - 1))
    testy <- c(df_test[, "credit.policy", drop = FALSE])
    predvals <- scoringTools::predict(df_par, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$credit.policy, predvals)
  }
  gini_avg[j+1] <- mean(ginis)
}
points(rejection_rate, gini_avg, pch=8, col="magenta")
lines(rejection_rate, gini_avg, col="magenta")

# fifth cycle: multinomial generative model
for (j in 0:j_max){
  coff <- coff_step*j
  for (i in 1:i_max){
    df_test <- df_list[[i]]
    df_train <- df[-df_idx_list[[i]],]
    lr.pred <- lr_list[[i]]
    overcut <- lr.pred > coff
    fdf <- df_train[overcut, ]   # predictions > coff
    nfdf <- df_train[!overcut, ] # predictions <= coff
    # Remove column y from nfdf
    nfdf <- nfdf[, !names(nfdf) %in% "credit.policy"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "credit.policy", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "credit.policy"]  # All columns except y
    fdfym <- as.vector(fdf_y$credit.policy)
    X_test <- as.data.frame(df_test[, !names(df_test) %in% "credit.policy"])
    testy <- c(df_test[, "credit.policy", drop = FALSE])
    X_all <- as.data.frame(rbind(fdf_x, nfdf))
    y_all <- factor(c(fdfym, rep(NA, nrow(nfdf))))
    #df_mnm <- multinom(y_all ~ . , data = X_all)
    #predvals <- predict(df_mnm, df_test, type="probs")
    df_mnm <- vglm(y_all ~ . , data = X_all,family=multinomial())
    predvals <- predict(df_mnm, df_test, type="response")
    ginis[i] <- normalizedGini(testy$credit.policy, predvals[,2])
  }
  gini_avg[j] <- mean(ginis)
}
points(rejection_rate, gini_avg, pch=9, col="grey")
lines(rejection_rate, gini_avg, col="grey")

legend(0, 0.5, legend=c("Logistic Regression", "Augmentation", 
                        "Reclassification", "Parcelling", "Generative Model"),
       fill=c("black", "green", "red", "magenta", "grey"))

