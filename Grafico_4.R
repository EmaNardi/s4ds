library(scoringTools)
library(caret)
library(VGAM)
library(nnet)
library(readxl)

# https://archive.ics.uci.edu/dataset/350/default+of+credit+card+clients

# As shown by the plot of frac_fdf_avg, that is the fraction of records for each cut-off
# value that are included in the fdf, the fraction of non-financed records (rejection rate)
# is above 50% up until 0.15 of cut-off value circa.

#                             Preprocessing

default_credit_card <- read_xls("Desktop/Script Progetto/default of credit card clients.xls")
colnames(default_credit_card) <- paste0(default_credit_card[1,])
colnames(default_credit_card)[25] <- "default"
default_credit_card <- default_credit_card[-1,]

for (e in colnames(default_credit_card)){
  default_credit_card[[e]] <- as.integer(as.character(default_credit_card[[e]]))
}

default_credit_card[default_credit_card$EDUCATION > 3,][4] <- 3
default_credit_card[default_credit_card$EDUCATION < 1,][4] <- 3
default_credit_card[default_credit_card$PAY_0 > 5,][7] <- 5
default_credit_card[default_credit_card$PAY_0 < -1,][7] <- -1
default_credit_card[default_credit_card$PAY_2 > 5,][8] <- 5
default_credit_card[default_credit_card$PAY_2 < -1,][8] <- -1
default_credit_card[default_credit_card$PAY_2 ==1 ,][8] <- 2
default_credit_card[default_credit_card$PAY_3 > 5,][9] <- 5
default_credit_card[default_credit_card$PAY_3 < -1,][9] <- -1
default_credit_card[default_credit_card$PAY_3 == 1,][9] <- 2
default_credit_card[default_credit_card$PAY_4 > 5,][10] <- 5
default_credit_card[default_credit_card$PAY_4 < -1,][10] <- -1
default_credit_card[default_credit_card$PAY_4 == 1,][10] <- 2
default_credit_card[default_credit_card$PAY_5 > 5,][11] <- 5
default_credit_card[default_credit_card$PAY_5 < -1,][11] <- -1
default_credit_card[default_credit_card$PAY_6 > 5,][12] <- 5
default_credit_card[default_credit_card$PAY_6 < -1,][12] <- -1
default_credit_card[default_credit_card$MARRIAGE > 3,][5] <- 3
default_credit_card[default_credit_card$MARRIAGE < 1,][5] <- 3
default_credit_card[default_credit_card$SEX > 2,][3] <- 2
default_credit_card[default_credit_card$SEX < 1,][3] <- 1

df_tot <- default_credit_card[,-1]

factors <- c("SEX", "EDUCATION", "MARRIAGE", "PAY_0", "PAY_2", "PAY_3", "PAY_4",  "PAY_5",
             "PAY_6")
numerics <- colnames(df_tot)[-c(1, 3, 4, 5)]#, 6, 7, 8, 9, 10, 11)]
for (e in numerics){
  df_tot[[e]] <- as.integer(df_tot[[e]])
}
for (e in factors){
  df_tot[[e]] <- as.factor(as.character(df_tot[[e]]))
}

df_tot$LIMIT_BAL <- as.numeric(as.double(df_tot$LIMIT_BAL))
#df_tot$LIMIT_BAL <- as.numeric(as.integer(df_tot$LIMIT_BAL))
#df_tot$LIMIT_BAL <- as.integer(cut(df_tot$LIMIT_BAL, breaks=c(0,50000,140000,240000,1000001), 
#                    labels=c("10000-50000","50000-140000","140000-240000", "240000-1000000")))
df_tot$AGE <- as.numeric(as.integer(df_tot$AGE))
df_tot$AGE <- as.integer(cut(df_tot$AGE, breaks=c(0,28,34,41,80), 
                    labels=c("21-28","28-34","34-41", "41-79")))
df_tot$BILL_AMT1 <- as.integer(cut(df_tot$BILL_AMT1, breaks=c(-165581,3559,22382,67091,964512), 
                    labels=c("-165580-3559","3559-22382","22382-67091", "67091-964512")))
df_tot$BILL_AMT2 <- as.integer(cut(df_tot$BILL_AMT2, breaks=c(-70000,2985,21200,64006,1000000), 
                    labels=c("-69777-2985","2985-21200","21200-64006", "64006-983931")))
df_tot$BILL_AMT3 <- as.integer(cut(df_tot$BILL_AMT3, breaks=c(-157270,2666,20088,60165,1664100), 
                    labels=c("-157264-2666","2666-20088","20088-60165", "60165-1664089")))
df_tot$BILL_AMT4 <- as.integer(cut(df_tot$BILL_AMT4, breaks=c(-170001,2327,19052,54506,1664089), 
                    labels=c("-170001-2327","2327-19052","19052-54506", "54506-891586")))
df_tot$BILL_AMT5 <- as.integer(cut(df_tot$BILL_AMT5, breaks=c(-165581,1763,18104,50190,927172), 
                    labels=c("-81334-1763","1763-18104","18104-50190", "50190-927171")))
df_tot$BILL_AMT6 <- as.integer(cut(df_tot$BILL_AMT6, breaks=c(-700000,1256,17071,49198,961664), 
                    labels=c("-339603-1256","1256-17071","17071-49198", "49198-961664")))
df_tot$PAY_AMT1 <- as.integer(cut(df_tot$PAY_AMT1, breaks=c(-1,1000,2100,5006,900000), 
                    labels=c("0-1000","1000-2100","2100-5006", "5006-873552")))
df_tot$PAY_AMT2 <- as.integer(cut(df_tot$PAY_AMT2, breaks=c(-1,833,2009,5000,1685000), 
                    labels=c("0-833","833-2009","2009-5000", "5000-1684259")))
df_tot$PAY_AMT3 <- as.integer(cut(df_tot$PAY_AMT3, breaks=c(-1,390,1800,4505,896050), 
                    labels=c("0-390","390-1800","1800-4505", "4505-896040")))
df_tot$PAY_AMT4 <- as.integer(cut(df_tot$PAY_AMT4, breaks=c(-1,296,1500,4013,621001), 
                    labels=c("0-296","296-1500","1500-4013", "4013-621000")))
df_tot$PAY_AMT5 <- as.integer(cut(df_tot$PAY_AMT5, breaks=c(-1,252,1500,4032,426530), 
                    labels=c("0-252","252-1500","1500-4032", "4032-426529")))
df_tot$PAY_AMT6 <- as.integer(cut(df_tot$PAY_AMT6, breaks=c(-1,117,1500,4000,530000), 
                    labels=c("0-117","117-1500","1500-4000", "4000-528666")))

#                             End of Preprocessing


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
  lr.fit <- glm(default ~ .-default, data = df[-df_idx,], family = binomial(logit))
  lr.pred <- predict.glm(lr.fit, newdata = df[-df_idx,], type="response")
  lr_list[[i]] <- lr.pred 
}

coff_step <- 0.02
x_max <- 0.16
j_max <- floor(x_max/coff_step)
#j_max <- 7
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
    nfdf <- nfdf[, !names(nfdf) %in% "default"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "default", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "default"]  # All columns except y
    fdfym <- as.vector(fdf_y$default)
    df_lr <- glm(fdf_y$default ~ ., data=fdf_x, family=binomial(logit))
    testy <- c(df_test[, "default", drop = FALSE])
    predvals <- scoringTools::predict(df_lr, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$default, predvals)
  }
  gini_avg[j+1] <- mean(ginis)
  frac_fdf_avg[j+1] <- mean(frac_fdf)
  #plot(seq(0, 0.9, 0.05), frac_fdf_avg)
}
rejection_rate <- rep(1, length(frac_fdf_avg)) - frac_fdf_avg
plot(rejection_rate, gini_avg, pch=15, col="black", xlab="Rejection Rate",
     ylab="Gini on test set", xlim=c(0, 0.6), ylim=c(0.2, 0.6))
lines(rejection_rate, gini_avg, col="black")

# second cycle: augmentation
for (j in 2:j_max){ #skips the first two steps, they don't cut anything, the augmentation breaks
  coff <- coff_step*j
  for (i in 1:i_max){
    df_test <- df_list[[i]]
    df_train <- df[-df_idx_list[[i]],]
    lr.pred <- lr_list[[i]]
    overcut <- lr.pred > coff
    fdf <- df_train[overcut, ]   # predictions > coff
    nfdf <- df_train[!overcut, ] # predictions <= coff
    # Remove column y from nfdf
    nfdf <- nfdf[, !names(nfdf) %in% "default"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "default", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "default"]  # All columns except y
    fdfym <- as.vector(fdf_y)
    df_aug <- augmentation(fdf_x, nfdf, fdfym$default)
    testy <- c(df_test[, "default", drop = FALSE])
    predvals <- scoringTools::predict(df_aug, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$default, predvals)
  }
  gini_avg[j+1] <- mean(ginis)
}
points(rejection_rate, gini_avg, pch=17, col="green")
lines(rejection_rate, gini_avg, col="green")

# third cycle: reclassification
for (j in 2:j_max){ 
  coff <- coff_step*j
  for (i in 1:i_max){
    df_test <- df_list[[i]]
    df_train <- df[-df_idx_list[[i]],]
    lr.pred <- lr_list[[i]]
    overcut <- lr.pred > coff
    fdf <- df_train[overcut, ]   # predictions > coff
    nfdf <- df_train[!overcut, ] # predictions <= coff
    # Remove column y from nfdf
    nfdf <- nfdf[, !names(nfdf) %in% "default"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "default", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "default"]  # All columns except y
    fdfym <- as.vector(fdf_y)
    df_rec <- reclassification(fdf_x, nfdf, fdfym$default, 0.5) # or 0.02*j ?
    testy <- c(df_test[, "default", drop = FALSE])
    predvals <- scoringTools::predict(df_rec, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$default, predvals)
  }
  gini_avg[j+1] <- mean(ginis)
}
points(rejection_rate, gini_avg, pch=7, col="red")
lines(rejection_rate, gini_avg, col="red")

# fourth cycle: parcelling
probs <- seq(0, 1, 0.1)
for (j in 2:j_max){
  coff <- coff_step*j
  for (i in 1:i_max){
    df_test <- df_list[[i]]
    df_train <- df[-df_idx_list[[i]],]
    lr.pred <- lr_list[[i]]
    overcut <- lr.pred > coff
    fdf <- df_train[overcut, ]   # predictions > coff
    nfdf <- df_train[!overcut, ] # predictions <= coff
    # Remove column y from nfdf
    nfdf <- nfdf[, !names(nfdf) %in% "default"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "default", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "default"]  # All columns except y
    fdfym <- as.vector(fdf_y)
    df_par <- parcelling(fdf_x, nfdf, fdfym$default, probs, rep(1.15, length(probs) - 1))
    testy <- c(df_test[, "default", drop = FALSE])
    predvals <- scoringTools::predict(df_par, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$default, predvals)
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
    nfdf <- nfdf[, !names(nfdf) %in% "default"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "default", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "default"]  # All columns except y
    fdfym <- as.vector(fdf_y$default)
    X_test <- as.data.frame(df_test[, !names(df_test) %in% "default"])
    testy <- c(df_test[, "default", drop = FALSE])
    X_all <- as.data.frame(rbind(fdf_x, nfdf))
    y_all <- factor(c(fdfym, rep(NA, nrow(nfdf))))
    #df_mnm <- multinom(y_all ~ . , data = X_all)
    #predvals <- predict(df_mnm, df_test, type="probs")
    df_mnm <- vglm(y_all ~ . , data = X_all,family=multinomial())
    predvals <- predict(df_mnm, df_test, type="response")
    ginis[i] <- normalizedGini(testy$default, predvals[,2])
  }
  gini_avg[j] <- mean(ginis)
}
points(rejection_rate, gini_avg, pch=9, col="grey")
lines(rejection_rate, gini_avg, col="grey")

legend(0, 0.4, legend=c("Logistic Regression", "Augmentation", 
                        "Reclassification", "Parcelling", "Generative Model"),
       fill=c("black", "green", "red", "magenta", "grey"))