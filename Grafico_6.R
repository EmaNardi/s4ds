library(scoringTools)
library(caret)
library(VGAM)
library(nnet)

#https://www.openml.org/search?type=data&id=43454&sort=runs&status=active

# As shown by the plot of frac_fdf_avg, that is the fraction of records for each cut-off
# value that are included in the fdf, the fraction of non-financed records (rejection rate)
# is above 50% up until 0.15 of cut-off value circa.

data <- as.data.frame(read.csv("~/Desktop/Script Progetto/dataset.csv", header=FALSE))

#                             Preprocessing

# Most of this data removed is because of redundancies (e.g. state and state code), the others
# because most of the data was missing
colnames(data) <- c("age", "annualincome", "homeownership", "employmentlength", "loanintent",
                    "loangrade", "loanamount", "loanintrate", "loanstatus", "loanpctincome",
                    "historicaldefault", "creditlength")

factors <- colnames(data)[c(3, 5, 6, 11, 9)]
for (e in factors){
  data[[e]] <- as.factor(as.character(data[[e]]))
}
numerics <- colnames(data)[-c(3, 5, 6, 11, 9)]
for (e in numerics){
  data[[e]] <- as.numeric(as.character(data[[e]]))
}

data <- data[!is.na(data$loanintrate),]
data <- data[!is.na(data$employmentlength),]


df_tot <- data

df_tot$age <- as.integer(cut(df_tot$age, breaks=c(0,23,26,30,150),
                                      labels=c("20-23","23-26","26-30", ">30")))
df_tot$annualincome <- as.integer(cut(df_tot$annualincome, breaks=c(0,39480,55956,80000,7000000),
                                      labels=c("4000-39480","39480-55956","55956-80000", ">80000")))
df_tot$employmentlength <- as.integer(cut(df_tot$employmentlength, breaks=c(-1,2,4,7,125),
                                      labels=c("0-2","2-4","4-7", ">7")))
df_tot$loanamount <- as.integer(cut(df_tot$loanamount, breaks=c(0,5000,8000,12500,35000),
                                      labels=c("500-5000","5000-8000","8000-12500", ">12500")))
df_tot$loanintrate <- as.integer(cut(df_tot$loanintrate, breaks=c(0,7.9,10.99,13.48,24),
                                      labels=c("5.42-7.9","7.9-10.99","10.99-13.48", ">13.48")))
df_tot$loanpctincome <- as.integer(cut(df_tot$loanpctincome, breaks=c(-1,0.09,0.15,0.23,0.84),
                                      labels=c("0-0.09","0.09-0.15","0.15-0.23", ">0.23")))
df_tot$creditlength <- as.integer(cut(df_tot$creditlength, breaks=c(0,3,4,8,31),
                                      labels=c("2-3","3-4","4-8", ">8")))

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
  lr.fit <- glm(loanstatus ~ .-loanstatus, data = df[-df_idx,], family = binomial(logit))
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
frac_fdf_avg <- rep(0, j_max+1)

# first cycle: logistic regression
for (j in 0:j_max){
  coff <- coff_step*j
  for (i in 1:i_max){
    df_test <- df_list[[i]]
    df_train <- df[-df_idx_list[[i]],]
    lr.pred <- lr_list[[i]]
    overcut <- lr.pred > coff
    fdf <- df_train[overcut, ]   # predictions > coff
    nfdf <- df_train[!overcut, ] # predictions <= coff
    frac_fdf[i] <- dim(fdf)[1]/(dim(fdf)[1]+dim(nfdf)[1])
    # Remove column y from nfdf
    nfdf <- nfdf[, !names(nfdf) %in% "loanstatus"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "loanstatus", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "loanstatus"]  # All columns except y
    fdfym <- as.vector(fdf_y$loanstatus)
    df_lr <- glm(fdf_y$loanstatus ~ ., data=fdf_x, family=binomial(logit))
    testy <- c(df_test[, "loanstatus", drop = FALSE])
    testy$loanstatus <- as.numeric(testy$loanstatus) # otherwise normalizedGini raises an error,
                                                     # but doing as.numeric earlier raises errors
                                                     # in the logistic regression
    predvals <- scoringTools::predict(df_lr, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$loanstatus-1, predvals)
  }
  gini_avg[j+1] <- mean(ginis)
  frac_fdf_avg[j+1] <- mean(frac_fdf)
  plot(seq(0, x_max, coff_step), frac_fdf_avg)
}
rejection_rate <- rep(1, length(frac_fdf_avg)) - frac_fdf_avg
plot(rejection_rate, gini_avg, pch=15, col="black", xlab="Rejection Rate",
     ylab="Gini on test set", xlim=c(0, 0.6), ylim=c(0.3, 0.71))
lines(rejection_rate, gini_avg, col="black")

# second cycle: augmentation
for (j in 1:j_max){ #skips the first step, it doesn't cut anything, the augmentation breaks
  coff <- coff_step*j
  for (i in 1:i_max){
    df_test <- df_list[[i]]
    df_train <- df[-df_idx_list[[i]],]
    lr.pred <- lr_list[[i]]
    overcut <- lr.pred > coff
    fdf <- df_train[overcut, ]   # predictions > coff
    nfdf <- df_train[!overcut, ] # predictions <= coff
    # Remove column y from nfdf
    nfdf <- nfdf[, !names(nfdf) %in% "loanstatus"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "loanstatus", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "loanstatus"]  # All columns except y
    fdfym <- as.vector(fdf_y)
    df_aug <- augmentation(fdf_x, nfdf, as.integer(fdfym$loanstatus)-1)
    testy <- c(df_test[, "loanstatus", drop = FALSE])
    testy$loanstatus <- as.numeric(testy$loanstatus)
    predvals <- scoringTools::predict(df_aug, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$loanstatus, predvals)
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
    nfdf <- nfdf[, !names(nfdf) %in% "loanstatus"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "loanstatus", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "loanstatus"]  # All columns except y
    fdfym <- as.vector(fdf_y)
    df_rec <- reclassification(fdf_x, nfdf, as.integer(fdfym$loanstatus)-1, 0.5) # or 0.02*j ?
    testy <- c(df_test[, "loanstatus", drop = FALSE])
    testy$loanstatus <- as.numeric(testy$loanstatus)
    predvals <- scoringTools::predict(df_rec, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$loanstatus, predvals)
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
    nfdf <- nfdf[, !names(nfdf) %in% "loanstatus"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "loanstatus", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "loanstatus"]  # All columns except y
    fdfym <- as.vector(fdf_y)
    df_par <- parcelling(fdf_x, nfdf, as.integer(fdfym$loanstatus)-1, probs, rep(1.15, length(probs) - 1))
    testy <- c(df_test[, "loanstatus", drop = FALSE])
    testy$loanstatus <- as.numeric(testy$loanstatus)
    predvals <- scoringTools::predict(df_par, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$loanstatus, predvals)
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
    nfdf <- nfdf[, !names(nfdf) %in% "loanstatus"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "loanstatus", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "loanstatus"]  # All columns except y
    fdfym <- as.vector(fdf_y$loanstatus)
    X_test <- as.data.frame(df_test[, !names(df_test) %in% "loanstatus"])
    testy <- c(df_test[, "loanstatus", drop = FALSE])
    testy$loanstatus <- as.numeric(testy$loanstatus)
    X_all <- as.data.frame(rbind(fdf_x, nfdf))
    y_all <- factor(c(fdfym, rep(NA, nrow(nfdf))))
    #df_mnm <- multinom(y_all ~ . , data = X_all)
    #predvals <- predict(df_mnm, df_test, type="probs")
    df_mnm <- vglm(y_all ~ . , data = X_all,family=multinomial())
    predvals <- predict(df_mnm, df_test, type="response")
    ginis[i] <- normalizedGini(testy$loanstatus, predvals[,2])
  }
  gini_avg[j] <- mean(ginis)
}
points(rejection_rate, gini_avg, pch=9, col="grey")
lines(rejection_rate, gini_avg, col="grey")

legend(0, 0.6, legend=c("Logistic Regression", "Augmentation", 
                        "Reclassification", "Parcelling", "Generative Model"),
       fill=c("black", "green", "red", "magenta", "grey"))