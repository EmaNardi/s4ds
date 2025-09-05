library(scoringTools)
library(caret)
library(VGAM)
library(nnet)

# https://github.com/pouyanebrahimi/Loan-Data-from-Prosper?tab=readme-ov-file

# As shown by the plot of frac_fdf_avg, that is the fraction of records for each cut-off
# value that are included in the fdf, the fraction of non-financed records (rejection rate)
# is above 50% up until 0.95 of cut-off value circa.

data <- as.data.frame(read.csv("~/Desktop/Script Progetto/prosperLoanData.csv", header=TRUE))
set.seed(42)

#                             Preprocessing

# Most of these have a lot of missing values, we remove it (hard to understand how to fill them)
# Others removed are just not useful enough
data <- data[,-c(1, 2, 3, 4, 7, 11, 12, 13, 14, 15, 16, 18, 21, 23, 24, 25, 27, 28, 29, 30, 36, 37,
                 39, 40, 41, 42, 43, 44, 45, 46, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
                 65, 66, 67, 69, 70, 74, 75, 76, 77, 80, 35, 81, 5, 79)]



#for (e in colnames(df)){
#  df[[e]] <- as.numeric(as.character(df[[e]]))
#}



# Occupation and EmploymentStatus have some missing values, filled with "Other"
data[data$Occupation == "",][6] <- "Other"
data[data$EmploymentStatus == "",][7] <- "Other"
data[data$Occupation == "Tradesman - Electrician",][6] <- "Tradesman"
data[data$Occupation == "Tradesman - Plumber",][6] <- "Tradesman"
data[data$Occupation == "Tradesman - Mechanic",][6] <- "Tradesman"
data[data$Occupation == "Tradesman - Carpenter",][6] <- "Tradesman"
data[data$Occupation == "Student - College Junior",][6] <- "Student"
data[data$Occupation == "Student - College Freshman",][6] <- "Student"
data[data$Occupation == "Student - College Graduate Student",][6] <- "Student"
data[data$Occupation == "Student - College Senior",][6] <- "Student"
data[data$Occupation == "Student - College Sophomore",][6] <- "Student"
data[data$Occupation == "Student - Community College",][6] <- "Student"
data[data$Occupation == "Student - Technical School",][6] <- "Student"
data[data$Occupation == "Engineer - Chemical",][6] <- "Engineer"
data[data$Occupation == "Engineer - Mechanical",][6] <- "Engineer"
data[data$Occupation == "Engineer - Electrical",][6] <- "Engineer"
data[data$Occupation == "Clergy",][6] <- "Clerical"
data[data$Occupation == "Religious",][6] <- "Clerical"
data[data$Occupation == "Judge",][6] <- "Attorney"
data[data$Occupation == "Dentist",][6] <- "Doctor"

data <- data[data$LoanStatus %in% c("Defaulted", "Completed"),]
data[data$LoanStatus == "Defaulted",][1] <- "0"
data[data$LoanStatus == "Completed",][1] <- "1"

data <- data[!is.na(data$CreditScoreRangeLower),]
data <- data[!is.na(data$TotalCreditLinespast7years),]
data <- data[!is.na(data$DelinquenciesLast7Years),]
data <- data[!is.na(data$DebtToIncomeRatio),]

data[data$CreditScoreRangeLower < 500,][9] <- 400
data[data$CreditScoreRangeLower > 780,][9] <- 800
data[data$CreditScoreRangeLower %in% c(500, 520, 540, 560, 580),][9] <- 500
data[data$CreditScoreRangeLower %in% c(600, 620, 640, 660, 680),][9] <- 600
data[data$CreditScoreRangeLower %in% c(700, 720, 740, 760, 780),][9] <- 700
#data[data$CreditScoreRangeUpper == 439,][11] <- 459
#data[data$CreditScoreRangeUpper == 899,][11] <- 879
data[data$Recommendations > 3,][24] <- 3
#data[data$InvestmentFromFriendsCount > 3,][25] <- 3

df_tot <- data

factors <- c("LoanStatus", "ListingCategory..numeric.", "Occupation", "EmploymentStatus",
             "IsBorrowerHomeowner", #"CreditScoreRangeLower", #"Term", "CreditScoreRangeUpper"
             "IncomeRange", "IncomeVerifiable", "Recommendations")#, "InvestmentFromFriendsCount")
numerics <- colnames(df_tot)[-c(1, 5, 6, 7, 8, 16, 17, 24)]#9, 25)]
for (e in numerics){
  df_tot[[e]] <- as.numeric(as.character(df_tot[[e]]))
}
for (e in factors){
  df_tot[[e]] <- as.factor(as.character(df_tot[[e]]))
}

# Automatizing this had problems: some attributes have more than one quantile equal to another
# (e.g. 1st Quantile == Mean), because a value was present for 80% of the dataset. In those cases,
# simply calling summary(df_tot$attribute) (e.g. summary(df_tot$attribute)[1] = Min, 
# summary(df_tot$attribute)[2] = 1st Quantile,...) would have given the same value to multiple
# boundaries of the cut function, breaking it
df_tot$BorrowerAPR <- as.integer(cut(df_tot$BorrowerAPR, breaks=c(0,0.13654,0.19876,0.28488,0.42),
                                     labels=c("0-0.13654","0.13654-0.19876","0.19876-0.28488", "0.28488-0.41355")))
df_tot$BorrowerRate <- as.integer(cut(df_tot$BorrowerRate, breaks=c(-1,0.1205,0.1792,0.2550,0.37),
                                      labels=c("0-0.1205","0.1205-0.1792","0.1792-0.2550", "0.2550-0.3600")))
df_tot$LenderYield <- as.integer(cut(df_tot$LenderYield, breaks=c(-1,0.1124,0.1678,0.2444,0.35),
                                     labels=c("-0.010-0.1124","0.1124-0.1678","0.1678-0.2444", "0.2444-0.3400")))
df_tot$TotalCreditLinespast7years <- as.integer(cut(df_tot$TotalCreditLinespast7years, breaks=c(0,15,24,34,137),
                                                    labels=c("2-15","15-24","24-34", "34-136")))
df_tot$OpenRevolvingAccounts <- as.integer(cut(df_tot$OpenRevolvingAccounts, breaks=c(-1,3,6,9,52),
                                               labels=c("0-3","3-6","6-9", "9-51")))
df_tot$OpenRevolvingMonthlyPayment <- as.integer(cut(df_tot$OpenRevolvingMonthlyPayment, breaks=c(-1,74,202,437,12800),
                                                     labels=c("0-74","74-202","202-437", "437-12769")))
df_tot$InquiriesLast6Months <- as.integer(cut(df_tot$InquiriesLast6Months, breaks=c(-1,1,2,3,64),
                                              labels=c("0-1","1-2","2-3", "3-64")))
df_tot$DelinquenciesLast7Years <- as.integer(cut(df_tot$DelinquenciesLast7Years, breaks=c(-1,1,2,3,100),
                                                 labels=c("0-1","1-2","2-3", "3-100")))
df_tot$DebtToIncomeRatio <- as.integer(cut(df_tot$DebtToIncomeRatio, breaks=c(-1,0.13,0.2,0.3,11),
                                           labels=c("0-0.13","0.13-0.2","0.2-0.3", "0.3-10.01")))
df_tot$StatedMonthlyIncome <- as.integer(cut(df_tot$StatedMonthlyIncome, breaks=c(-1,3000,4417,6500,485000),
                                             labels=c("0-3000","3000-4417","4417-6500", "6500-483333")))
df_tot$LoanOriginalAmount <- as.integer(cut(df_tot$LoanOriginalAmount, breaks=c(0,2600,4880,8000,36000),
                                            labels=c("1000-2600","2600-4880","4880-8000", "8000-35000")))
df_tot$MonthlyLoanPayment <- as.integer(cut(df_tot$MonthlyLoanPayment, breaks=c(-1,95.77,172.1,300.67,2252),
                                            labels=c("0-95.77","95.77-172.1","172.1-300.67", "300.67-2251.51")))
df_tot$LP_InterestandFees <- as.integer(cut(df_tot$LP_InterestandFees, breaks=c(-1,325.26,768,1525.26,13300),
                                            labels=c("-0.0034-325.26","325.26-768","768-1525.26", "1525.26-13206.08")))
df_tot$LP_ServiceFees <- as.integer(cut(df_tot$LP_ServiceFees, breaks=c(-600,-79.25,-37.53,-15.81,33),
                                        labels=c("-589.95--79.25","-79.25--37.53","-37.52--15.81", "-15.81-32.06")))
df_tot$LP_CollectionFees <- as.integer(cut(df_tot$LP_CollectionFees, breaks=c(-6230,-300,-200,-100,0),
                                           labels=c("-6221.32--300","-300--200","-200--100", "-100-0")))


df_tot$LoanStatus <- as.factor(data$LoanStatus)

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
  lr.fit <- glm(LoanStatus ~ .-LoanStatus, data = df[-df_idx,], family = binomial(logit))
  lr.pred <- predict.glm(lr.fit, newdata = df[-df_idx,], type="response")
  lr_list[[i]] <- lr.pred 
}

coff_step <- 0.095
x_max <- 0.95
#j_max <- floor(x_max/coff_step)
j_max <- 10
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
    overcut <- lr.pred >= coff
    fdf <- df_train[overcut, ]   # predictions > coff
    nfdf <- df_train[!overcut, ] # predictions <= coff
    frac_fdf[i] <- dim(fdf)[1]/(dim(fdf)[1]+dim(nfdf)[1])
    # Remove column y from nfdf
    nfdf <- nfdf[, !names(nfdf) %in% "LoanStatus"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "LoanStatus", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "LoanStatus"]  # All columns except y
    fdfym <- as.vector(fdf_y$LoanStatus)
    df_lr <- glm(fdf_y$LoanStatus ~ ., data=fdf_x, family=binomial(logit))
    testy <- c(df_test[, "LoanStatus", drop = FALSE])
    testy$LoanStatus <- as.numeric(testy$LoanStatus) # otherwise normalizedGini raises an error,
                                                     # but doing as.numeric earlier raises errors
                                                     # in the logistic regression
    predvals <- scoringTools::predict(df_lr, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$LoanStatus, predvals)
  }
  gini_avg[j+1] <- mean(ginis)
  frac_fdf_avg[j+1] <- mean(frac_fdf)
  plot(seq(0, x_max, coff_step), frac_fdf_avg)
}
rejection_rate <- rep(1, length(frac_fdf_avg)) - frac_fdf_avg
plot(rejection_rate, gini_avg, pch=15, col="black", xlab="Rejection Rate",
     ylab="Gini on test set", xlim=c(0, 0.5), ylim=c(0.1, 0.8))
lines(rejection_rate, gini_avg, col="black")

# second cycle: augmentation
for (j in 1:j_max){ #skips the first two steps, they don't cut anything, the augmentation breaks
  coff <- coff_step*j
  for (i in 1:i_max){
    df_test <- df_list[[i]]
    df_train <- df[-df_idx_list[[i]],]
    lr.pred <- lr_list[[i]]
    overcut <- lr.pred > coff
    fdf <- df_train[overcut, ]   # predictions > coff
    nfdf <- df_train[!overcut, ] # predictions <= coff
    # Remove column y from nfdf
    nfdf <- nfdf[, !names(nfdf) %in% "LoanStatus"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "LoanStatus", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "LoanStatus"]  # All columns except y
    fdfym <- as.vector(fdf_y)
    df_aug <- augmentation(fdf_x, nfdf, as.integer(fdfym$LoanStatus)-1)
    testy <- c(df_test[, "LoanStatus", drop = FALSE])
    testy$LoanStatus <- as.numeric(testy$LoanStatus)
    predvals <- scoringTools::predict(df_aug, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$LoanStatus, predvals)
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
    nfdf <- nfdf[, !names(nfdf) %in% "LoanStatus"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "LoanStatus", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "LoanStatus"]  # All columns except y
    fdfym <- as.vector(fdf_y)
    df_rec <- reclassification(fdf_x, nfdf, as.integer(fdfym$LoanStatus)-1, 0.5) # or 0.02*j ?
    testy <- c(df_test[, "LoanStatus", drop = FALSE])
    testy$LoanStatus <- as.numeric(testy$LoanStatus)
    predvals <- scoringTools::predict(df_rec, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$LoanStatus, predvals)
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
    nfdf <- nfdf[, !names(nfdf) %in% "LoanStatus"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "LoanStatus", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "LoanStatus"]  # All columns except y
    fdfym <- as.vector(fdf_y)
    df_par <- parcelling(fdf_x, nfdf, as.integer(fdfym$LoanStatus)-1, probs, rep(1.15, length(probs) - 1))
    testy <- c(df_test[, "LoanStatus", drop = FALSE])
    testy$LoanStatus <- as.numeric(testy$LoanStatus)
    predvals <- scoringTools::predict(df_par, df_test, type = "response")
    ginis[i] <- normalizedGini(testy$LoanStatus, predvals)
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
    nfdf <- nfdf[, !names(nfdf) %in% "LoanStatus"]
    # Split fdf into two dataframes
    fdf_y <- fdf[, "LoanStatus", drop = FALSE]  # Only y column (keep as dataframe)
    fdf_x <- fdf[, !names(fdf) %in% "LoanStatus"]  # All columns except y
    fdfym <- as.vector(fdf_y$LoanStatus)
    X_test <- as.data.frame(df_test[, !names(df_test) %in% "LoanStatus"])
    testy <- c(df_test[, "LoanStatus", drop = FALSE])
    testy$LoanStatus <- as.numeric(testy$LoanStatus)
    X_all <- as.data.frame(rbind(fdf_x, nfdf))
    y_all <- factor(c(fdfym, rep(NA, nrow(nfdf))))
    #df_mnm <- multinom(y_all ~ . , data = X_all)
    #predvals <- predict(df_mnm, df_test, type="probs")
    df_mnm <- vglm(y_all ~ . , data = X_all,family=multinomial())
    predvals <- predict(df_mnm, df_test, type="response")
    ginis[i] <- normalizedGini(testy$LoanStatus, predvals[,2])
  }
  gini_avg[j] <- mean(ginis)
}
points(rejection_rate, gini_avg, pch=9, col="grey")
lines(rejection_rate, gini_avg, col="grey")

legend(0, 0.5, legend=c("Logistic Regression", "Augmentation", 
                        "Reclassification", "Parcelling", "Generative Model"),
       fill=c("black", "green", "red", "magenta", "grey"))
