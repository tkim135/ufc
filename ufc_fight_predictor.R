####################################################
#                      HEAD                        #
####################################################

# File Name: ufc_fight_predictor.R
# Authors: Cary Turner and Tony Kim
# Description: Data stuff
####################################################

library(tidyverse)

master <- read.csv('ufc-master.csv')


# Create an indicator set to 1 if red fighter won and 0 otherwise.
# There were no draws in this data set.
master$red_win <- ifelse(master$Winner == 'Red', 1, 0)
names(master)


# Create a vector of all variables we want to keep
cols <- c(
  'R_odds',
  'B_odds',
  'title_bout',
  'weight_class',
  'no_of_rounds',
  'B_current_lose_streak',
  'B_current_win_streak',
  'B_draw',
  'B_longest_win_streak',
  'B_losses',
  'B_wins',
  'B_Stance',
  'B_total_rounds_fought',
  'B_total_title_bouts',
  'B_win_by_KO.TKO',
  'B_win_by_Submission',
  'B_Weight_lbs',
  'B_avg_SIG_STR_landed',
  'B_avg_SIG_STR_pct',
  'B_avg_SUB_ATT',
  'B_avg_TD_landed',
  'B_avg_TD_pct',
  'B_Height_cms',
  'B_Reach_cms',
  'B_age',
  'R_current_lose_streak',
  'R_current_win_streak',
  'R_draw',
  'R_longest_win_streak',
  'R_losses',
  'R_wins',
  'R_Stance',
  'R_total_rounds_fought',
  'R_total_title_bouts',
  'R_win_by_KO.TKO',
  'R_win_by_Submission',
  'R_Weight_lbs',
  'R_avg_SIG_STR_landed',
  'R_avg_SIG_STR_pct',
  'R_avg_SUB_ATT',
  'R_avg_TD_landed',
  'R_avg_TD_pct',
  'R_Height_cms',
  'R_Reach_cms',
  'R_age',
  'better_rank',
  'finish_round',
  'total_fight_time_secs',
  'red_win'
)



# Update data frame to include only relevant variables
fights <- master[,cols]

# for (var in names(fights)) {
#   print(var)
#   print(sum(is.na(fights[,var])))
#   print("")
# }

# Get rid of NA values
fights <- na.omit(fights)


# Subtract Blue height, weight, reach, and age from Red's
# to get Red's height, weight, reach, and age advantage
fights$height_adv <- fights$R_Height_cms - fights$B_Height_cms
fights$weight_adv <- fights$R_Weight_lbs - fights$B_Weight_lbs
fights$reach_adv <- fights$R_Reach_cms - fights$B_Reach_cms
fights$age_adv <- fights$R_age - fights$B_age

# Do same for avg strikes, and takedowns landed/percentage, and for submission attempts
fights$str_landed_adv <- fights$R_avg_SIG_STR_landed - fights$B_avg_SIG_STR_landed
fights$str_pct_adv <- fights$R_avg_SIG_STR_pct - fights$B_avg_SIG_STR_pct
fights$TD_landed_adv <- fights$R_avg_TD_landed - fights$B_avg_TD_landed
fights$TD_pct_adv <- fights$R_avg_TD_pct - fights$B_avg_TD_pct
fights$sub_att_adv <- fights$R_avg_SUB_ATT - fights$B_avg_SUB_ATT

fights$lose_streak_dif <- fights$R_current_lose_streak - fights$B_current_lose_streak
fights$win_streak_dif <- fights$R_current_win_streak - fights$B_current_win_streak
fights$longest_win_streak_dif <- fights$R_longest_win_streak - fights$B_longest_win_streak
fights$draws_dif <- fights$R_draw - fights$B_draw
fights$losses_dif <- fights$R_losses - fights$B_losses
fights$wins_dif <- fights$R_wins - fights$B_wins
fights$total_rounds_dif <- fights$R_total_rounds_fought - fights$B_total_rounds_fought
fights$title_bouts_dif <- fights$R_total_title_bouts - fights$B_total_title_bouts

# Determine number of wins by KO/TKO or submission for each fighter
fights$R_wins_by_KO_sub <- fights$R_win_by_KO.TKO + fights$R_win_by_Submission
fights$B_wins_by_KO_sub <- fights$B_win_by_KO.TKO + fights$B_win_by_Submission

# Determine number of wins by decision for each fighter.
# Wins by decision = wins - wins by KO/TKO or submission
fights$R_wins_by_dec <- fights$R_wins - fights$R_wins_by_KO_sub
fights$B_wins_by_dec <- fights$B_wins - fights$B_wins_by_KO_sub

# Get rid of nonsensical negative values
fights <- fights %>%
  filter(R_wins_by_dec >= 0, B_wins_by_dec >= 0)

# Calculate the ratio of wins by KO/TKO and submission (finishes) to wins by decision.
# We add one to each to avoid zero division.
fights$R_finish_dec_ratio <- (fights$R_wins_by_KO_sub + 1) / (fights$R_wins_by_dec + 1)
fights$B_finish_dec_ratio <- (fights$B_wins_by_KO_sub + 1) / (fights$B_wins_by_dec + 1)

# Calculate overall win loss ratio for each fighter. Add one again to avoid zero division.
fights$R_wl_ratio <- (fights$R_wins + 1) / (fights$R_losses + 1)
fights$B_wl_ratio <- (fights$R_wins + 1) / (fights$B_losses + 1)

# Difference between number of KO/submission finishes
fights$KO_sub_wins_dif <- fights$R_wins_by_KO_sub - fights$B_wins_by_KO_sub

# Finish ratio difference between fighters
fights$finish_ratio_adv <- fights$R_finish_dec_ratio - fights$B_finish_dec_ratio

# Win-loss ratio difference between fighters
fights$wl_ratio_adv <- fights$R_wl_ratio - fights$B_wl_ratio

# Create variables for different stances of the fighters
fights$R_switch <- ifelse(fights$R_Stance == 'Switch', 1, 0)
fights$B_switch <- ifelse(fights$B_Stance == 'Switch', 1, 0)

# Create indicator for each fighter's relative ranking
fights$R_better_rank <- ifelse(fights$better_rank == 'Red', 1, 0)
fights$B_better_rank <- ifelse(fights$better_rank == 'Blue',  1, 0)



# Create vector with names of all variables we are keeping for analysis
keep <- c(
          'R_odds',
          'red_win',
          'title_bout',
          'weight_class',
          #'no_of_rounds',
          #'total_fight_time_secs',
          'height_adv',
          'weight_adv',
          'age_adv',
          'reach_adv',
          'str_landed_adv',
          'str_pct_adv',
          'TD_landed_adv',
          'TD_pct_adv',
          'lose_streak_dif',
          'win_streak_dif',
          'longest_win_streak_dif',
          'draws_dif',
          'losses_dif',
          'wins_dif',
          'total_rounds_dif',
          'title_bouts_dif',
          'KO_sub_wins_dif',
          'finish_ratio_adv',
          'wl_ratio_adv',
          'R_better_rank',
          'B_better_rank',
          'R_switch',
          'B_switch'
          )

# # Vector containing names of all independent fighter stat variables
# ind.stats <- c(
#             'B_current_lose_streak',
#             'B_current_win_streak',
#             'B_draw',
#             'B_losses',
#             'B_wins',
#             'B_total_rounds_fought',
#             'B_total_title_bouts',
#             'B_win_by_Submission',
#             'B_Weight_lbs',
#             'B_avg_SIG_STR_landed',
#             'B_avg_SIG_STR_pct',
#             'B_win_by_KO.TKO',
#             'B_avg_SUB_ATT',
#             'B_avg_TD_landed',
#             'B_avg_TD_pct',
#             'B_Height_cms',
#             'B_Reach_cms',
#             'B_age',
#             'R_current_lose_streak',
#             'R_current_win_streak',
#             'R_draw',
#             'R_losses',
#             'R_wins',
#             'R_total_rounds_fought',
#             'R_total_title_bouts',
#             'R_win_by_Submission',
#             'R_Weight_lbs',
#             'R_avg_SIG_STR_landed',
#             'R_avg_SIG_STR_pct',
#             'R_win_by_KO.TKO',
#             'R_avg_SUB_ATT',
#             'R_avg_TD_landed',
#             'R_avg_TD_pct',
#             'R_Height_cms',
#             'R_Reach_cms',
#             'R_age',
#             'B_wins_by_KO_sub',
#             'R_wins_by_KO_sub',
#             'B_wins_by_dec',
#             'R_wins_by_dec',
#             'B_finish_dec_ratio',
#             'R_finish_dec_ratio',
#             'B_wl_ratio',
#             'R_wl_ratio'
#             )
#
# # Vector containing variable names common to both data frames
# common.vars <- c(
#                   'R_odds',
#                   'B_odds',
#                   'title_bout',
#                   'weight_class',
#                   'no_of_rounds',
#                   'total_fight_time_secs',
#                   'red_win',
#                   'R_better_rank',
#                   'B_better_rank',
#                   'R_switch',
#                   'B_switch'
#                   )





############### Split Data Into Test and Train Sets ###################

# Split into train and test sets, using 80/20 split
set.seed(138)
in.test <- sample(nrow(fights), nrow(fights)/5)

# Sanity check
length(in.test)
dim(fights)[1]*0.2

# Assign train and test sets
test <- fights[in.test,keep]
train <- fights[-in.test,keep]



############## Initial Data Exploration ###############

# Indices of non-numeric variables
non_numeric <- c(3,4)


# # Data frame containing the difference in fighter stats
# train.difs <- train %>%
#   select(c(common.vars, difs)) %>%
#   filter(finish_ratio_adv != Inf)
#
# # Data frame containing the independent fighter stats
# train.ind <- train %>%
#   select(c(common.vars, ind.stats)) %>%
#   filter(B_finish_dec_ratio != Inf, R_finish_dec_ratio != Inf)
#
# log.ind.vars <-log(train.ind[,-c(non_numeric, 1, 2, 6, 7)] + .05)
# log.ind.vars$red_win <- train.ind$red_win
# log.ind.vars$R_odds <- train.ind$R_odds
#
# squared.ind.vars <- train.ind[,-c(non_numeric, 1, 2, 6, 7)]^2
# squared.ind.vars$red_win <- train.ind$red_win
# squared.ind.vars$R_odds <- train.ind$R_odds
#
# #log.dif.vars <- log(train.difs[,-c(non_numeric, 1, 2, 7)] + .05)


# cube predictors (would square them but we want negatives to stay negative)
cubed.vars <- train[,-c(1, 2, 3, 4)]^3
cubed.vars$red_win <- train$red_win
cubed.vars$R_odds <- train$R_odds

# Check for correlation between variables and outputs
library(corrplot)
C <- cor(train[,-non_numeric])
corrplot(C, method = 'circle')
C

# Check for correlation between cubic vars and outputs
C.cubic <- cor(cubed.vars)
corrplot(C.cubic, method = 'circle')

colors <- c('1' = 'red', '0' = 'blue')

#library(GGally)
#ggpairs(train)

# Plot the density of all variables, separated by cases where red wins vs. blue wins
for (var in names(train[,-non_numeric])) {
  title <- cat('Density of',var)
  plot <- ggplot(train) +
    geom_density(aes(x = train[,var], color = as.factor(red_win))) +
    scale_color_manual(values = colors) +
    labs(x = var, color = 'red_win', title = title) +
    theme(plot.title = element_text(hjust = 0.5))
  print(plot)
  Sys.sleep(2)
}

# Scatter plot of each var vs. R_odds
for (var in names(train[,-non_numeric])) {
  title <- cat('Red Odds Vs.',var)
  plot <- ggplot(train, aes(x = train[,var], y = train$R_odds)) +
    geom_point() +
    geom_smooth(method = 'lm', color = 'blue', se = FALSE) +
    labs(x = var, y = 'R_odds', title = title) +
    theme(plot.title = element_text(hjust = 0.5))
  print(plot)
  Sys.sleep(2)
}



############### Predictive Modeling #################
library(ROCR)
library(glmnet)
library(cvTools)
library(boot)

# Function to compute the AUC of a binary classifier
compute_auc <- function(p, labels) {
  pred <- prediction(p, labels)
  auc <- performance(pred, 'auc')
  auc <- unlist(slot(auc, 'y.values'))
  auc
}

# Function to plot the ROC curve of a binary classifier
plot_roc <- function(p, labels, model_name) {
  pred <- prediction(p, labels)
  perf <- performance(pred,"tpr","fpr")
  plot(perf, col="black", main = paste("ROC for", model_name))
}

# Calculates accuracy of our logistic regression classifier
# to be used when running cross-validation
cost <- function(r, pi) mean(abs(r-pi) < 0.5)

# Build logistic regression model using all variables. This is our base
# model that we will judge other models against.
glm.base <- glm(red_win ~ ., data = train, family = 'binomial')

# Compute cross-validation error using the base model
base.err <- cv.glm(train, glm.base, cost, K = 10)
base.err[3]

glm.base.prob <- predict(glm.base, train, type='response')
glm.base.pred <- rep(0, length(train$red_win))
glm.base.pred[(glm.base.prob > 0.55)] <- 1
mean(glm.base.pred == train$red_win)

# Compute AUC on training set
base.train.auc <- compute_auc(predict(glm.base, data = train, type = 'response'), train$red_win)
base.train.auc



################## Ridge and Lasso Models for Binary Outcome #####################

### Predicting wins/losses ###

# Remove binary outcome variable (red_win) from cubed.vars data frame
cubed.vars <- cubed.vars[,-24]

# Change names of all columns to reflect that they are cubed.
cubed.cols <- rep('', dim(cubed.vars)[2])
for (i in 1:length(names(cubed.vars))) {
  cubed.cols[i] <- paste(names(cubed.vars)[i], '^3', sep='')
}


# Create model matrix with all cubed terms and all two way interactions
form <- red_win ~ . + .^2
x <- model.matrix(form, data=train)

# Use column bind to add all cubed variables and our outcome variable (red_win)
x <- cbind(train$red_win, x, cubed.vars)
x <- x[,-2]
colnames(x)[colnames(x) == 'train$red_win'] <- 'red_win'

# Recast it as a model matrix
x.train <- model.matrix(red_win ~ ., x)

# Create vector of our outcome variable
y.train <- train$red_win

# Standardize the data so all variables are measured on the same scale
# x.train <- scale(x.train)
# y.train <- scale(y.train)

### Ridge Regrssion
ridge.lr <- cv.glmnet(x.train, y.train, type.measure='auc', alpha=0,
                      family='binomial', standardize=TRUE, seed=1)
plot(ridge.lr$lambda, ridge.lr$cvm, xlab='Lambda', ylab='AUC', main='Ridge')
ridge.lambda <- ridge.lr$lambda.min
ridge.auc <- max(ridge.lr$cvm)

# Make predictions on full training set (using lambda that maximizes AUC)
ridge.lr.pred <- predict(ridge.lr, s=ridge.lr$lambda.min, newx=x.train, type='response')

# Plot ROC curve for model using AUC-maximizing lambda
plot_roc(ridge.lr.pred, train$red_win, paste('Ridge (Lambda = ',ridge.lr$lambda.min,')', sep=''))
ridge.train.auc <- compute_auc(ridge.lr.pred, train$red_win)
ridge.train.auc

# Transform probabilities into actual predicted outcomes, using 0.55 as our threshold
ridge.lr.pred <- ifelse(ridge.lr.pred > 0.55, 1, 0)

# Create confusion matrix
ridge.conf <- table(ridge.lr.pred, train$red_win)
TP <- ridge.conf[2,2]
TN <- ridge.conf[1,1]
FP <- ridge.conf[2,1]
FN <- ridge.conf[1,2]
n <- TP + TN + FP + FN
accuracy <- (TP + TN) / n
zo.loss <- (FP + FN) / n
TPR <- TP / (FN + TP)
FPR <- FP / (TN + FP)
TNR <- TN / (TN + FP)
FNR <- FN / (FN + TP)
precision <- TP / (TP + FP)
false.discovery <- FP / (TP + FP)

Ridge <- c(accuracy, zo.loss, TPR, TNR, precision, FPR, FNR, false.discovery, ridge.train.auc)

### Lasso
lasso.lr <- cv.glmnet(x.train, y.train, type.measure='auc', alpha=1,
                      family='binomial', standardize=TRUE, seed=1)
plot(lasso.lr$lambda, lasso.lr$cvm, xlab='Lambda', ylab='AUC', main='Lasso')
lasso.lambda <- lasso.lr$lambda.min
lasso.auc <- max(lasso.lr$cvm)

# Make predictions on full training set (using AUC-maximizing lambda)
lasso.lr.pred <- predict(lasso.lr, s=lasso.lr$lambda.min, newx=x.train, type='response')

# Plot ROC curve for Lasso model using AUC-maximizing lambda
plot_roc(lasso.lr.pred, train$red_win, paste('Lasso (Lambda = ',lasso.lr$lambda.min,')', sep=''))
lasso.train.auc <- compute_auc(lasso.lr.pred, train$red_win)
lasso.train.auc

# Transform probabilities into actual predicted outcomes, using 0.55 as our threshold
lasso.lr.pred <- ifelse(lasso.lr.pred > 0.55, 1, 0)

# Create confusion matrix
lasso.conf <- table(lasso.lr.pred, train$red_win)

TP <- lasso.conf[2,2]
TN <- lasso.conf[1,1]
FP <- lasso.conf[2,1]
FN <- lasso.conf[1,2]
n <- TP + TN + FP + FN
accuracy <- (TP + TN) / n
zo.loss <- (FP + FN) / n
TPR <- TP / (FN + TP)
FPR <- FP / (TN + FP)
TNR <- TN / (TN + FP)
FNR <- FN / (FN + TP)
precision <- TP / (TP + FP)
false.discovery <- FP / (TP + FP)

Lasso <- c(accuracy, zo.loss, TPR, TNR, precision, FPR, FNR, false.discovery, lasso.train.auc)
Metric <- c('Accuracy', '0-1 Loss', 'Sensitivity', 'Specificity', 'Precision',
            'Type I Error Rate', 'Type II Error Rate', 'False Discovery Rate', 'AUC')

model.metrics.train <- data.frame(Metric, Lasso, Ridge)
model.metrics.train


coef(lasso.lr, s='lambda.min')
coef(ridge.lr, s='lambda.min')


################### Predicting Fighter Odds ######################











