####################################################
#                      HEAD                        #
####################################################

# File Name: ufc_fight_predictor.R
# Authors: Cary Turner and Tony Kim
# Description: 
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
  'gender',
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
  'finish',
  'finish_round',
  'total_fight_time_secs',
  'red_win'
)



# Update data frame to include only relevant variables
fights <- master[,cols]
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

# Determine number of wins by KO/TKO or submission for each fighter
fights$R_wins_by_KO_sub <- fights$R_win_by_KO.TKO + fights$R_win_by_Submission
fights$B_wins_by_KO_sub <- fights$B_win_by_KO.TKO + fights$B_win_by_Submission

# Determine number of wins by decision for each fighter.
# Wins by decision = wins - wins by KO/TKO or submission
fights$R_wins_by_dec <- fights$R_wins - fights$R_wins_by_KO_sub
fights$B_wins_by_dec <- fights$B_wins - fights$B_wins_by_KO_sub

# Calculate the ratio of wins by KO/TKO and submission (finishes) to wins by decision.
# We add one to each to avoid zero division.
fights$R_finish_dec_ratio <- (fights$R_wins_by_KO_sub + 1) / (fights$R_wins_by_dec + 1)
fights$B_finish_dec_ratio <- (fights$B_wins_by_KO_sub + 1) / (fights$B_wins_by_dec + 1)

# Calculate overall win loss ratio for each fighter. Add one again to avoid zero division.
fights$R_wl_ratio <- (fights$R_wins + 1) / (fights$R_losses + 1)
fights$B_wl_ratio <- (fights$R_wins + 1) / (fights$B_losses + 1)



# Split into train and test sets, using 80/20 split
set.seed(138)
in.test <- sample(nrow(fights), nrow(fights)/5)

# Sanity check
length(in.test)
dim(fights)[1]*0.2

# Assign train and test sets
test <- fights[in.test,]
train <- fights[-in.test,]


############## Initial Data Exploration ###############

# Indices of non-numeric variables
non_numeric <- c(3,4,5,13,33,47,48)

































