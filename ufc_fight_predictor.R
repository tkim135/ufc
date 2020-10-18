####################################################
#                      HEAD                        #
####################################################

# File Name: ufc_fight_predictor.R
# Authors: Cary Turner and Tony Kim
# Description: 
####################################################

library(tidyverse)

master <- read.csv('ufc-master.csv')

master$red_win <- master$Winner == 'Red'
names(master)

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
          'B_Weight_lbs',
          'B_avg_SIG_STR_landed',
          'B_avg_SIG_STR_pct',
          'B_avg_SUB_ATT',
          'B_avg_TD_landed',
          'B_avg_TD_pct',
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
          'R_Weight_lbs',
          'R_avg_SIG_STR_landed',
          'R_avg_SIG_STR_pct',
          'R_avg_SUB_ATT',
          'R_avg_TD_landed',
          'R_avg_TD_pct',
          'height_dif',
          'reach_dif',
          'age_dif',
          'better_rank',
          'finish',
          'red_win'
          )

fights_full <- master[,cols]


































