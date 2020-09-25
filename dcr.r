# This script calculates hypotheses and generates figures to assess Decred Project governance
# Author: Marcelo Martins (stakey.club)
# Version: 0.1
# 
# Tested with the following configuration:
# sessionInfo()
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.6
# > packageVersion("plyr")
# [1] '1.8.6'
# > packageVersion("ggplot2")
# [1] '3.3.2'
# > packageVersion("dplyr")
# [1] '1.0.0'
# > packageVersion("patchwork")
# [1] '1.0.1'
# > packageVersion("corrplot")
# [1] '0.84'
# > packageVersion("gplots")
# [1] '3.0.4'
# > packageVersion("rpart")
# [1] '4.1.15'
# > packageVersion("rpart.plot")
# [1] '3.0.8'
# > packageVersion("rsample")
# [1] '0.0.7'
# > packageVersion("caret")
# [1] '6.0.86'

# This script requires the following libraries, to install:
list.of.packages <- c("plyr", "ggplot2", "dplyr", "patchwork", "corrplot", "gplots", "rpart", "rpart.plot", "rsample", "caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load libraries
library(plyr)
library(dplyr)
# Load graphs libraries
library(gplots)     # Required by heatmap.2
library(ggplot2)    # Used by save_figure()
library(patchwork)  # 2 charts together

# Define na.locf function without loading a huge package (zoo)
na.locf <- function(x) {
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v) + 1]
}

print_version <- function() {
  print(paste("R.Version()$version.string:", R.Version()$version.string))
  print(paste("R.Version()$platform:", R.Version()$platform))
  print(paste("plyr:", packageVersion("plyr")))
  print(paste("ggplot2:", packageVersion("ggplot2")))
  print(paste("dplyr:", packageVersion("dplyr")))
  print(paste("patchwork:", packageVersion("patchwork")))
  print(paste("corrplot:", packageVersion("corrplot")))
  print(paste("gplots:", packageVersion("gplots")))
  print(paste("rpart:", packageVersion("rpart")))
  print(paste("rpart.plot:", packageVersion("rpart.plot")))
  print(paste("rsample:", packageVersion("rsample")))
  print(paste("caret:", packageVersion("caret")))
}

define_variables <- function() {
  # Dates
  str_genesis_date <<- "2016-02-08"
  str_initial_date <<- "2016-02-08"
  str_final_date <<- "2020-02-10"
  str_one_year_genesis_date <<- "2017-02-10"
  str_dcr_asic_launch_date <<- "2018-06-01"
  # str_privacy_initial_date <<- "2019-08-28"
  # str_privacy_final_date <<- "2019-08-29"
  str_privacy_initial_date <<- "2019-02-10"
  str_privacy_final_date <<- str_final_date
  date_genesis_date <<- as.Date(str_genesis_date, format = "%Y-%m-%d")
  date_initial_date <<- as.Date(str_initial_date, format = "%Y-%m-%d")
  date_final_date <<- as.Date(str_final_date, format = "%Y-%m-%d")
  str_genesis_datetime <<- "2016-02-08 18:00:00"
  str_final_datetime <<- "2020-02-10 18:00:00"
  posixct_genesis_date <<- as.POSIXct(str_genesis_datetime, format = "%Y-%m-%d %H:%M:%S")
  posixct_final_date <<- as.POSIXct(str_final_datetime, format = "%Y-%m-%d %H:%M:%S")
  # str_dcr_est_end_datetime <<- "2039-06-21 18:00:00"
  str_dcr_est_end_datetime <<- "2039-06-21 23:59:00"
  posixct_dcr_genesis_date <<- as.POSIXct(str_genesis_datetime, format = "%Y-%m-%d %H:%M:%S")
  posixct_dcr_final_date <<- as.POSIXct(str_dcr_est_end_datetime, format = "%Y-%m-%d %H:%M:%S")
  # Bitcoin dates
  str_btc_genesis_datetime <<- "2009-01-03 18:00:00"
  posixct_btc_genesis_date <<- as.POSIXct(str_btc_genesis_datetime, format = "%Y-%m-%d %H:%M:%S")
  posixct_btc_final_date <<- as.POSIXct(str_dcr_est_end_datetime, format = "%Y-%m-%d %H:%M:%S")
  # General
  str_basedir <<- "decred-gov-data-analysis/csv/"
  str_figuredir <<- "decred-gov-data-analysis/figures/"
  str_textfilename <<- "dcr_r_results.txt"
  int_final_block <<- 422600
  dbl_dcr_supply_upper_limit <<- 20999999.99800912
  # Colour
  g_color1 <<- "black"
  g_color2 <<- "blue"
  g_color3 <<- "steelblue"
  g_color4 <<- "orange"
  # File List
  file_list <<- c()
}

prepare_dataframes <- function() {
  # DCR and BTC dataframes -- dcr_d: daily; dcr_g: general; dcr_t: tickets
  # Daily
  dcr_d_actual_coin_issuance <<- read.table(paste0(str_basedir, "psql_daily_coin-issuance.csv"), header = TRUE, sep = ",")
  dcr_d_fees_diff_tx <<- read.table(paste0(str_basedir, "psql_daily_fees-difficulty-txcount.csv"), header = TRUE, sep = ",")
  dcr_d_transaction_volume <<- read.table(paste0(str_basedir, "psql_daily_transactions-volume.csv"), header = TRUE, sep = ",")
  dcr_d_ticket_price_pool_size <<- read.table(paste0(str_basedir, "psql_daily_ticket-price_pool-size.csv"), header = TRUE, sep = ",")
  dcr_d_voters_difficulty <<- read.table(paste0(str_basedir, "psql_daily_voters_difficulty.csv"), header = TRUE, sep = ",")
  dcr_d_block_time <<- read.table(paste0(str_basedir, "psql_daily_block-time-historical-average.csv"), header = TRUE, sep = ",")
  dcr_d_circ_staked <<- read.table(paste0(str_basedir, "dcrdata_json_circ_staked.csv"), header = TRUE, sep = ",")
  dcr_d_privacy_mix <<- read.table(paste0(str_basedir, "dcrdata_json_privacy_mix.csv"), header = TRUE, sep = ",")
  # Daily, rewards
  #dcr_d_network_funding <<- read.table(paste0(str_basedir, "psql_daily_network-funding.csv"), header = TRUE, sep = ",")
  #dcr_d_pos_rewards <<- read.table(paste0(str_basedir, "psql_daily_pos-rewards.csv"), header = TRUE, sep = ",")
  # Ticket
  dcr_t_ticket_draw <<- read.table(paste0(str_basedir, "psql_ticket-time-to-draw.csv"), header = TRUE, sep = ",")
  dcr_t_ticket_revoked <<- read.table(paste0(str_basedir, "psql_ticket-revoked.csv"), header = TRUE, sep = ",")
  # General

  dcr_g_agendas_vote_results <<- read.table(paste0(str_basedir, "psql_general_agenda_vote_results.csv"), header = TRUE, sep = ",")
  #dcr_g_agendas_lockin <<- read.table(paste0(str_basedir, "psql_general_agendas-lockin.csv"), header = TRUE, sep = ",")
  dcr_g_vote_change <<- read.table(paste0(str_basedir, "psql_general_vote-change.csv"), header = TRUE, sep = ",")
  dcr_g_vote_total_number <<- read.table(paste0(str_basedir, "psql_general_vote-total-number.csv"), header = TRUE, sep = ",")
  #dcr_g_voters_block_count <<- read.table(paste0(str_basedir, "psql_general_voters-block-count.csv"), header = TRUE, sep = ",")
  dcr_g_voters_block_count <<- read.table(paste0(str_basedir, "psql_general_voters_per_block-alltime.csv"), header = TRUE, sep = ",")
  # Block
  dcr_b_expected_coin_issuance <<- read.table(paste0(str_basedir, "dcr-expected-block-date_and_coin_issuance.csv"), header = TRUE, sep = ",")
  #dcr_b_issuance_pos_reward <<- read.table(paste0(str_basedir, "psql_block_coin-issuance-pos_reward.csv"), header = TRUE, sep = ",")
  dcr_b_issuance_pow_reward <<- read.table(paste0(str_basedir, "psql_block_coin-issuance-pow_reward.csv"), header = TRUE, sep = ",")
  #dcr_b_issuance_network_fund <<- read.table(paste0(str_basedir, "psql_block_coin-issuance-network_fund.csv"), header = TRUE, sep = ",")
  # Genesis block, coins destination
  dcr_1_block_founders_dest <<- read.table(paste0(str_basedir, "psql_transactions-genesis_block-founders.csv"), header = TRUE, sep = ",")
  dcr_1_block_airdrop_dest <<- read.table(paste0(str_basedir, "psql_transactions-genesis_block-airdrop.csv"), header = TRUE, sep = ",")
}

process_dataframes <- function() {
  ### High level analysis: when were those transactions spent?
  # Genesis block, founders
  dcr_1_block_founders_dest$value <- round(dcr_1_block_founders_dest$value / 100000000, 8)
  dcr_1_block_founders_dest$block_height <- as.numeric(dcr_1_block_founders_dest$block_height)
  dcr_1_block_founders_dest$num_vout <- as.numeric(dcr_1_block_founders_dest$num_vout)
  dcr_1_block_founders_dest <<- dcr_1_block_founders_dest
  # Genesis block, airdrops
  dcr_1_block_airdrop_dest$value <- round(dcr_1_block_airdrop_dest$value / 100000000, 8)
  dcr_1_block_airdrop_dest$block_height <- as.numeric(dcr_1_block_airdrop_dest$block_height)
  dcr_1_block_airdrop_dest$num_vout <- as.numeric(dcr_1_block_airdrop_dest$num_vout)
  dcr_1_block_airdrop_dest <<- dcr_1_block_airdrop_dest
  # Calculate totals
  # Consider as spent only the addresses spent before block height 422600 (defined in variable 'int_final_block'), mined on 11 Feb 2020, to be consistent
  founders_unspent_addresses <<- sum(ifelse(dcr_1_block_founders_dest[complete.cases(dcr_1_block_founders_dest), ]$block_height > int_final_block, 1, 0)) + sum(is.na(dcr_1_block_founders_dest$block_height))
  founders_spent_addresses <<- sum(ifelse(dcr_1_block_founders_dest[complete.cases(dcr_1_block_founders_dest), ]$block_height < int_final_block, 1, 0))
  airdrop_unspent_addresses <<- sum(ifelse(dcr_1_block_airdrop_dest[complete.cases(dcr_1_block_airdrop_dest), ]$block_height > int_final_block, 1, 0)) + sum(is.na(dcr_1_block_airdrop_dest$block_height))
  airdrop_spent_addresses <<- sum(ifelse(dcr_1_block_airdrop_dest[complete.cases(dcr_1_block_airdrop_dest), ]$block_height < int_final_block, 1, 0))
  airdrop_total_addresses <<- nrow(dcr_1_block_airdrop_dest)
  founders_total_addresses <<- nrow(dcr_1_block_founders_dest)
  founders_total_dcr <<- sum(dcr_1_block_founders_dest$value, 0)  # Expected: 840000 DCR
  airdrop_total_dcr <<- sum(dcr_1_block_airdrop_dest$value, 0)  # Expected: 840000 DCR
  founders_total_spent_dcr <<- sum(ifelse(dcr_1_block_founders_dest[complete.cases(dcr_1_block_founders_dest), ]$block_height < int_final_block, dcr_1_block_founders_dest$value, 0))
  airdrop_total_spent_dcr <<- sum(ifelse(dcr_1_block_airdrop_dest[complete.cases(dcr_1_block_airdrop_dest), ]$block_height < int_final_block, dcr_1_block_airdrop_dest$value, 0))
  vert_founders_total_spent_dcr <<- round(founders_total_spent_dcr / founders_total_dcr, 4)
  vert_airdrop_total_spent_dcr <<- round(airdrop_total_spent_dcr / airdrop_total_dcr, 4)
  founders_12m_spent_dcr <<- sum(ifelse(dcr_1_block_founders_dest[complete.cases(dcr_1_block_founders_dest), ]$block_height < 100000, dcr_1_block_founders_dest$value, 0))
  airdrop_12m_spent_dcr <<- sum(ifelse(dcr_1_block_airdrop_dest[complete.cases(dcr_1_block_airdrop_dest), ]$block_height < 100000, dcr_1_block_airdrop_dest$value, 0))
  vert_founders_12m_spent_dcr <<- round(founders_12m_spent_dcr / founders_total_dcr, 4)
  vert_airdrop_12m_spent_dcr <<- round(airdrop_12m_spent_dcr / airdrop_total_dcr, 4)

  ### Expected vs observed coin issuance
  # Clean up actual coin issuance (dcr_d_actual_coin_issuance)
  dcr_d_actual_coin_issuance$day <- as.Date(as.character(dcr_d_actual_coin_issuance$day), format = "%Y-%m-%d")
  dcr_d_actual_coin_issuance <- subset(dcr_d_actual_coin_issuance, day >= str_initial_date & day <= str_final_date)
  dcr_d_actual_coin_issuance$sum_total_reward <- as.numeric(dcr_d_actual_coin_issuance$sum_total_reward)
  dcr_d_actual_coin_issuance$sum_reward <- as.numeric(dcr_d_actual_coin_issuance$sum_reward)
  dcr_d_actual_coin_issuance$day <- as.Date(as.character(dcr_d_actual_coin_issuance$day), format = "%Y-%m-%d") # Yes, again!
  dcr_d_actual_coin_issuance <<- dcr_d_actual_coin_issuance

  # Clean up expected coin issuance (dcr_d_expected_coin_issuance)
  dcr_d_expected_coin_issuance <- subset(dcr_b_expected_coin_issuance, select=-c(date, block_height, pow_reward, pos_reward, net_fund))
  dcr_d_expected_coin_issuance$day <- as.Date(as.character(dcr_d_expected_coin_issuance$day), format = "%Y-%m-%d")
  dcr_d_expected_coin_issuance <- subset(dcr_d_expected_coin_issuance, day >= str_initial_date & day <= str_final_date)
  dcr_d_expected_coin_issuance <- aggregate(dcr_b_expected_coin_issuance["total_block_reward"], by=dcr_b_expected_coin_issuance["day"], sum)
  dcr_d_expected_coin_issuance <- within(dcr_d_expected_coin_issuance, acc_sum <- cumsum(total_block_reward))
  dcr_d_expected_coin_issuance$total_block_reward <- as.numeric(dcr_d_expected_coin_issuance$total_block_reward)
  dcr_d_expected_coin_issuance$acc_sum <- as.numeric(dcr_d_expected_coin_issuance$acc_sum)
  dcr_d_expected_coin_issuance$day <- as.Date(as.character(dcr_d_expected_coin_issuance$day), format = "%Y-%m-%d") # Yes, again!
  dcr_d_expected_coin_issuance <<- dcr_d_expected_coin_issuance

  # Merge both dataframes in dcr_d_expected_actual_issuance
  dcr_d_expected_actual_issuance <- merge(dcr_d_expected_coin_issuance, dcr_d_actual_coin_issuance, by = "day", all = TRUE)
  colnames(dcr_d_expected_actual_issuance)[2] <- "expected_total_daily_reward"
  colnames(dcr_d_expected_actual_issuance)[3] <- "acc_expected_daily_reward"
  colnames(dcr_d_expected_actual_issuance)[4] <- "actual_total_daily_reward"
  colnames(dcr_d_expected_actual_issuance)[5] <- "acc_actual_daily_reward"
  dcr_d_expected_actual_issuance$day <- as.Date(as.character(dcr_d_expected_actual_issuance$day))
  dcr_d_expected_actual_issuance$expected_total_daily_reward <- as.numeric(dcr_d_expected_actual_issuance$expected_total_daily_reward)
  dcr_d_expected_actual_issuance$acc_expected_daily_reward <- as.numeric(dcr_d_expected_actual_issuance$acc_expected_daily_reward)
  dcr_d_expected_actual_issuance$actual_total_daily_reward <- as.numeric(dcr_d_expected_actual_issuance$actual_total_daily_reward)
  dcr_d_expected_actual_issuance$acc_actual_daily_reward <- as.numeric(dcr_d_expected_actual_issuance$acc_actual_daily_reward)
  dcr_d_expected_actual_issuance <<- dcr_d_expected_actual_issuance

  ### Calculate average blocks per day
  dcr_d_block_time$day <- as.Date(as.character(dcr_d_block_time$day), format = "%Y-%m-%d")
  dcr_d_block_time <- subset(dcr_d_block_time, day >= str_initial_date & day <= str_final_date)
  rownames(dcr_d_block_time) <- 1:nrow(dcr_d_block_time)
  dcr_d_block_time <<- dcr_d_block_time
  avg_block_per_day <<- sum(dcr_d_block_time$block_count) / nrow(dcr_d_block_time)

  ### Calculate circulation and staked in dcr_d_circ_staked
  # Source for this table: https://dcrdata.decred.org/api/chart/stake-participation?axis=time&bin=day
  # Source is a JSON file: has to be 'joined' as a CSV
  # Then need to sum 'day' with base date 2016-02-08 and remove dates out of range to avoid errors when joining columns
  dcr_d_circ_staked['new_day'] = dcr_d_circ_staked$day + as.Date(str_genesis_date, format = "%Y-%m-%d")
  dcr_d_circ_staked['day'] <- dcr_d_circ_staked['new_day']
  dcr_d_circ_staked <- subset(dcr_d_circ_staked, select=-c(new_day))
  dcr_d_circ_staked$day <- as.Date(as.character(dcr_d_circ_staked$day), format = "%Y-%m-%d")
  dcr_d_circ_staked <- subset(dcr_d_circ_staked, day >= str_initial_date & day <= str_final_date)
  # Calculate percentage of staked coins, then divide by 100000000 to remove the 8 decimal places not shown in original
  dcr_d_circ_staked$circulation_coins = as.numeric(dcr_d_circ_staked$circulation_coins / 100000000)
  dcr_d_circ_staked$staked_coins = as.numeric(dcr_d_circ_staked$staked_coins/ 100000000)
  dcr_d_circ_staked$perc_staked = as.numeric(dcr_d_circ_staked$staked_coins / dcr_d_circ_staked$circulation_coins)
  rownames(dcr_d_circ_staked) <- 1:nrow(dcr_d_circ_staked)
  dcr_d_circ_staked <<- dcr_d_circ_staked

  ### Calculate privae transactions in dcr_d_privacy_mix
  # Source for this table: https://dcrdata.decred.org/api/chart/privacy-participation?axis=time&bin=day
  # Source is a JSON file and was manually 'converted' to CSV
  # Then need to sum 'day' with base date 2016-02-08 and remove dates out of range to avoid errors when joining columns
  dcr_d_privacy_mix['new_day'] = dcr_d_privacy_mix$day + as.Date(str_genesis_date)
  dcr_d_privacy_mix['day'] <- dcr_d_privacy_mix['new_day']
  dcr_d_privacy_mix <- subset(dcr_d_privacy_mix, select=-c(new_day))
  dcr_d_privacy_mix$day <- as.Date(as.character(dcr_d_privacy_mix$day), format = "%Y-%m-%d")
  dcr_d_privacy_mix$private_mix_rate <- as.numeric(as.character(dcr_d_privacy_mix$private_mix_rate))
  dcr_d_privacy_mix$private_mix_rate <- round(dcr_d_privacy_mix$private_mix_rate / 100000000, 0)
  dcr_d_privacy_mix <- subset(dcr_d_privacy_mix, day >= str_privacy_initial_date & day <= str_privacy_final_date)
  rownames(dcr_d_privacy_mix) <- 1:nrow(dcr_d_privacy_mix)
  dcr_d_privacy_mix <<- dcr_d_privacy_mix

  ### Calculate voting related indicators
  # Average ticket pool size :: clean up dcr_d_voters_difficulty
  dcr_d_voters_difficulty$day <- as.Date(as.character(dcr_d_voters_difficulty$day), format = "%Y-%m-%d")
  dcr_d_voters_difficulty$avg_block_voters = as.numeric(dcr_d_voters_difficulty$avg_block_voters)
  dcr_d_voters_difficulty$avg_block_poolsize = as.numeric(dcr_d_voters_difficulty$avg_block_poolsize)
  dcr_d_voters_difficulty$avg_block_difficulty = as.numeric(dcr_d_voters_difficulty$avg_block_difficulty)
  dcr_d_voters_difficulty$avg_hashrate = as.numeric(dcr_d_voters_difficulty$avg_hashrate)
  dcr_d_voters_difficulty <- subset(dcr_d_voters_difficulty, day >= str_genesis_date & day <= str_final_date)
  rownames(dcr_d_voters_difficulty) <- 1:nrow(dcr_d_voters_difficulty)
  dcr_d_voters_difficulty <<- dcr_d_voters_difficulty
  avg_ticket_poolsize <<- as.numeric(sum(dcr_d_voters_difficulty$avg_block_poolsize) / nrow(dcr_d_voters_difficulty))

  # Voters per block (count)
  dcr_g_voters_block_count$time <- as.Date(as.character(dcr_g_voters_block_count$time), format = "%Y-%m-%d %H:%M:%S")
  dcr_g_voters_block_count <- subset(dcr_g_voters_block_count, time >= str_genesis_date & time <= str_final_date)
  dcr_g_voters_block_count_summary <- ddply(dcr_g_voters_block_count, ~voters_per_block, summarise, count=length(voters_per_block))   # Needs library 'plyr'
  dcr_g_voters_block_count_summary['perc_block'] <- round(dcr_g_voters_block_count_summary$count / sum(dcr_g_voters_block_count_summary$count), 4)
  dcr_g_voters_block_count <<- dcr_g_voters_block_count
  dcr_g_voters_block_count_summary <<- dcr_g_voters_block_count_summary

  # Voting quorum analysis (off-chain)
  dcr_g_vote_total_number$total = as.numeric(dcr_g_vote_total_number$total)
  dcr_g_vote_total_number$approx_turnout <- as.numeric((dcr_g_vote_total_number$yes + dcr_g_vote_total_number$no) / avg_ticket_poolsize)
  dcr_g_vote_total_number <- dcr_g_vote_total_number[order(dcr_g_vote_total_number$year),]
  dcr_g_vote_total_number$quorum <- ifelse(dcr_g_vote_total_number$approx_turnout > 0.19, "yes", "no")
  dcr_g_vote_total_number$approved_perc = as.numeric(dcr_g_vote_total_number$yes / dcr_g_vote_total_number$total)
  dcr_g_vote_total_number$approved <- ifelse(dcr_g_vote_total_number$approved_perc >= 0.6, "yes", "no")
  avg_proposal_turnout <<- as.numeric(sum(dcr_g_vote_total_number$approx_turnout) / nrow(dcr_g_vote_total_number))
  avg_proposal_approval <<- as.numeric(sum(ifelse(dcr_g_vote_total_number$approved == "yes", 1, 0) / nrow(dcr_g_vote_total_number)))

  # Vote result change during voting days (off-chain)
  dcr_g_vote_change$day <- as.Date(as.character(dcr_g_vote_change$day), format = "%Y-%m-%d")
  dcr_g_vote_change$token <- as.character(dcr_g_vote_change$token)
  dcr_g_vote_change_summary <- ddply(dcr_g_vote_change, ~token, summarise, distinct_days=length(unique(day)))   # Needs library 'plyr'
  for(t in unique(dcr_g_vote_change$token)) {
    for(d in unique(dcr_g_vote_change$day)) {
      dcr_g_vote_change$day_winner <- ifelse(dcr_g_vote_change$yes > dcr_g_vote_change$no, "yes", "no")
    }
  }
  dcr_g_vote_change_summary <- ddply(dcr_g_vote_change, ~token, summarise, winner = length(unique(day_winner)))   # Needs library 'plyr'
  dcr_g_vote_change_summary_count <- ddply(dcr_g_vote_change_summary, ~winner, summarise, count = length(winner))   # Needs library 'plyr'
  dcr_g_vote_change <<- dcr_g_vote_change
  dcr_g_vote_change_summary <<- dcr_g_vote_change_summary
  dcr_g_vote_change_summary_count <<- dcr_g_vote_change_summary_count

  ### Agendas voting results (on-chain)
  dcr_g_agendas_vote_results$total <- as.numeric(dcr_g_agendas_vote_results$abs + dcr_g_agendas_vote_results$yes + dcr_g_agendas_vote_results$no)
  dcr_g_agendas_vote_results$abs_perc <- as.numeric(dcr_g_agendas_vote_results$abs / dcr_g_agendas_vote_results$total)
  dcr_g_agendas_vote_results$no_perc <- as.numeric(dcr_g_agendas_vote_results$no / dcr_g_agendas_vote_results$total)
  dcr_g_agendas_vote_results$yes_perc <- as.numeric(dcr_g_agendas_vote_results$yes / dcr_g_agendas_vote_results$total)
  dcr_g_agendas_vote_results <<- dcr_g_agendas_vote_results
  # I'm not keeping the next column
  dcr_g_agendas_vote_results$approx_turnout <- as.numeric((dcr_g_agendas_vote_results$yes + dcr_g_agendas_vote_results$no) / avg_ticket_poolsize)
  avg_agendas_turnout <<- as.numeric(sum(dcr_g_agendas_vote_results$approx_turnout) / nrow(dcr_g_agendas_vote_results))
  avg_agendas_approval <<- as.numeric(sum(dcr_g_agendas_vote_results$yes_perc) / nrow(dcr_g_agendas_vote_results))

  ### Calculate frequency in days of drawn tickets
  dcr_t_ticket_draw['days_to_draw'] = as.numeric((dcr_t_ticket_draw$height - dcr_t_ticket_draw$block_height) / avg_block_per_day)
  dcr_t_ticket_draw['days_to_draw_roundup'] = as.numeric(ceiling(dcr_t_ticket_draw$days_to_draw))
  dcr_t_ticket_draw['return'] = as.numeric(dcr_t_ticket_draw$vote_reward / dcr_t_ticket_draw$ticket_price)
  dcr_t_ticket_draw <- subset(dcr_t_ticket_draw, select=-c(ticket_hash))
  dcr_t_ticket_draw <<- dcr_t_ticket_draw

  # Calc expired tickets
  # Expired tickets = Revoked tickets - N blocks with 3 votes * 2 - N blocks with 4 votes
  # Expired tickets % = Expired tickets / (N tickets drawn + Expired tickets)
  expired_tickets <<- (nrow(dcr_t_ticket_revoked) - (dcr_g_voters_block_count_summary[2,2] * 2) - dcr_g_voters_block_count_summary[3,2])
  expired_tickets_perc <<- expired_tickets / (nrow(dcr_t_ticket_draw) + expired_tickets)

  # Creating a new dataframe to hold ticket frequency information (needs library 'plyr'):
  ticket_draw_freq <- count(dcr_t_ticket_draw, days_to_draw_roundup)
  # Add to the dataframe the expired tickets above as last row
  ticket_draw_freq[nrow(ticket_draw_freq) + 1,] <- c(144, expired_tickets)
  ticket_draw_freq <- within(ticket_draw_freq, n_acc_sum <- cumsum(n))
  ticket_draw_freq['perc_freq'] = ticket_draw_freq$n / sum(ticket_draw_freq$n)
  ticket_draw_freq <- within(ticket_draw_freq, perc_acc_sum <- cumsum(perc_freq))
  ticket_draw_freq <<- ticket_draw_freq

  # Clean up daily transaction count and volume (dcr_d_transaction_volume), used in calc_econ_model()
  dcr_d_transaction_volume$day <- as.Date(as.character(dcr_d_transaction_volume$day), format = "%Y-%m-%d")
  dcr_d_transaction_volume$tx_count = as.numeric(dcr_d_transaction_volume$tx_count)
  dcr_d_transaction_volume$sum_sent = as.numeric(dcr_d_transaction_volume$sum_sent)
  dcr_d_transaction_volume$sum_fees = as.numeric(dcr_d_transaction_volume$sum_fees)
  dcr_d_transaction_volume <- subset(dcr_d_transaction_volume, day >= str_genesis_date & day <= str_final_date)
  rownames(dcr_d_transaction_volume) <- 1:nrow(dcr_d_transaction_volume)
  dcr_d_transaction_volume <<- dcr_d_transaction_volume

  ### DCR and BTC prices and difficulty
  # Source DCR-USD: https://finance.yahoo.com/quote/DCR-USD/history?period1=1454716800&period2=1586131200&interval=1d&filter=history&frequency=1d
  # DCR daily prices in USD
  dcr_usd_price <- read.table(paste0(str_basedir, "DCR-USD.csv"), header = TRUE, sep = ",")
  dcr_usd_price <- subset(dcr_usd_price, select=-c(Open, High, Low, Adj.Close, Volume))
  dcr_usd_price$Date <- as.Date(as.character(dcr_usd_price$Date), format = "%Y-%m-%d")
  dcr_usd_price <- subset(dcr_usd_price, Date >= str_initial_date & Date <= str_final_date)
  # Although the URL above specified the beginning of the period as 06-02-2016, there is no data for the first two days do Decred
  # Probably because it wasn't being traded in USD
  dcr_usd_price <- rbind(data.frame(Date = as.Date("2016-02-09", format = "%Y-%m-%d"), Close = 0), dcr_usd_price)
  dcr_usd_price <- rbind(data.frame(Date = as.Date("2016-02-08", format = "%Y-%m-%d"), Close = 0), dcr_usd_price)
  # Continue processing the table
  dcr_usd_price$Close <- as.numeric(as.character(dcr_usd_price$Close))
  colnames(dcr_usd_price)[1] <- "day"
  colnames(dcr_usd_price)[2] <- "dcr_price_close"
  dcr_usd_price <<- dcr_usd_price

  # Source BTC-USD: https://finance.yahoo.com/quote/BTC-USD/history?period1=1454716800&period2=1586131200&interval=1d&filter=history&frequency=1d
  # BTC, merging USD prices with network difficulty
  # Source BTC difficulty: https://www.blockchain.com/charts/difficulty
  btc_usd_price <- read.table(paste0(str_basedir, "BTC-USD.csv"), header = TRUE, sep = ",")
  btc_usd_price$Date <- as.Date(as.character(btc_usd_price$Date), format = "%Y-%m-%d")
  btc_usd_price <- subset(btc_usd_price, select=-c(Open, High, Low, Adj.Close, Volume))
  btc_difficulty <- read.table(paste0(str_basedir, "difficulty.csv"), header = TRUE, sep = ",")
  btc_difficulty$X...Timestamp <- format(as.POSIXct(btc_difficulty$X...Timestamp, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d")
  colnames(btc_difficulty)[1] <- "day"
  btc_table <- merge(btc_usd_price, btc_difficulty, by.x = c("Date"), by.y = c("day"), all = TRUE)
  btc_table$Date <- as.Date(btc_table$Date, format = "%Y-%m-%d")
  btc_table$difficulty <- na.locf(btc_table$difficulty)
  btc_table <- subset(btc_table, Date >= str_initial_date & Date <= str_final_date)
  btc_table <- btc_table[complete.cases(btc_table), ]
  colnames(btc_usd_price)[1] <- "day"
  colnames(btc_usd_price)[2] <- "btc_price_close"
  colnames(btc_table)[1] <- "day"
  colnames(btc_table)[2] <- "btc_price_close"
  colnames(btc_table)[3] <- "btc_difficulty"
  rownames(btc_table) <- 1:nrow(btc_table)
  btc_table <<- btc_table
  btc_usd_price <<- btc_usd_price

  # Decred financial return per ticket, in average
  dcr_t_return_usd <- subset(dcr_t_ticket_draw, select=c(block_time, ticket_price, vote_reward, return))
  dcr_t_return_usd$block_time <- format(as.POSIXct(dcr_t_return_usd$block_time, format = '%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d')
  colnames(dcr_t_return_usd)[1] <- "day"
  dcr_t_return_usd$day <- as.Date(as.character(dcr_t_return_usd$day), format = "%Y-%m-%d")
  dcr_t_return_usd <- merge(dcr_t_return_usd, dcr_usd_price, by = c("day"), all = TRUE)
  dcr_t_return_usd <- subset(dcr_t_return_usd, day >= "2016-02-21" & day <= str_final_date)
  dcr_t_return_usd$return_usd <- as.numeric(dcr_t_return_usd$return * dcr_t_return_usd$dcr_price_close)
  dcr_t_return_usd_aggregate <- aggregate(dcr_t_return_usd, by = list(dcr_t_return_usd$day), FUN = mean)
  dcr_t_return_usd_aggregate <- subset(dcr_t_return_usd_aggregate, select=-c(Group.1))
  dcr_t_return_usd_aggregate <<- dcr_t_return_usd_aggregate

  # # Format "day" columns before merging dataframes
  # dcr_d_actual_coin_issuance <- dcr_d_actual_coin_issuance[1:nrow(dcr_d_fees_diff_tx),]
  # dcr_d_actual_coin_issuance$day <- as.Date(as.character(dcr_d_actual_coin_issuance$day), format = "%Y-%m-%d")

  ### Clean up dcr_d_fees_diff_tx (hip 3)
  dcr_d_fees_diff_tx$day <- as.Date(as.character(dcr_d_fees_diff_tx$day), format = "%Y-%m-%d")
  dcr_d_fees_diff_tx$sum_fees <- as.numeric(as.character(dcr_d_fees_diff_tx$sum_fees))
  dcr_d_fees_diff_tx$difficulty <- as.numeric(dcr_d_fees_diff_tx$difficulty)
  dcr_d_fees_diff_tx$count_tx <- as.numeric(dcr_d_fees_diff_tx$count_tx)
  colnames(dcr_d_fees_diff_tx)[3] <- "dcr_avg_difficulty"
  dcr_d_fees_diff_tx <- subset(dcr_d_fees_diff_tx, day >= str_initial_date & day <= str_final_date)
  rownames(dcr_d_fees_diff_tx) <- 1:nrow(dcr_d_fees_diff_tx)
  dcr_d_fees_diff_tx <<- dcr_d_fees_diff_tx

  # Format dcr_d_ticket_price_pool_size (it was impossible to convert 'day' to Format Date, this is an ugly functional workaround)
  dcr_d_ticket_price_pool_size$new_day <- as.Date(as.character(dcr_d_ticket_price_pool_size$day), format = "%Y-%m-%d")
  dcr_d_ticket_price_pool_size$day <- as.Date(as.character(dcr_d_ticket_price_pool_size$new_day), format = "%Y-%m-%d")
  dcr_d_ticket_price_pool_size <- subset(dcr_d_ticket_price_pool_size, select=-c(new_day))
  dcr_d_ticket_price_pool_size <- subset(dcr_d_ticket_price_pool_size, day >= str_genesis_date & day <= str_final_date)
  dcr_d_ticket_price_pool_size$avg_price <- as.numeric(dcr_d_ticket_price_pool_size$avg_price)
  dcr_d_ticket_price_pool_size$avg_pool <- as.numeric(dcr_d_ticket_price_pool_size$avg_pool)
  dcr_d_ticket_price_pool_size$avg_pool_dcr <- as.numeric(dcr_d_ticket_price_pool_size$avg_pool_dcr)
  dcr_d_ticket_price_pool_size <<- dcr_d_ticket_price_pool_size

  #dcr_d_network_funding$day <- as.Date(as.character(dcr_d_network_funding$day), format = "%Y-%m-%d")
  #dcr_d_pos_rewards$day <- as.Date(as.character(dcr_d_pos_rewards$day), format = "%Y-%m-%d")
  #dcr_d_transaction_volume$day <- as.Date(as.character(dcr_d_transaction_volume$day), format = "%Y-%m-%d")

  append_csv_results(dcr_g_voters_block_count_summary)  # Write mode, no append, to reset the file
  append_text_results(paste("\nAverage block per day:", round(avg_block_per_day, 4)))
  append_text_results(paste("Average daily ticket pool size:", round(avg_ticket_poolsize, 4)))
}

gen_dcr_exp_issuance <- function() {
  # This function generates a dataframe with the 
  # expected Decred issuance according to consensus rules
  premined_dcrs <- 1680000
  blocks_until_reduction <- 6144
  blocks_without_ticket_draw <- 4096  # 0 - 4095
  total_reductions <- 399   # 0 - 399
  base_block_reward <- 31.19582664
  reduction_factor <- 100/101

  dcr_exp_issuance <- data.frame(date = seq(from = posixct_dcr_genesis_date, to = posixct_dcr_final_date, by = as.difftime("00:05:00", format = "%H:%M:%S")))
  dcr_exp_issuance$day <- as.Date(dcr_exp_issuance$date, format = "%Y-%m-%d")

  # Block 0
  total_reward_vector <- c(rep(c(0), times = 1))

  # Block 1
  total_reward_vector <- c(total_reward_vector, rep(c(premined_dcrs), times = 1))

  # Blocks 2:4095
  total_reward <- base_block_reward * 0.7   # PoW + Network
  total_reward_vector <- c(total_reward_vector, rep(c(total_reward), times = 4095 - 2))

  # Blocks 4095:6143
  total_reward <- base_block_reward   # PoW + PoS + Network
  total_reward_vector <- c(total_reward_vector, rep(c(total_reward), times = 6143 - 4095))

  for(n_total_reductions in 1:total_reductions) {
    total_reward <- base_block_reward * (reduction_factor ** n_total_reductions)
    total_reward_vector <- c(total_reward_vector, rep(c(total_reward), times = blocks_until_reduction))
    if(length(total_reward_vector) > nrow(dcr_exp_issuance)) {
      total_reward_vector <- head(total_reward_vector, nrow(dcr_exp_issuance))
      break
    }
  }

  if(length(total_reward_vector) < nrow(dcr_exp_issuance)) {
    dcr_exp_issuance <- dcr_exp_issuance[1:length(total_reward_vector),]
  }

  dcr_exp_issuance$expected_total_reward <- total_reward_vector
  dcr_exp_issuance <<- dcr_exp_issuance
  dcr_exp_issuance_by_day <- aggregate(dcr_exp_issuance$expected_total_reward, by = dcr_exp_issuance["day"], sum)
  dcr_exp_issuance_by_day <- within(dcr_exp_issuance_by_day, acc_sum <- cumsum(x))
  dcr_exp_issuance_by_day$x <- as.numeric(dcr_exp_issuance_by_day$x)
  dcr_exp_issuance_by_day$acc_sum <- as.numeric(dcr_exp_issuance_by_day$acc_sum)
  dcr_exp_issuance_by_day <<- dcr_exp_issuance_by_day
  # This dataframe will be used in gen_graph_btc_dcr_comparison()
}

gen_btc_exp_issuance <- function() {
  # This function generates a dataframe with the 
  # expected Bitcoin issuance according to consensus rules
  btc_exp_issuance <- data.frame(date = seq(from = posixct_btc_genesis_date, to = posixct_dcr_final_date, by = as.difftime("00:10:00", format = "%H:%M:%S")))
  btc_exp_issuance$day <- as.Date(btc_exp_issuance$date, format = "%Y-%m-%d")

  blocks_until_reduction <- 210000
  total_reductions <- 33   # 0 - 32
  base_block_reward <- 50
  n_total_reductions <- 0

  total_reward_vector <- c()
  for(n_total_reductions in 0:total_reductions) {
    total_reward <- base_block_reward / (2 ** n_total_reductions)
    total_reward_vector <- c(total_reward_vector, rep(c(total_reward), times = blocks_until_reduction))
    if(length(total_reward_vector) > nrow(btc_exp_issuance)) {
      total_reward_vector <- head(total_reward_vector, nrow(btc_exp_issuance))
      break
    }
  }

  btc_exp_issuance$expected_total_reward <- total_reward_vector
  # btc_exp_issuance <<- btc_exp_issuance
  btc_exp_issuance_by_day <- aggregate(btc_exp_issuance$expected_total_reward, by=btc_exp_issuance["day"], sum)
  btc_exp_issuance_by_day <- within(btc_exp_issuance_by_day, acc_sum <- cumsum(x))
  btc_exp_issuance_by_day$x <- as.numeric(btc_exp_issuance_by_day$x)
  btc_exp_issuance_by_day$acc_sum <- as.numeric(btc_exp_issuance_by_day$acc_sum)
  btc_exp_issuance_by_day <<- btc_exp_issuance_by_day
  # This dataframe will be used in gen_graph_btc_dcr_comparison()
}

df_expected_dcr_coin_issuance <- function() {
  ### Function to generate expected issuance dataframe
  # This function needs an upgrade to replicate those 6144 blocks

  if (exists("dcr_d_expected_coin_issuance")) {
    return("No need to run. Dataframe dcr_d_expected_coin_issuance has already been loaded.")
  }
  print(paste("This will take a few hours to complete...", expected_total_block_reward))
  premined_dcrs <- 1680000
  blocks_until_reduction <- 6144
  blocks_without_ticket_draw <- 4096  # 0 - 4095
  total_reductions <- 100   # 0 - 399, but only 100 here because it's already past the assessment period
  base_block_reward <- 31.19582664
  reduction_factor <- 100/101
  n_blocks_until_reduction <- 1
  n_total_reductions <- 0
  block_height <- 0

  df_date <- data.frame(date = seq(from = posixct_genesis_date, to = posixct_final_date, by = as.difftime("00:05:00", format = "%H:%M:%S")))
  df_date$day <- as.Date(df_date$date, format = "%Y-%m-%d")
  df_date$block_height <- as.numeric(seq.int(from = 0, to = nrow(df_date) - 1))

  df_expected <<- data.frame(
                            total_block_reward = numeric(),
                            pow_reward = numeric(),
                            pos_reward = numeric(),
                            net_fund = numeric(),
                            stringsAsFactors = FALSE)
  
  # Genesis (block 0)
  total_block_reward <- as.numeric(0)
  pow_reward <- as.numeric(0)
  pos_reward <- as.numeric(0)
  net_fund <- as.numeric(0)
  pow_reward <- as.numeric(0)
  df_expected[nrow(df_expected) + 1,] = c(total_block_reward, pow_reward, pos_reward, net_fund)
  n_blocks_until_reduction <- n_blocks_until_reduction + 1
  
  # Block 1
  total_block_reward <- as.numeric(premined_dcrs)
  pow_reward <- as.numeric(premined_dcrs)
  df_expected[nrow(df_expected) + 1,] = c(total_block_reward, pow_reward, pos_reward, net_fund)
  n_blocks_until_reduction <- n_blocks_until_reduction + 1

  # Block 2 - 6143 (n = 6142)
  expected_total_block_reward <- as.numeric(base_block_reward * (reduction_factor ** n_total_reductions))
  pow_reward <- as.numeric(round(expected_total_block_reward * 0.6, 8))
  pos_reward <- as.numeric(ifelse(n_total_reductions > 0 || (n_blocks_until_reduction >= blocks_without_ticket_draw && n_total_reductions == 0), round(expected_total_block_reward * 0.3, 8), 0))
  net_fund <- as.numeric(round(expected_total_block_reward * 0.1, 8))
  print(paste("expected_total_block_reward:", expected_total_block_reward))
  print(paste("pow_reward", pow_reward))
  print(paste("pos_reward", pos_reward))
  print(paste("net_fund", net_fund))
  total_block_reward <- as.numeric(round(pow_reward + pos_reward + net_fund, 8))

  for(n_blocks_until_reduction in 2:6143) {
    pos_reward <- as.numeric(ifelse(n_total_reductions > 0 || (n_blocks_until_reduction >= blocks_without_ticket_draw && n_total_reductions == 0), round(expected_total_block_reward * 0.3, 8), 0))
    total_block_reward <- as.numeric(round(pow_reward + pos_reward + net_fund, 8))
    df_expected[nrow(df_expected) + 1,] = c(total_block_reward, pow_reward, pos_reward, net_fund)
    n_blocks_until_reduction <- n_blocks_until_reduction + 1
  }
  print(paste("df_expected_dcr_coin_issuance(): generated block height:", blocks_until_reduction * 1))
  df_expected <<- df_expected

  # Block 6144 onwards
  for(n_total_reductions in 1:total_reductions) {
    # Same calc for next 6144 blocks
    expected_total_block_reward <- as.numeric(base_block_reward * (reduction_factor ** n_total_reductions))
    pow_reward <- as.numeric(round(expected_total_block_reward * 0.6, 8))
    pos_reward <- as.numeric(ifelse(n_total_reductions > 0 || (n_blocks_until_reduction >= blocks_without_ticket_draw && n_total_reductions == 0), round(expected_total_block_reward * 0.3, 8), 0))
    net_fund <- as.numeric(round(expected_total_block_reward * 0.1, 8))
    print(paste("expected_total_block_reward:", expected_total_block_reward))
    print(paste("pow_reward", pow_reward))
    print(paste("pos_reward", pos_reward))
    print(paste("net_fund", net_fund))
    total_block_reward <- as.numeric(round(pow_reward + pos_reward + net_fund, 8))

    for(n_blocks_until_reduction in 0:blocks_until_reduction) {
      df_expected[nrow(df_expected) + 1,] = c(total_block_reward, pow_reward, pos_reward, net_fund)
    }
    print(paste("df_expected_dcr_coin_issuance(): generated block height:", blocks_until_reduction * (n_total_reductions + 1)))
    (blocks_until_reduction * (n_total_reductions + 1) > 450000) && break   # No need to produce the whole estimate (until 2039)
  }
  df_expected <- within(df_expected, acc_sum <- cumsum(total_block_reward))
  df_expected <<- df_expected

  print("df_expected_dcr_coin_issuance(): DONE")
  write.csv(df_expected, paste0(str_basedir, "dcr-expected-block-coin_issuance.csv"), row.names = FALSE)
  df_expected2 <- df_expected[1:nrow(df_date),]
  df_date$total_block_reward <- df_expected2$total_block_reward
  df_date$pow_reward <- df_expected2$pow_reward
  df_date$pos_reward <- df_expected2$pos_reward
  df_date$net_fund <- df_expected2$net_fund
  df_date$acc_sum <- df_expected2$acc_sum
  write.csv(df_date, paste0(str_basedir, "dcr-expected-block-date_and_coin_issuance.csv"), row.names = FALSE)
  df_dcr_expected_coin_issuance <- df_date
  dcr_d_expected_coin_issuance <- aggregate(df_date["total_block_reward"], by = df_date["day"], sum)
  dcr_d_expected_coin_issuance <- within(dcr_d_expected_coin_issuance, acc_sum <- cumsum(total_block_reward))
  colnames(dcr_d_expected_coin_issuance)[2] <- "expected_total_reward"
  colnames(dcr_d_expected_coin_issuance)[3] <- "acc_expected_reward"
}

### Definition of Hipotheses
# Hip. 1: Decred in tickets is not related to coin issuance
calc_hip1 <- function() {
  hip1 <- dcr_d_circ_staked
  hip1_corr <- with(hip1, cor(circulation_coins, perc_staked))
  hip1_chisq <- chisq.test(hip1[,-1])
  hip1_corr <<- hip1_corr
  hip1_chisq <<- hip1_chisq
  hip1 <<- hip1

  str_hip1_cut_date <- "2018-02-01"
  hip1_part1 <- subset(hip1, day >= str_genesis_date & day < str_hip1_cut_date)
  hip1_part2 <- subset(hip1, day >= str_hip1_cut_date & day <= str_final_date)

  hip1_part1_corr <- with(hip1_part1, cor(as.numeric(circulation_coins), as.numeric(perc_staked)))
  hip1_part1_chisq <- chisq.test(hip1_part1[,-1])
  hip1_part2_corr <- with(hip1_part2, cor(as.numeric(circulation_coins), as.numeric(perc_staked)))
  hip1_part2_chisq <- chisq.test(hip1_part2[,-1])
  hip1_part1_corr <<- hip1_part1_corr
  hip1_part1_chisq <<- hip1_part1_chisq
  hip1_part2_corr <<- hip1_part2_corr
  hip1_part2_chisq <<- hip1_part2_chisq

  append_text_results(paste("Hip. 1 N:", nrow(hip1)))
  append_text_results(paste("Hip. 1 Correlation:", round(hip1_corr, 4)))
  append_text_results(paste("Hip. 1 Chi-Sq: X-squared:", gsub("[\r\n]", " ", hip1_chisq$statistic), "p-value:", hip1_chisq$p.value, "\n"))
}

# Hip. 2: DCR liquidity is not related to price fall
calc_hip2 <- function() {
  hip2 <- merge(x = dcr_usd_price, y = dcr_d_circ_staked[ , c("day", "perc_staked")], by = "day", all.x = TRUE)
  hip2 <- subset(hip2, day >= "2016-02-10" & day <= str_final_date)   # chisq.test() returns error with 0 values in sample
  hip2$dcr_price_close <- as.numeric(hip2$dcr_price_close)
  hip2$perc_staked <- as.numeric(hip2$perc_staked)
  rownames(hip2) <- 1:nrow(hip2)
  hip2_corr <- with(hip2, cor(as.numeric(dcr_price_close), as.numeric(perc_staked)))
  hip2_chisq <- chisq.test(hip2[,-1])
  hip2_corr <<- hip2_corr
  hip2_chisq <<- hip2_chisq
  hip2 <<- hip2

  str_hip2_cut_date <- "2017-02-10"
  hip2_part2 <- subset(hip2, day >= str_hip2_cut_date & day <= str_final_date)
  rownames(hip2_part2) <- 1:nrow(hip2_part2)
  hip2_part2_corr <- with(hip2_part2, cor(as.numeric(dcr_price_close), as.numeric(perc_staked)))
  hip2_part2_chisq <- chisq.test(hip2_part2[,-1])
  hip2_part2_corr <<- hip2_part2_corr
  hip2_part2_chisq <<- hip2_part2_chisq

  # Calculation for financial return
  str_hip2_fin_start_date <- "2016-02-21"  # When financial returns started
  str_hip2_fin_cut_date <- "2017-02-21"    # On eyear after financial returns started to disregard the initial rise on staking from 0
  hip2_fin <- dcr_d_circ_staked
  hip2_fin <- merge(x = hip2_fin, y = dcr_t_return_usd_aggregate[, c("day", "return", "return_usd")], by = "day", all.x = TRUE)
  hip2_fin <- subset(hip2_fin, day >= str_hip2_fin_start_date & day <= str_final_date)
  hip2_fin <- subset(hip2_fin, select=-c(circulation_coins, staked_coins))
  hip2_fin <<- hip2_fin

  hip2_fin_part1 <- subset(hip2_fin, day >= str_genesis_date & day <= str_final_date)
  hip2_fin_part2 <- subset(hip2_fin, day >= str_hip2_fin_cut_date & day <= str_final_date)
  hip2_fin_part1_corr <- with(hip2_fin_part1, cor(return_usd, perc_staked))
  hip2_fin_part1_chisq <- chisq.test(hip2_fin_part1[,-1])
  hip2_fin_part1_corr <<- hip2_fin_part1_corr
  hip2_fin_part1_chisq <<- hip2_fin_part1_chisq
  hip2_fin_part1 <<- hip2_fin_part1
  hip2_fin_part2_corr <- with(hip2_fin_part2, cor(return_usd, perc_staked))
  hip2_fin_part2_chisq <- chisq.test(hip2_fin_part2[,-1])
  hip2_fin_part2_corr <<- hip2_fin_part2_corr
  hip2_fin_part2_chisq <<- hip2_fin_part2_chisq
  hip2_fin_part2 <<- hip2_fin_part2

  append_text_results(paste("Hip. 2 N:", nrow(hip2)))
  append_text_results(paste("Hip. 2 Correlation:", round(hip2_corr, 4)))
  append_text_results(paste("Hip. 2 Chi-Sq: X-squared:", gsub("[\r\n]", " ", hip2_chisq$statistic), "p-value:", hip2_chisq$p.value, "\n"))
}

# Hip. 3: PoW and PoS security are not related to price fall
calc_hip3_dcr <- function() {
  hip3_dcr <- merge(x = dcr_usd_price, y = dcr_d_circ_staked[ , c("day", "perc_staked")], by = "day", all.x = TRUE)
  hip3_dcr <- merge(x = hip3_dcr, y = dcr_d_fees_diff_tx[ , c("day", "dcr_avg_difficulty")], by = "day", all.x = TRUE)
  hip3_dcr <- subset(hip3_dcr, day >= str_initial_date & day <= str_final_date)
  hip3_dcr$dcr_price_close <- as.numeric(hip3_dcr$dcr_price_close)
  hip3_dcr$dcr_avg_difficulty <- as.numeric(hip3_dcr$dcr_avg_difficulty)
  hip3_dcr$perc_staked <- as.numeric(hip3_dcr$perc_staked)
  hip3_dcr_corr <- with(hip3_dcr, cor.test(as.numeric(dcr_price_close), as.numeric(dcr_avg_difficulty)))
  hip3_dcr_chisq <- chisq.test(hip3_dcr[,-1][,-2])
  hip3_dcr_corr <<- hip3_dcr_corr
  hip3_dcr_chisq <<- hip3_dcr_chisq
  hip3_dcr <<- hip3_dcr

  str_hip3_dcr_cut_date <- "2019-01-01"
  hip3_dcr_part1 <- subset(hip3_dcr, day >= str_genesis_date & day < str_hip3_dcr_cut_date)
  hip3_dcr_part2 <- subset(hip3_dcr, day >= str_hip3_dcr_cut_date & day <= str_final_date)

  hip3_dcr_part1_corr <- with(hip3_dcr_part1, cor(as.numeric(dcr_price_close), as.numeric(dcr_avg_difficulty)))
  hip3_dcr_part1_chisq <- chisq.test(hip3_dcr_part1[,2:4][,-2])
  hip3_dcr_part2_corr <- with(hip3_dcr_part2, cor(as.numeric(dcr_price_close), as.numeric(dcr_avg_difficulty)))
  hip3_dcr_part2_chisq <- chisq.test(hip3_dcr_part2[,2:4][,-2])
  hip3_dcr_part1_corr <<- hip3_dcr_part1_corr
  hip3_dcr_part1_chisq <<- hip3_dcr_part1_chisq
  hip3_dcr_part2_corr <<- hip3_dcr_part2_corr
  hip3_dcr_part2_chisq <<- hip3_dcr_part2_chisq

  append_text_results(paste("Hip. 3 DCR N:", nrow(hip3_dcr)))
  append_text_results(paste("Hip. 3 DCR Correlation:", round(as.numeric(as.character(hip3_dcr_corr$estimate)), 4)))
  append_text_results(paste("Hip. 3 DCR Chi-Sq: X-squared:", gsub("[\r\n]", " ", hip3_dcr_chisq$statistic), "p-value:", hip3_dcr_chisq$p.value, "\n"))
}

calc_hip3_btc <- function() {
  hip3_btc <- subset(btc_table, select=c(day, btc_price_close, btc_difficulty))
  hip3_btc_corr <- with(hip3_btc, cor(as.numeric(btc_price_close), as.numeric(btc_difficulty)))
  hip3_btc$btc_price_close <- as.numeric(hip3_btc$btc_price_close)
  hip3_btc_chisq <- chisq.test(hip3_btc[,-1])
  hip3_btc_corr <<- hip3_btc_corr
  hip3_btc_chisq <<- hip3_btc_chisq
  hip3_btc <<- hip3_btc

  str_hip3_btc_cut_date <- "2019-01-01"
  hip3_btc_part1 <- subset(hip3_btc, day >= str_genesis_date & day < str_hip3_btc_cut_date)
  hip3_btc_part2 <- subset(hip3_btc, day >= str_hip3_btc_cut_date & day <= str_final_date)

  hip3_btc_part1_corr <- with(hip3_btc_part1, cor(as.numeric(btc_price_close), as.numeric(btc_difficulty)))
  hip3_btc_part1_chisq <- chisq.test(hip3_btc_part1[,-1])
  hip3_btc_part2_corr <- with(hip3_btc_part2, cor(as.numeric(btc_price_close), as.numeric(btc_difficulty)))
  hip3_btc_part2_chisq <- chisq.test(hip3_btc_part2[,-1])
  hip3_btc_part1_corr <<- hip3_btc_part1_corr
  hip3_btc_part1_chisq <<- hip3_btc_part1_chisq
  hip3_btc_part2_corr <<- hip3_btc_part2_corr
  hip3_btc_part2_chisq <<- hip3_btc_part2_chisq

  append_text_results(paste("Hip. 3 BTC N:", nrow(hip3_btc)))
  append_text_results(paste("Hip. 3 BTC Correlation:", round(hip3_btc_corr, 4)))
  append_text_results(paste("Hip. 3 BTC Chi-Sq: X-squared:", gsub("[\r\n]", " ", hip3_btc_chisq$statistic), "p-value:", hip3_btc_chisq$p.value, "\n"))
}

# Hip. 4: Tickets are drawn as defined in the official documentation (https://docs.decred.org/proof-of-stake/overview/)
# Ref.: https://mgimond.github.io/Stats-in-R/ChiSquare_test.html
calc_hip4 <- function() {
  hip4 <- ticket_draw_freq
  # Cumulative values for 10, 20, 28, 45, 60, 90; expired ticket percentage:
  hip4$perc_acc_sum <- as.numeric(hip4$perc_acc_sum)
  hip4$perc_acc_sum_round <- as.numeric(round(hip4$perc_acc_sum, 3))
  # Expected values from official doc above; last item is expired tickets (source is the above link)
  expected <- c(0.296, 0.505, 0.626, 0.794, 0.879, 0.958, 0.005)
  #actual <- c(hip4[10,5], hip4[20,5], hip4[28,5], hip4[45,5], hip4[60,5], hip4[90,5], round(hip4[144,4], 3))
  #actual <- c(0.273, 0.490, 0.617, 0.790, 0.877, 0.958, 0.007)
  actual <- c(hip4[10,6], hip4[20,6], hip4[28,6], hip4[45,6], hip4[60,6], hip4[90,6], round(hip4[144,4], 3))
  hip4_x = data.frame(actual = as.numeric(actual), expected = as.numeric(expected))
  hip4_corr <- with(hip4_x, cor(as.numeric(actual), as.numeric(expected)))
  hip4_chisq <- chisq.test(hip4_x)
  expected <<- expected
  actual <<- actual
  hip4_corr <<- hip4_corr
  hip4_chisq <<- hip4_chisq
  hip4 <<- hip4

  append_text_results(paste("Hip. 4 N:", length(actual)))
  append_text_results(paste("Hip. 4 Correlation:", round(hip4_corr, 4)))
  append_text_results(paste("Hip. 4 Chi-Sq: X-squared:", gsub("[\r\n]", " ", hip4_chisq$statistic), "p-value:", hip4_chisq$p.value, "\n"))
}

# Hip. 5: Privacy participation is not related to price fall
calc_hip5 <- function() {
  hip5 <- merge(dcr_d_privacy_mix, dcr_usd_price, by.x = c("day"), by.y = c("day"), all = TRUE)
  hip5 <- subset(hip5, day >= str_privacy_initial_date & day <= str_privacy_final_date)
  rownames(hip5) <- 1:nrow(hip5)
  hip5_corr <- with(hip5, cor(as.numeric(dcr_price_close), as.numeric(private_mix_rate)))
  hip5_chisq <- chisq.test(hip5[,-1])
  hip5_corr <<- hip5_corr
  hip5_chisq <<- hip5_chisq
  hip5 <<- hip5

  append_text_results(paste("Hip. 5 N:", nrow(hip5)))
  append_text_results(paste("Hip. 5 Correlation:", round(hip5_corr, 4)))
  append_text_results(paste("Hip. 5 Chi-Sq: X-squared:", gsub("[\r\n]", " ", hip5_chisq$statistic), "p-value:", hip5_chisq$p.value, "\n"))
}

# Hip. 6: Actual coin issuance is related to expected coin issuance
calc_hip6 <- function() {
  hip6 <- dcr_d_expected_actual_issuance
  hip6 <- subset(hip6, day >= str_genesis_date & day <= str_final_date)
  rownames(hip6) <- 1:nrow(hip6)
  hip6_corr <- with(hip6, cor(as.numeric(acc_expected_daily_reward), as.numeric(acc_actual_daily_reward)))
  hip6_chisq <- chisq.test(hip6[,-1])
  hip6_corr <<- hip6_corr
  hip6_chisq <<- hip6_chisq
  hip6 <<- hip6

  append_text_results(paste("Hip. 6", hip6[nrow(hip6),1], "Expected coins:", hip6[nrow(hip6),3], "- Observed coins:", hip6[nrow(hip6),5]))
  append_text_results(paste("Hip. 6 N:", nrow(hip6)))
  append_text_results(paste("Hip. 6 Correlation:", round(hip6_corr, 4)))
  append_text_results(paste("Hip. 6 Chi-Sq: X-squared:", gsub("[\r\n]", " ", hip6_chisq$statistic), "p-value:", hip6_chisq$p.value, "\n"))
}

# Hip. 7: Decred founders' DCR expenditure is related to price variance
calc_hip7 <- function() {
  hip7 <- dcr_b_issuance_pow_reward
  hip7['day'] = as.Date(as.character(dcr_b_issuance_pow_reward$block_time), format = "%Y-%m-%d")
  hip7 <- subset(hip7, select=-c(block_time, tx_type, script_addresses, pow_reward))
  hip7 <- unique(hip7[, 1:2])
  hip7 <- merge(x = hip7, y = dcr_usd_price, by="day")
  hip7 <- merge(x = hip7, y = dcr_1_block_founders_dest, by = "block_height")
  rownames(hip7) <- 1:nrow(hip7)

  # Maximum exchange rate in USD for Decred in 2016
  hip7_max_price_2016 <<- max(subset(dcr_usd_price, day >= str_genesis_date & day <= "2016-12-31")[,2])
  hip7 <<- hip7
}

# Hip. 8: Decred price variance is related to Bitcoin price variance
calc_hip8 <- function() {
  hip8 <- merge(dcr_usd_price, btc_usd_price, by.x = c("day"), by.y = c("day"), all = TRUE)
  hip8 <- subset(hip8, day >= str_genesis_date & day <= str_final_date)
  rownames(hip8) <- 1:nrow(hip8)
  hip8_corr <- with(hip8, cor(as.numeric(dcr_price_close), as.numeric(btc_price_close)))
  hip8_chisq <- chisq.test(hip8[,-1])
  hip8_corr <<- hip8_corr
  hip8_chisq <<- hip8_chisq
  hip8 <<- hip8

  str_hip8_cut_date <- "2018-06-01"
  hip8_part1 <- subset(hip8, day >= str_genesis_date & day < str_hip8_cut_date)
  hip8_part2 <- subset(hip8, day >= str_hip8_cut_date & day <= str_final_date)

  hip8_part1_corr <- with(hip8_part1, cor(as.numeric(dcr_price_close), as.numeric(btc_price_close)))
  hip8_part1_chisq <- chisq.test(hip8_part1[,-1])
  hip8_part2_corr <- with(hip8_part2, cor(as.numeric(dcr_price_close), as.numeric(btc_price_close)))
  hip8_part2_chisq <- chisq.test(hip8_part2[,-1])
  hip8_part1_corr <<- hip8_part1_corr
  hip8_part1_chisq <<- hip8_part1_chisq
  hip8_part2_corr <<- hip8_part2_corr
  hip8_part2_chisq <<- hip8_part2_chisq
  hip8_part1 <<- hip8_part1
  hip8_part2 <<- hip8_part2

  append_text_results(paste("Hip. 8 N:", nrow(hip8)))
  append_text_results(paste("Hip. 8 Correlation:", round(hip8_corr, 4)))
  append_text_results(paste("Hip. 8 Chi-Sq: X-squared:", gsub("[\r\n]", " ", hip8_chisq$statistic), "p-value:", hip8_chisq$p.value, "\n"))

  append_text_results(paste("Hip. 8 Part 1 N:", nrow(hip8_part1)))
  append_text_results(paste("Hip. 8 Part 1 Correlation:", round(hip8_part1_corr, 4)))
  append_text_results(paste("Hip. 8 Part 1 Chi-Sq: X-squared:", gsub("[\r\n]", " ", hip8_part1_chisq$statistic), "p-value:", hip8_part1_chisq$p.value, "\n"))

  append_text_results(paste("Hip. 8 Part 2 N:", nrow(hip8_part2)))
  append_text_results(paste("Hip. 8 Part 2 Correlation:", round(hip8_part2_corr, 4)))
  append_text_results(paste("Hip. 8 Part 2 Chi-Sq: X-squared:", gsub("[\r\n]", " ", hip8_part2_chisq$statistic), "p-value:", hip8_part2_chisq$p.value, "\n"))
}

#### OLS model

# Called inside calc_econ_model()
cor.mtest <- function(mat) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
          tmp <- cor.test(mat[, i], mat[, j])
          p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat   # Do not remove
}

# Called inside calc_econ_model()
r_tree <- function(user_seed = 456) {
  library(rpart)       # to create regression trees
  library(rpart.plot)  # to plot regression trees
  library(rsample)     # to sample the data into training and testing

  # Define sample for training and testing
  ifelse (user_seed > 0, seed <- user_seed, seed <- 456)
  set.seed(seed)       # Set seed for reproducibility
  dcr_split <- initial_split(e2_p2, prop = .7)
  dcr_train <- training(dcr_split)
  dcr_test  <- testing(dcr_split)

  # Tuning --> https://uc-r.github.io/regression_trees
  hyper_grid <- expand.grid(
    minsplit = seq(5, 20, 1),
    maxdepth = seq(6, 20, 1)
  )

  models <- list()
  for (i in 1:nrow(hyper_grid)) {
    minsplit <- hyper_grid$minsplit[i]
    maxdepth <- hyper_grid$maxdepth[i]

    # train a model and store in the list
    models[[i]] <- rpart(
      formula = dcr_price_close ~ .,
      data    = dcr_train,
      method  = "anova",
      control = list(minsplit = minsplit, maxdepth = maxdepth)
      )
  }

  # Get optimal cp (used below)
  get_cp <- function(x) {
    min <- which.min(x$cptable[, "xerror"])
    cp  <- x$cptable[min, "CP"] 
  }

  # Get minimum error (used below)
  get_min_error <- function(x) {
    min    <- which.min(x$cptable[, "xerror"])
    xerror <- x$cptable[min, "xerror"] 
  }

  # Find the combination with the least amount of error
  hyper_grid_mutate <<- hyper_grid %>%
    mutate(
      cp    = purrr::map_dbl(models, get_cp),
      error = purrr::map_dbl(models, get_min_error)
      ) %>%
    arrange(error) %>%
    top_n(-5, wt = error)

  # Use the top combination to draw the optimal regression tree
  optimal_minsplit <- hyper_grid_mutate[1, 1]
  optimal_maxdepth <- hyper_grid_mutate[1, 2]
  optimal_cp <- hyper_grid_mutate[1, 3]
  optimal_tree <- rpart(
      formula = dcr_price_close ~ .,
      data    = dcr_train,
      method  = "anova",
      control = list(minsplit = optimal_minsplit, maxdepth = optimal_maxdepth, cp = optimal_cp)
      )

  # Mean Absolute Error
  pred <- predict(optimal_tree, newdata = dcr_test)
  mean_abs_err <- mean(abs(pred - dcr_test$dcr_price_close))

  # Bagging
  library(caret)
  ctrl <- trainControl(method = "cv",  number = 10)   # Specify 10-fold cross validation

  # CV bagged model
  bagged_cv <- caret::train(
    dcr_price_close ~ .,
    data = dcr_train,
    method = "rpart",
    trControl = ctrl,
    tuneLength = 50,
    control = list(minsplit = optimal_minsplit, maxdepth = optimal_maxdepth, cp = optimal_cp)
    )

  bagged_cv <<- bagged_cv
  bagged_cv_varimp <<- varImp(bagged_cv)

  # Output results, optimal regression tree
  append_text_results("\nRegression Tree, Rules:\n")
  r_tree_rules <<- rpart.rules(optimal_tree, cover = TRUE, roundint = FALSE, digits = 4, varlen = 0, faclen = 0)
  save_figure(rpart.plot(optimal_tree, roundint = FALSE, digits = 4), "econ_model_regression_tree_figure.png", 960, 630)
  sink_text_results(r_tree_rules)
  append_text_results("\nRegression Tree, Variable Importance:\n")
  sink_text_results(optimal_tree$variable.importance)
  append_text_results(paste("\nRegression Tree, Mean Absolute Error:", mean_abs_err))

  # Output results, bagging regression tree
  append_text_results("\nRegression Tree, Bagging:")
  save_figure(rpart.plot(bagged_cv$finalModel, roundint = FALSE, digits = 4), "econ_model_regression_tree_baggedcv_figure.png", 960, 630)
  sink_text_results(bagged_cv)
  append_text_results("\nRegression Tree, Bagging Rules:\n")
  baggedcv_rules <- rpart.rules(bagged_cv$finalModel, cover = TRUE, roundint = FALSE, digits = 4, varlen = 0, faclen = 0)
  sink_text_results(baggedcv_rules)
  append_text_results("\nRegression Tree, Bagging Variable Importance:\n")
  sink_text_results(bagged_cv_varimp)
}

# OLS model:
calc_econ_model <- function() {
  econ_model <- dcr_usd_price
  econ_model$day <- as.Date(as.character(econ_model$day), format = "%Y-%m-%d")
  econ_model <- merge(econ_model, dcr_d_actual_coin_issuance[, c("day", "sum_total_reward")], by = "day", all = TRUE)
  econ_model <- merge(econ_model, dcr_d_voters_difficulty[, c("day", "avg_hashrate")], by = "day", all = TRUE)
  econ_model <- merge(econ_model, dcr_d_circ_staked[, c("day", "perc_staked")], by = "day", all = TRUE)
  econ_model <- merge(econ_model, dcr_d_transaction_volume[, c("day", "tx_count", "sum_sent")], by = "day", all = TRUE)
  econ_model <- subset(econ_model, day >= str_genesis_date & day <= str_final_date)
  colnames(econ_model)[3] <- "obs_coin_issuance"
  colnames(econ_model)[4] <- "obs_avg_hashrate"
  colnames(econ_model)[7] <- "tx_sum"
  rownames(econ_model) <- 1:nrow(econ_model)
  econ_model_part1 <- subset(econ_model, day >= str_genesis_date & day <= "2018-12-31")
  econ_model_part2 <- subset(econ_model, day >= "2019-01-01" & day <= str_final_date)

  ## Correlations
  e2 <- econ_model[, 2:7]
  e2_p1 <- econ_model_part1[, 2:7]
  e2_p2 <- econ_model_part2[, 2:7]
  e2_p1_cor_mat <- as.matrix(cor(e2_p1))
  e2_p2_cor_mat <- as.matrix(cor(e2_p2))
  e2 <<- e2
  e2_p2 <<- e2_p2
  library(corrplot)
  
  # Matrix of the p-value of the correlation
  p1.mat <- cor.mtest(e2_p1)
  p2.mat <- cor.mtest(e2_p2)

  ## Heatmap
  econ_model_cor <- cor(e2_p2)
  colnames(econ_model_cor) <- c("Price", "Coin Issuance", "Hashrate", "Staked Percentage", "Transaction Count", "Transaction Sum")
  rownames(econ_model_cor) <- c("Price", "Coin Issuance", "Hashrate", "Staked Percentage", "Transaction Count", "Transaction Sum")
  econ_model_cor <<- econ_model_cor
  
  save_figure(
    heatmap.2(
      x = as.matrix(econ_model_cor),
      cellnote = round(as.matrix(econ_model_cor), 2),
      notecol = "black",
      density.info = "none",
      margins = c(9,15),
      dendrogram = "row",
      scale = "none", 
      trace = "none", 
      col = colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))(n = 299),
      cexRow = 1.25,
      cexCol = 1.25,
      notecex = 1.6,
      srtCol = 25), "econ_model_heat_figure.png", 1024, 768)

  ## Linear model
  linear_model_results <- lm(dcr_price_close ~ ., data = e2_p2)
  linear_model_results_summary <- print(summary(linear_model_results))

  linear_model_price_staked <- ggplot(e2_p2, aes(x = perc_staked, y = dcr_price_close)) + 
    geom_point() +
    xlab("Staked Percentage") +
    scale_y_continuous(
        name = "DCR Price (USD) Close"                              # first axis
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 24),
      axis.title.y = element_text(color = g_color1)
    ) +
    stat_smooth(method = "lm", col = g_color2)
  save_figure(linear_model_price_staked, "econ_model_linear_price_staked_figure.png", 1024, 768)

  linear_model_price_hashrate <- ggplot(e2_p2, aes(x = obs_avg_hashrate, y = dcr_price_close)) + 
    geom_point() +
    xlab("Average Hashrate") +
    scale_y_continuous(
        name = "DCR Price (USD) Close"                              # first axis
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 24),
      axis.title.y = element_text(color = g_color1)
    ) +
    stat_smooth(method = "lm", col = g_color2)
  save_figure(linear_model_price_hashrate, "econ_model_linear_price_hashrate_figure.png", 1024, 768)

  linear_model_results_summary <<- linear_model_results_summary
  linear_model_results <<- linear_model_results
  
  append_text_results("\nLinear Model Results Summary:\n")
  sink_text_results(linear_model_results_summary)

  r_tree()
}

### Functions to save text to file
append_text_results <- function(msg) {
  output_file = paste0(str_figuredir, str_textfilename)
  write(msg, file = output_file, append = TRUE)
}

append_csv_results <- function(msg) {
  output_file = paste0(str_figuredir, str_textfilename)
  write.csv(msg, output_file, row.names = FALSE)
}

sink_text_results <- function(msg) {
  output_file = paste0(str_figuredir, str_textfilename)
  sink(file = output_file, append = TRUE)
  print(msg)
  sink()
}

### Function to generate figures
save_figure <- function(hip_df, hip_filename, w, h) {
  png(filename = paste0(str_figuredir, hip_filename),
    width = w, height = h, units = "px", pointsize = 16,
    bg = "white",  res = NA, type = "quartz")
  print(hip_df)
  dev.off()

  # Copy filename to global variable file_list
  file_list <- c(file_list, hip_filename)
  file_list <<- file_list
}

# Graph Hip 1:
gen_graph_hip1 <- function() {
coeff <- 100
hip1_figure <- ggplot(hip1, aes(x = as.Date(day, format = "%Y-%m-%d"), by = "year")) +
  geom_line(data = hip1, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(circulation_coins)), size = 0.4, color = g_color1) +
  geom_line(data = hip1, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(perc_staked * 100000 * coeff)), size = 0.4, color  =g_color2) +
  xlab("Time") +
  scale_y_continuous(
      # labels = scales::comma,
      labels = function(x) format(x, big.mark = " ", scientific = FALSE),
      limits = c(0, 12000000),
      breaks = seq(0, 12000000, by = 2000000),
      name = "Issued Coins",                              # first axis
      sec.axis = sec_axis(~ . / 100000, name = "Staked percentage")      # second axis
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 24),
    axis.title.y = element_text(color = g_color1),
    axis.title.y.right = element_text(color = g_color2)
  )

  save_figure(hip1_figure, "hip1_figure.png", 1024, 768)
}

# Graph Hip 2:
gen_graph_hip2 <- function() {
  coeff <- 100
  hip2_figure <- ggplot(hip2, aes(x = as.Date(day, format = "%Y-%m-%d"), by = "year")) +
  geom_line(data = hip2, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(dcr_price_close)), size = 0.4, color = g_color1) +
  geom_line(data = hip2, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(perc_staked * coeff)), size = 0.4, color = g_color2) +
  xlab("Time") +
  # ggtitle("Price variance and percent staked") +
  scale_y_continuous(
      name = "Price (Close) in USD",                             # first axis
      sec.axis = sec_axis(~ ., name = "Staked percentage")          # second axis
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 24),
    axis.title.y = element_text(color = g_color1),
    axis.title.y.right = element_text(color = g_color2)
  )

  save_figure(hip2_figure, "hip2_figure.png", 1024, 768)
}

gen_graph_hip2_fin <- function() {
coeff <- 10
hip2_fin_figure <- ggplot(hip2_fin, aes(x = as.Date(day, format = "%Y-%m-%d"), by = "year")) +
  geom_line(data = hip2_fin, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(return_usd), color = "Return in USD"), size = 0.4) +
  geom_line(data = hip2_fin, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(perc_staked * coeff), color = "Staked percentage"), size = 0.4) +
  geom_line(data = hip2_fin, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(return * coeff), color = "ROI"), size = 0.4) +
  scale_color_manual(name = "Return", values = c("Return in USD" = g_color1, "Staked percentage" = g_color2, "ROI" = g_color4)) +
  xlab("Time") +
  scale_y_continuous(
      # labels = scales::comma,
      labels = function(x) format(x, big.mark = " ", scientific = FALSE),
      limits = c(0, 10),
      breaks = seq(0, 10, by = 1),
      name = "Return in USD per ticket, in average",            # first axis
      sec.axis = sec_axis(~ . / 10, name = "Staked percentage", breaks = seq(0, 1, by = 0.1))      # second axis
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 24),
    axis.title.y = element_text(color = g_color1),
    axis.title.y.right = element_text(color = g_color2),
    legend.position = "bottom"
  )
  save_figure(hip2_fin_figure, "hip2_fin_figure.png", 1024, 768)
}

# Graph Hip 3:
gen_graph_hip3_dcr <- function() {
  options(scipen = 3)
  coeff <- 1000000000
  hip3_dcr_figure <- ggplot(hip3_dcr, aes(x = as.Date(day, format = "%Y-%m-%d"), by = "year")) +
    geom_line(data = hip3_dcr, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(dcr_price_close)), size = 0.4, color = g_color1) +
    geom_line(data = hip3_dcr, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(dcr_avg_difficulty / coeff)), size = 0.4, color = g_color2) +
    xlab("Time") +
    # ggtitle("Price variance and PoW security") +
    scale_y_continuous(
        name = "Price (Close) in USD",                                     # first axis
        sec.axis = sec_axis(~ . * coeff, name = "Network (block) difficulty")   # second axis
    ) +
    theme_bw() +
    geom_vline(xintercept = as.Date('2018-06-01', format = "%Y-%m-%d"), colour = g_color1, linetype = "dashed") +
    annotate("text", x = as.Date('2018-06-01', format = "%Y-%m-%d"), y = 18, label = "Before ASIC devices\n", size = 5, colour = g_color3, angle = 90) +
    annotate("text", x = as.Date('2018-06-01', format = "%Y-%m-%d"), y = 18, label = "\nAfter ASIC devices", size = 5, colour = g_color2, angle = 90) +
    theme(
      text = element_text(size = 24),
      axis.title.y = element_text(color = g_color1),
      axis.title.y.right = element_text(color = g_color2)
    )

  save_figure(hip3_dcr_figure, "hip3_dcr_figure.png", 1024, 768)
}

gen_graph_hip3_btc <- function() {
  options(scipen = 3)
  coeff <- 1000000000
  hip3_btc_figure <- ggplot(hip3_btc, aes(x = as.Date(day, format = "%Y-%m-%d"), by = "year")) +
    geom_line(data = hip3_btc, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(btc_price_close)), size = 0.4, color = g_color1) +
    geom_line(data = hip3_btc, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(btc_difficulty / coeff)), size = 0.4, color = g_color2) +
    xlab("Time") +
    # ggtitle("Price variance and PoW security") +
    scale_y_continuous(
        name = "Price (Close) in USD",                                     # first axis
        sec.axis = sec_axis(~.*coeff, name="Network (block) difficulty")   # second axis
    ) +
    theme_bw() +
    geom_vline(xintercept = as.Date('2016-07-09', format = "%Y-%m-%d"), colour = g_color1, linetype = "dashed") +
    annotate("text", x = as.Date('2016-07-09', format = "%Y-%m-%d"), y = 3600, label = "Third Bitcoin halving\n", size = 5, colour = g_color2, angle = 90) +
    theme(
      text = element_text(size = 24),
      axis.title.y = element_text(color = g_color1),
      axis.title.y.right = element_text(color = g_color2)
    )

  save_figure(hip3_btc_figure, "hip3_btc_figure.png", 1024, 768)
}

# Graph Hip 4:
gen_graph_hip4 <- function() {
  # Points to plot, come from 'actual' vector created before:
  x_points <- as.numeric(c(10, 20, 28, 45, 60, 90))
  y_points <- actual[-length(actual)]
  id_labels <- data.frame(x_points, y_points)
  id_labels$label <- paste(id_labels$x_points, "days =", id_labels$y_points)

  hip4_figure <- ggplot(hip4, aes(x = days_to_draw_roundup, y = perc_acc_sum)) +
    geom_line(data = hip4, aes(x = as.numeric(days_to_draw_roundup), y = as.numeric(perc_acc_sum)), size = 0.5, color = g_color1) +
    scale_x_continuous(limits = c(1, 143), breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
    # ggtitle("Cumulative frequency of drawn tickets") +
    xlab("Days") +
    ylab("Ticket vote chance") +
    theme_bw() +
    # colour=factor(y_points) 
    geom_point(data = id_labels, aes(x = x_points, y = y_points, size = 3.0), color = g_color2) +
    theme(
      text = element_text(size = 24),
      legend.position = "none"
    ) +
    geom_text(data=id_labels, aes(x = x_points, y = y_points, label = label), size = 8, hjust = "left", nudge_x = +3)

  save_figure(hip4_figure, "hip4_figure.png", 1024, 768)
}

# Graph Hip 5:
gen_graph_hip5 <- function() {
  coeff <- 1000
  hip5_figure <- ggplot(hip5, aes(x = as.Date(day, format = "%Y-%m-%d"), by = "year")) +
    geom_line(data = hip5, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(dcr_price_close)), size = 0.4, color = g_color1) +
    geom_line(data = hip5, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(private_mix_rate / coeff)), size = 0.4, color = g_color2) +
    xlab("Time") +
    # ggtitle("Transaction count and privacy mix rate") +
    scale_y_continuous(
        name = "Price (Close) in USD",                              # first axis
        sec.axis = sec_axis(~. * coeff, name = "Privacy mix rate")      # second axis
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 24),
      axis.title.y = element_text(color = g_color1),
      axis.title.y.right = element_text(color = g_color2)
    )

  save_figure(hip5_figure, "hip5_figure.png", 1024, 768)
}

# Graph Hip 6:
gen_graph_hip6 <- function() {
  coeff <- 10
  options(scipen = 6)
  hip6_figure <- ggplot() +
    geom_line(data = hip6, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(acc_actual_daily_reward), color = "Observed"), size = 1.6) +
    geom_line(data = hip6, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(acc_expected_daily_reward), color = "Expected"), size = 1.3, linetype = "dashed") +
    scale_color_manual(name = "Issuance", values = c("Observed" = g_color1, "Expected" = g_color2)) +
    scale_y_continuous(
      # labels = scales::comma,
      breaks = c(1000000, 3000000, 5000000, 7000000, 9000000, 11000000),
      labels = function(x) format(x, big.mark = " ", scientific = FALSE)
    ) +
    xlab("Time") +
    ylab("Issued DCRs") +
    theme_bw() + 
    theme(
      text = element_text(size = 24),
      legend.position = "bottom"
    )

  save_figure(hip6_figure, "hip6_figure.png", 1024, 768)
}

# Graph Hip 7:
gen_graph_hip7 <- function() {
  coeff <- 0.01
  options(scipen = 6)
  id_labels <- data.frame(hip7$day, hip7$dcr_price_close)

  hip7_figure <- ggplot() +
    geom_line(data = subset(dcr_usd_price, day >= str_genesis_date & day < str_final_date), aes(x=as.Date(day, format = "%Y-%m-%d"), y=as.numeric(dcr_price_close), color = "Decred"), size=0.4) +
    scale_color_manual(name = "Price", values = c("Bitcoin" = g_color1, "Decred" = g_color1)) +
    scale_y_continuous(
        name = "Decred Price (Close) in USD"  # first axis
    ) +
    geom_point(data=id_labels, aes(x = hip7.day, y = hip7.dcr_price_close, size = 3.0), color = g_color2) +
    xlab("Time") +
    theme_bw() + 
    theme(
      text = element_text(size = 24),
      axis.title.y = element_text(color = g_color1),
      axis.title.y.right = element_text(color = g_color2),
      legend.position = ""
    )

  save_figure(hip7_figure, "hip7_figure.png", 1024, 768)
}

# Graph Hip 8:
gen_graph_hip8 <- function() {
  coeff <- 0.01
  options(scipen = 6)
  hip8_figure <- ggplot() +
    geom_line(data = hip8, aes(x = as.Date(day, format= "%Y-%m-%d"), y = as.numeric(btc_price_close), color = "Bitcoin"), size = 0.4) +
    geom_line(data = hip8, aes(x = as.Date(day, format= "%Y-%m-%d"), y = as.numeric(dcr_price_close / coeff), color = "Decred"), size = 0.4) +
    scale_color_manual(name = "Price", values = c("Bitcoin" = g_color1, "Decred" = g_color2)) +
    scale_y_continuous(
        name = "Bitcoin Price (Close) in USD",                                 # first axis
        sec.axis = sec_axis(~. * coeff, name = "Decred Price (Close) in USD")      # second axis
    ) +
    geom_vline(xintercept = as.Date('2018-06-01', format = "%Y-%m-%d"), colour = g_color1, linetype = "dashed") +
    annotate("text", x = as.Date('2018-06-01', format = "%Y-%m-%d"), y = 1800, label = "Before ASIC devices\n", size = 5, colour = g_color3, angle = 90) +
    annotate("text", x = as.Date('2018-06-01', format = "%Y-%m-%d"), y = 1800, label = "\nAfter ASIC devices", size = 5, colour = g_color2, angle = 90) +
    xlab("Time") +
    theme_bw() + 
    theme(
      text = element_text(size = 24),
      axis.title.y = element_text(color = g_color1),
      axis.title.y.right = element_text(color = g_color2),
      legend.position = "bottom"
    )

  save_figure(hip8_figure, "hip8_figure.png", 1024, 768)
}

# Graph Hip 9:
gen_graph_hip9 <- function() {
  # Depends on how many tickets were live on the previous week.
  # Here the average of live tickets in the pool is used to calculate an approximate quorum for all proposals.
  quorum <- as.numeric(avg_ticket_poolsize * 0.2)
  quorum_min <- as.numeric(quorum - 200)
  quorum_max <- as.numeric(quorum + 200)

  df_long <- reshape2::melt(dcr_g_vote_total_number[,1:4][,-2], id = "token")

  hip9_figure <- ggplot(data = df_long, aes(x = token, y = value, fill = variable, label = "")) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    geom_hline(yintercept = quorum, colour = g_color1) +
    geom_hline(yintercept = quorum_min, colour = g_color1, linetype = "dashed") +
    geom_hline(yintercept = quorum_max, colour = g_color1, linetype = "dashed") +
    annotate("label", x = 6, y = quorum, label = "Quorum", size = 6, colour = "white", fill = g_color1, angle = 0) +
    xlab("Proposals") +
    ylab("Votes") +
    labs(fill = "Choice") +
    scale_y_continuous(
      # labels = scales::comma,
      breaks = c(5000, 10000, 15000, 20000),
      labels = function(x) format(x, big.mark = " ", scientific = FALSE)
    ) +
    theme_bw() + 
    theme(
      text = element_text(size = 24),
      axis.text.x = element_blank(),
      axis.title.y = element_text(color = g_color1),
      axis.title.y.right = element_text(color = g_color2),
      legend.position = "right",
      legend.direction = "vertical"
    )

  save_figure(hip9_figure, "hip9_figure.png", 1124, 768)

  append_text_results(paste("Hip. 9 N Proposals:", nrow(dcr_g_vote_total_number)))
  append_text_results(paste("Hip. 9 Average daily ticket pool size:", round(avg_ticket_poolsize, 4)))
  append_text_results(paste("Hip. 9 Average proposal turnout:", round(avg_proposal_turnout, 4)))
  append_text_results(paste("Hip. 9 Average proposal approval:", round(avg_proposal_approval, 4)))
  append_text_results(paste("Hip. 9 Proposals with different winning days:", round(avg_proposal_approval, 4)))
}

gen_graph_hip9_agenda <- function() {
  quorum <- as.numeric(8064 * 5 * 0.1)
  
  # Depends on how many tickets were live on the previous week.
  # Here the average of live tickets in the pool is used to calculate an approximate quorum for all proposals.
  df_long <- reshape2::melt(dcr_g_agendas_vote_results[,2:5][,-3], id = "name")

  hip9_agendas_figure <- ggplot(data = df_long, aes(x = name, y = value, fill = variable, label = "")) +
    geom_bar(stat = "identity", width = 0.3, position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = c("yes" = "#00BFC4", "no" = "#FF866D", "abs" = "#619CFF")) +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    geom_hline(yintercept = quorum, colour = g_color1) +
    annotate("label", x = 1, y = quorum, label = "Quorum", size = 6, colour = "white", fill = g_color1, angle = 0) +
    xlab("Agendas") +
    ylab("Votes") +
    labs(fill = "Choice") +
    scale_y_continuous(
      # labels = scales::comma,
      # breaks = c(10000, 20000, 30000, 40000),
      labels = function(x) format(x, big.mark = " ", scientific = FALSE)
    ) +
    theme_bw() + 
    theme(
      text = element_text(size = 24),
      #axis.text.x = element_blank(),
      axis.title.y = element_text(color = g_color1),
      axis.title.y.right = element_text(color = g_color2),
      legend.position = "right",
      legend.direction = "vertical"
    )

  save_figure(hip9_agendas_figure, "hip9_agendas_figure.png", 1124, 768)

  append_text_results(paste("Hip. 9 N Agendas:", nrow(dcr_g_vote_total_number)))
  append_text_results(paste("Hip. 9 Average daily ticket pool size:", round(avg_ticket_poolsize, 4)))
  append_text_results(paste("Hip. 9 Average proposal turnout:", round(avg_proposal_turnout, 4)))
  append_text_results(paste("Hip. 9 Average proposal approval:", round(avg_proposal_approval, 4)))
  append_text_results(paste("Hip. 9 Proposals with different winning days:", round(avg_proposal_approval, 4)))
}

# Graph BTC DCR comparison:
gen_graph_btc_dcr_comparison <- function() {
  coeff <- 10
  options(scipen = 6)
  btc_dcr_figure <- ggplot() +
    geom_line(data = btc_exp_issuance_by_day, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(acc_sum), color = "Bitcoin"), size = 0.7) +
    geom_line(data = dcr_exp_issuance_by_day, aes(x = as.Date(day, format = "%Y-%m-%d"), y = as.numeric(acc_sum), color = "Decred"), size = 0.7) +
    scale_color_manual(name = "Expected Issuance", values = c("Bitcoin" = g_color1, "Decred" = g_color2)) +
    scale_y_continuous(
      # labels = scales::comma,
      breaks = c(0, 2000000, 4000000, 6000000, 8000000, 10000000, 12000000, 14000000, 16000000, 18000000, 20000000),
      labels = function(x) format(x, big.mark = " ", scientific = FALSE)
    ) +
    # BTC DCR Comparison: Last: DCR: 20638308.2075263  - BTC:  20887554.296875
    xlab("Expected Time") +
    ylab("Cumulative Issuance") +
    # ggtitle("Bitcoin and Decred estimated supply growth curve until June 2039") +
    theme_bw() + 
    theme(
      text = element_text(size = 20),
      legend.position = "bottom"
    )

  save_figure(btc_dcr_figure, "btc_dcr_figure.png", 1024, 568)

  append_text_results(paste("\nBTC DCR Comparison: DCR:", nrow(dcr_exp_issuance_by_day), " - BTC: ", nrow(btc_exp_issuance_by_day)))
  append_text_results(paste("BTC DCR Comparison: Last: DCR:", dcr_exp_issuance_by_day[nrow(dcr_exp_issuance_by_day):nrow(dcr_exp_issuance_by_day),3], " - BTC: ", btc_exp_issuance_by_day[nrow(btc_exp_issuance_by_day):nrow(btc_exp_issuance_by_day),3]))
}

# Calculate the security curve
gen_sec_curve <- function() {
  sec_curve <- data.frame(pos_share = seq(from = 0.001, to = 0.80, by = 0.001))
  # A 'for loop' must be used because of sum() below, otherwise it sums all rows
  for (row in 1:nrow(sec_curve)) {
    sec_curve[row, "x_hashpower"] = as.numeric(1 / sum(dbinom(0:2, 5, (1 - sec_curve[row, "pos_share"])))) - 1
  }
  sec_curve <<- sec_curve
}

# Graph graph security curve:
gen_graph_sec_curve <- function() {
  options(scipen = 6)
  sec_curve_figure <- ggplot() +
    geom_line(data = sec_curve, aes(x = as.numeric(pos_share), y = as.numeric(x_hashpower), color = "PoW+PoS"), size = 0.8) +
    scale_color_manual(name = "Security Curve", values = c("PoW+PoS" = g_color2)) +
    scale_y_continuous(
      # labels = scales::comma,
      trans = "log10",
      breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
      labels = function(x) format(x, big.mark = " ", scientific = FALSE)
    ) +
    scale_x_continuous(
      breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00),
      labels = function(x) format(x, big.mark = " ", scientific = FALSE)
    ) +
    xlab("\nAttacker fraction of ticket pool (PoS)") +
    ylab("Attacker required hashpower (PoW)\n(multiple of honest adversaries' hashpower)") +
    theme_bw() + 
    theme(
      text = element_text(size = 20),
      legend.position = "bottom"
    )

  save_figure(sec_curve_figure, "sec_curve_figure.png", 1024, 568)
}

### Main function
main <- function() {
  print_version()

  print("Reading files into dataframes...")
  define_variables()
  prepare_dataframes()
  process_dataframes()

   print("Calculating hypotheses...")
   calc_hip1()
   calc_hip2()
   calc_hip3_dcr()
   calc_hip3_btc()
   calc_hip4()
   calc_hip5()
   calc_hip6()
   calc_hip7()
   calc_hip8()

   print("Generating expected coin issuance dataframes...")
   gen_dcr_exp_issuance()
   gen_btc_exp_issuance()

   print("Generating figures...")
   gen_graph_hip1()
   gen_graph_hip2()
   gen_graph_hip2_fin()
   gen_graph_hip3_dcr()
   gen_graph_hip3_btc()
   gen_graph_hip4()
   gen_graph_hip5()
   gen_graph_hip6()
   gen_graph_hip7()
   gen_graph_hip8()
   gen_graph_hip9()
   gen_graph_hip9_agenda()
   gen_graph_btc_dcr_comparison()

   print("Generating Security Curve figures...")
   gen_sec_curve()
   gen_graph_sec_curve()

  print("Calculating econometric model...")
  calc_econ_model()

  # Copy text results filename to global variable file_list
  file_list <- c(file_list, str_textfilename)
  file_list <<- file_list

  print("All done.")
  print(paste(c("Generated files:", file_list), collapse = ", "))
}

main()
