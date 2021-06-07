## ---- include = FALSE---------------------------------------------------------
library(autoFC)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
set.seed(2021)
# Simulation of 1,000 respondents on 60 items. A better simulation should be
# consisting of responses produced by specific IRT parameters.
s1 <- sample(seq(1:5), 500*60, replace = TRUE, 
                                prob = c(0.10, 0.15, 0.20, 0.25, 0.30))
s2 <- sample(seq(1:5), 500*60, replace = TRUE, 
                                prob = c(0.50, 0.10, 0.10, 0.15, 0.15))

item_responses <- matrix(c(s1, s2), ncol = 60)

item_dims <- sample(c("Openness","Conscientiousness","Neuroticism",
                      "Extraversion","Agreeableness"), 60, replace = TRUE)
item_mean <- colMeans(item_responses)
item_difficulty <- runif(60, -1, 1)

# Then we build a data frame with item characteristics
item_chars <- data.frame(DIM = item_dims, SD_Mean = item_mean, DIFF = item_difficulty)

char_weights = c(1, -1, -3)

## -----------------------------------------------------------------------------
initial_FC <- make_random_block(total_items = 60, item_per_block = 3)
knitr::kable(initial_FC)

## -----------------------------------------------------------------------------
knitr::kable(matrix(item_chars$DIM[t(initial_FC)], ncol = 3, byrow = TRUE))

## -----------------------------------------------------------------------------
sd_initial <- matrix(item_chars$SD_Mean[t(initial_FC)], ncol = 3, byrow = TRUE)
knitr::kable(sd_initial)

## -----------------------------------------------------------------------------
diff_initial <- matrix(item_chars$DIFF[t(initial_FC)], ncol = 3, byrow = TRUE)
knitr::kable(diff_initial)

## -----------------------------------------------------------------------------
cal_block_energy(block = initial_FC, item_chars = item_chars, weights = char_weights)

## -----------------------------------------------------------------------------
cal_block_energy_with_iia(block = initial_FC, item_chars = item_chars, 
                          weights = char_weights,
                          rater_chars = item_responses)

## -----------------------------------------------------------------------------
cal_block_energy_with_iia(block = initial_FC, item_chars = item_chars, 
                          weights = char_weights,
                          rater_chars = item_responses, 
                          iia_weights = c(0, 0, 0, 0))

## -----------------------------------------------------------------------------
knitr::kable(get_iia(block = initial_FC, data = item_responses))

## -----------------------------------------------------------------------------
# Note that this will take some time to run! (~ 1-2 minutes with this setting)

# Weights for social desirability score and item difficulty should be set to -1, 
# because we don't want variance for these characteristics to be big.
result <- sa_pairing_generalized(block = initial_FC, eta_Temperature = 0.01,
                                 r = 0.995, end_criteria = 10^(-6), 
                                 weights = char_weights,
                                 item_chars = item_chars, use_IIA = TRUE,
                                 rater_chars = item_responses)

## -----------------------------------------------------------------------------
# Initial energy with IIA
cal_block_energy_with_iia(block = result$block_initial, item_chars = item_chars, 
                          weights = char_weights, rater_chars = item_responses)

# Alternative way to calculate initial energy
print(result$energy_initial)

## -----------------------------------------------------------------------------
# Final energy with IIA
cal_block_energy_with_iia(block = result$block_final, item_chars = item_chars, 
                          weights = char_weights, rater_chars = item_responses)

# Alternative way to calculate final energy
print(result$energy_final)

## -----------------------------------------------------------------------------
knitr::kable(matrix(item_chars$DIM[t(result$block_final)], ncol = 3, byrow = TRUE))

## -----------------------------------------------------------------------------
sd_final <- matrix(item_chars$SD_Mean[t(result$block_final)], ncol = 3, byrow = TRUE)
knitr::kable(sd_final)

## -----------------------------------------------------------------------------
# Initial
print(mean(apply(sd_initial, 1, var)))   

# Final
print(mean(apply(sd_final, 1, var)))     

## -----------------------------------------------------------------------------
diff_final <- matrix(item_chars$DIF[t(result$block_final)], ncol = 3, byrow = TRUE)
knitr::kable(diff_final)

## -----------------------------------------------------------------------------
print(mean(apply(diff_initial, 1, var)))   
print(mean(apply(diff_final, 1, var)))     

## -----------------------------------------------------------------------------
colMeans(get_iia(result$block_final, data = item_responses))

## -----------------------------------------------------------------------------
FC_1 <- sa_pairing_generalized(initial_FC, eta_Temperature = 0.01,
                                 r = 0.995, end_criteria = 10^(-6), 
                                 weights = c(1, 0, 0),
                                 item_chars = item_chars, use_IIA = TRUE,
                                 rater_chars = item_responses)

## -----------------------------------------------------------------------------
FC_2 <- sa_pairing_generalized(FC_1$block_final, eta_Temperature = 0.01,
                               r = 0.995, end_criteria = 10^(-6), 
                               weights = c(1, -1, 0),
                               item_chars = item_chars, use_IIA = TRUE,
                               rater_chars = item_responses)

## -----------------------------------------------------------------------------
FC_3 <- sa_pairing_generalized(FC_2$block_final, eta_Temperature = 0.01,
                               r = 0.995, end_criteria = 10^(-6), 
                               weights = c(1, -1, -3),
                               item_chars = item_chars, use_IIA = TRUE,
                               rater_chars = item_responses)

## -----------------------------------------------------------------------------
knitr::kable(matrix(item_chars$DIM[t(FC_3$block_final)], ncol = 3, byrow = TRUE))

## -----------------------------------------------------------------------------
sd_FC3 <- matrix(item_chars$SD_Mean[t(FC_3$block_final)], ncol = 3, byrow = TRUE)
knitr::kable(sd_FC3)

## -----------------------------------------------------------------------------
# Initial solution
print(mean(apply(sd_initial, 1, var)))   

# Simultaneous optimization
print(mean(apply(sd_final, 1, var)))    

# Sequential optimization
print(mean(apply(sd_FC3, 1, var)))       

## -----------------------------------------------------------------------------
diff_fc3 <- matrix(item_chars$DIF[t(FC_3$block_final)], ncol = 3, byrow = TRUE)
knitr::kable(diff_final)

## -----------------------------------------------------------------------------
# Initial solution
print(mean(apply(diff_initial, 1, var))) 

# Simultaneous optimization
print(mean(apply(diff_final, 1, var)))   

# Sequential optimization
print(mean(apply(diff_fc3, 1, var)))      

## -----------------------------------------------------------------------------
colMeans(get_iia(FC_3$block_final, data = item_responses))

