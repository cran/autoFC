## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load_response------------------------------------------------------------
library(autoFC)

# Load the built-in HEXACO data
data("HEXACO_example_data")
head(HEXACO_example_data[, 1:5])

## ----load_item_info-----------------------------------------------------------
data("FC_item_info")


FC_item_info$M_Score <- colMeans(HEXACO_example_data[, -1], na.rm = TRUE)
head(FC_item_info)

## ----initial-solution---------------------------------------------------------
set.seed(123)

# Build 20 random triplets from the 60 items
starting_blocks <- make_random_block(total_items = 60, block_size = 3)
starting_blocks

## ----optimize-----------------------------------------------------------------
# Run the simulated annealing optimizer
set.seed(2026)
optimized_test <- optimize_blocks(
  block = starting_blocks, 
  total_items = 60,
  item_chars = FC_item_info[, -1],                  # Containing item characteristics (Keying, factor, mean scores)
  # We give keying a weight of 1, factor weight of 2 (soft constraints for preventing overlap; hard constraints can be added as long as you provide target_dist parameter)
  # Weight for mean scores is set higher at -5 (because we want to REDUCE the absolute difference or variance of item mean scores within a block)
  char_weights = c(1, 2, -5),                       
  optim_funcs = c("var", "facfun", "var"),          # Minimize variance for item mean scores
  temp_cooling = 0.999,                              # Fast cooling for vignette speed
  temp_stop_ratio = 1e-6
)

# View the optimized block matrix
final_blocks <- optimized_test$block_final
head(FC_item_info[c(t(final_blocks)),])


## ----diagnostics--------------------------------------------------------------
# Check the structural pair distribution of our constructed test
diagnostics <- summarize_trait_pairs(
  blocks = final_blocks,
  item_chars = FC_item_info,
  trait_col = "factor",
  key_col = "keying"
)

# View the diagnostic table
diagnostics

## ----constract desired trait distributions------------------------------------
target_dist <- build_target_dist(traits = unique(FC_item_info$factor), total_pairs = 60, 
                                 equal_mixed_ratio = c(1, 1), allow_same_trait = FALSE)
head(target_dist)

## -----------------------------------------------------------------------------
optimized_test2 <- optimize_blocks(
  block = starting_blocks, 
  total_items = 60,
  item_chars = FC_item_info[, -1],                  
  char_weights = c(1, 2, -5),                       
  optim_funcs = c("var", "facfun", "var"),          
  temp_cooling = 0.999,                             
  target_dist = target_dist,                    # NEW: Now we have a pre-specified target distribution
  trait_col = "factor",                         # NEW: Now we should tell optimize_blocks() which column specifies item traits in item_chars
  key_col = "keying",                           # NEW: Now we should tell optimize_blocks() which column specifies item keying in item_chars
  scale_fit_weight = 100,                       # NEW: How much do we value the consistency with our desired target distribution?
  prevent_overlap = TRUE,                       # NEW: Don't put items measuring the same traits in the same block!
  temp_stop_ratio = 1e-6)

final_blocks2 <- optimized_test2$block_final
head(FC_item_info[c(t(final_blocks2)),])


## ----diagnostics2-------------------------------------------------------------

# Check the structural pair distribution of our constructed test
diagnostics2 <- summarize_trait_pairs(
  blocks = final_blocks2,
  item_chars = FC_item_info,
  trait_col = "factor",
  key_col = "keying"
)

# View the diagnostic table
diagnostics2


## -----------------------------------------------------------------------------
data("MOLE_data")


MOLE_data <- MOLE_data[!is.na(MOLE_data$Q1_0_GROUP_T1),]
head(MOLE_data)

## ----convert to pairwise------------------------------------------------------
resp_data <- MOLE_data[MOLE_data$Group == "FC1", ]
resp_data <- resp_data[, -41]    ### Remove the last column indicating group

resp_pairwise_data <- convert_mole_to_pairwise(resp_data, n_blocks = 20, block_size = 3)

### This data will be readily used for later scoring!
head(resp_pairwise_data)

## ----ipsative scoring---------------------------------------------------------
data("FC_blocks")
### Because blocks in FC1 do not follow 1, 2, 3... 60 item order, we need to adjust into item orders that are actually presented in FC1
FC1_item_info <- FC_item_info[FC_blocks$FC1_Blocks, ]

ipsative_scores <- score_tirt_ipsative(resp_pairwise_data, n_blocks = 20, block_size = 3,
                                       key_matrix = FC1_item_info, trait_col = "factor", key_col = "keying")

head(ipsative_scores)

## ----build lavaan model-------------------------------------------------------
tirt_lavaan <- generate_tirt_lavaan_syntax(n_blocks = 20, block_size = 3, key_matrix = FC1_item_info,
                                           trait_col = "factor", key_col = "keying", model_type = "TFM",
                                           force_positive_variances = FALSE)     ## In practice, you can set it to TRUE
# cat(tirt_lavaan)

## ----test lavaan model, eval = FALSE------------------------------------------
# library(lavaan)
# example_fit_lavaan <- sem(tirt_lavaan, data = resp_pairwise_data, parameterization = "theta",
#                           estimator = "ULSMV", verbose = TRUE, ordered = TRUE, std.lv = FALSE, mimic = "mplus")

## ----score lavaan, eval = FALSE-----------------------------------------------
# tirt_lavaan_scores <- score_tirt_lavaan(example_fit_lavaan, data = resp_pairwise_data)

## ----load-precomputed, echo = FALSE-------------------------------------------
tirt_lavaan_scores <- readRDS("fit_FC_lavaan.rds")

## ----correlation--------------------------------------------------------------
### Now check out how the scores correlated with ipsative scores.
cor(tirt_lavaan_scores$honestyhumility, ipsative_scores$honestyhumility)
cor(tirt_lavaan_scores$emotionality, ipsative_scores$emotionality)
cor(tirt_lavaan_scores$extraversion, ipsative_scores$extraversion)
cor(tirt_lavaan_scores$agreeableness, ipsative_scores$agreeableness)
cor(tirt_lavaan_scores$conscientiousness, ipsative_scores$conscientiousness)
cor(tirt_lavaan_scores$openness, ipsative_scores$openness)


## ----standata-----------------------------------------------------------------
stan_data <- prepare_tirt_stan_data(resp_pairwise_data, n_blocks = 20, block_size = 3,
                                    key_matrix = FC1_item_info, trait_col = "factor", key_col = "keying")

## ----runstan, eval = FALSE----------------------------------------------------
# example_fit_stan <- score_tirt_stan(stan_data, chains = 4, parallel_chains = 4,
#                                     threads_per_chain = 4,
#                                     iter_warmup = 1000, iter_sampling = 1000,
#                                     init = 0)
# 

## ----load-precomputed stan, echo = FALSE--------------------------------------
example_fit_stan <- readRDS("fit_FC_stan.rds")

## ----convergent validity------------------------------------------------------
tirt_stan_scores <- example_fit_stan$scores

### Correlation between stan and lavaan scores
cor(tirt_stan_scores$honestyhumility, tirt_lavaan_scores$honestyhumility)
cor(tirt_stan_scores$emotionality, tirt_lavaan_scores$emotionality)
cor(tirt_stan_scores$extraversion, tirt_lavaan_scores$extraversion)
cor(tirt_stan_scores$agreeableness, tirt_lavaan_scores$agreeableness)
cor(tirt_stan_scores$conscientiousness, tirt_lavaan_scores$conscientiousness)
cor(tirt_stan_scores$openness, tirt_lavaan_scores$openness)

### Correlation between stan and ipsative scores
cor(tirt_stan_scores$honestyhumility, ipsative_scores$honestyhumility)
cor(tirt_stan_scores$emotionality, ipsative_scores$emotionality)
cor(tirt_stan_scores$extraversion, ipsative_scores$extraversion)
cor(tirt_stan_scores$agreeableness, ipsative_scores$agreeableness)
cor(tirt_stan_scores$conscientiousness, ipsative_scores$conscientiousness)
cor(tirt_stan_scores$openness, ipsative_scores$openness)


## -----------------------------------------------------------------------------
HEXACO_example_data$H_SUM = rowSums(HEXACO_example_data[, c(7, 19, 37, 55)]) + 36 - rowSums(HEXACO_example_data[, c(13, 25, 31, 43, 49, 61)])
HEXACO_example_data$E_SUM = rowSums(HEXACO_example_data[, c(6, 12, 18, 24, 30, 48)]) + 24 - rowSums(HEXACO_example_data[, c(36, 42 ,54, 60)])
HEXACO_example_data$X_SUM = rowSums(HEXACO_example_data[, c(5, 17, 23, 35, 41, 59)]) + 24 - rowSums(HEXACO_example_data[, c(11, 29, 47, 53)])
HEXACO_example_data$A_SUM = rowSums(HEXACO_example_data[, c(4, 28, 34, 40, 46, 52)]) + 24 - rowSums(HEXACO_example_data[, c(10, 16, 22, 58)])
HEXACO_example_data$C_SUM = rowSums(HEXACO_example_data[, c(3, 9, 39, 51)]) + 36 - rowSums(HEXACO_example_data[, c(15, 21, 27, 33, 45, 57)])
HEXACO_example_data$O_SUM = rowSums(HEXACO_example_data[, c(8, 14, 26, 38, 44)]) + 30 - rowSums(HEXACO_example_data[, c(2, 20, 32, 50, 56)])

cor(HEXACO_example_data$H_SUM[HEXACO_example_data$Group == "FC1"], tirt_lavaan_scores$honestyhumility)
cor(HEXACO_example_data$E_SUM[HEXACO_example_data$Group == "FC1"], tirt_lavaan_scores$emotionality)
cor(HEXACO_example_data$X_SUM[HEXACO_example_data$Group == "FC1"], tirt_lavaan_scores$extraversion)
cor(HEXACO_example_data$A_SUM[HEXACO_example_data$Group == "FC1"], tirt_lavaan_scores$agreeableness)
cor(HEXACO_example_data$C_SUM[HEXACO_example_data$Group == "FC1"], tirt_lavaan_scores$conscientiousness)
cor(HEXACO_example_data$O_SUM[HEXACO_example_data$Group == "FC1"], tirt_lavaan_scores$openness)

cor(HEXACO_example_data$H_SUM[HEXACO_example_data$Group == "FC1"], tirt_stan_scores$honestyhumility)
cor(HEXACO_example_data$E_SUM[HEXACO_example_data$Group == "FC1"], tirt_stan_scores$emotionality)
cor(HEXACO_example_data$X_SUM[HEXACO_example_data$Group == "FC1"], tirt_stan_scores$extraversion)
cor(HEXACO_example_data$A_SUM[HEXACO_example_data$Group == "FC1"], tirt_stan_scores$agreeableness)
cor(HEXACO_example_data$C_SUM[HEXACO_example_data$Group == "FC1"], tirt_stan_scores$conscientiousness)
cor(HEXACO_example_data$O_SUM[HEXACO_example_data$Group == "FC1"], tirt_stan_scores$openness)

cor(HEXACO_example_data$H_SUM[HEXACO_example_data$Group == "FC1"], ipsative_scores$honestyhumility)
cor(HEXACO_example_data$E_SUM[HEXACO_example_data$Group == "FC1"], ipsative_scores$emotionality)
cor(HEXACO_example_data$X_SUM[HEXACO_example_data$Group == "FC1"], ipsative_scores$extraversion)
cor(HEXACO_example_data$A_SUM[HEXACO_example_data$Group == "FC1"], ipsative_scores$agreeableness)
cor(HEXACO_example_data$C_SUM[HEXACO_example_data$Group == "FC1"], ipsative_scores$conscientiousness)
cor(HEXACO_example_data$O_SUM[HEXACO_example_data$Group == "FC1"], ipsative_scores$openness)

