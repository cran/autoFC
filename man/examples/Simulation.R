cfa_model <- paste0("H =~ ", paste0("SS", seq(6, 18, by = 6), collapse = " + "), "\n",
                    "E =~ ", paste0("SS", seq(5, 18, by = 6), collapse = " + "), "\n",
                    "X =~ ", paste0("SS", seq(4, 18, by = 6), collapse = " + "), "\n")
cfa_fit <- lavaan::cfa(cfa_model, data = HEXACO_example_data[1:500,], ordered = TRUE,
                       estimator = "WLSMV", verbose = TRUE, test = "none", se = "none")
cfa_estimates <- get_CFA_estimates(cfa_fit)
simu_estimates <- get_simulation_matrices(cfa_estimates, N = 500)

### Because utilities orders are exactly the order each item appears in the cfa model,
### you may need to re-order the columns back in your own case.

num_order <- order(as.numeric(gsub("[^0-9]", "", colnames(simu_estimates$Utility))))
new_Utility <- simu_estimates$Utility[, num_order]
