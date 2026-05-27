#' Generate lavaan Syntax for Thurstonian Models
#'
#' Generates the syntax required to fit either a Second-Order Thurstonian Factor 
#' Model or a First-Order Thurstonian IRT Model in lavaan.
#'
#' @param n_blocks Integer. Number of blocks in the questionnaire.
#' @param block_size Integer. Number of items per block.
#' @param key_matrix A data.frame with two columns, one indicating item trait,
#'        another indicating item sign.
#' @param trait_col Character string. The name of the column in \code{key_matrix} indicating the trait
#'        measured by the item. 
#' @param key_col Character string. The name of the column in \code{key_matrix} indicating the keying 
#'        direction of the item (e.g., positive/negative).  
#' @param cor_matrix Optional matrix. Starting values for trait correlations.
#' @param model_type Character. Either "TFM" (Second-Order Thurstonian Factor Model) or "IRT" (First-Order).
#' @param force_positive_variances Logical. If TRUE, prevents Heywood cases. 
#'        In "Factor" models, utility variances > 0.0001. 
#'        In "IRT" models, utility variances and derived correlated errors > 0.0001. 
#'        Defaults to TRUE.
#' @param more_constraints Additional lavaan syntax strings for constraining certain parameters if needed.
#'
#' @return A character string containing the lavaan model syntax.
#' @export
generate_tirt_lavaan_syntax <- function(n_blocks, 
                                        block_size, 
                                        key_matrix, 
                                        trait_col,
                                        key_col,
                                        cor_matrix = NULL,
                                        model_type = c("TFM", "TIRT"),
                                        force_positive_variances = TRUE,
                                        more_constraints = "") {
  
  model_type <- match.arg(model_type)
  
  n_items <- n_blocks * block_size
  n_pairs <- n_blocks * (block_size * (block_size - 1) / 2)
  
  if (nrow(key_matrix) != n_items) {
    stop("key_matrix must have exactly ", n_items, " rows (n_blocks * block_size).")
  }
  
  # --- Extract Traits ---
  trait_names <- unique(as.character(key_matrix[[trait_col]]))
  n_traits <- length(trait_names)
  
  # --- Validate Correlation Matrix ---
  if (is.null(cor_matrix)) {
    cor_matrix <- diag(n_traits)
    rownames(cor_matrix) <- trait_names
    colnames(cor_matrix) <- trait_names
  } else {
    if (nrow(cor_matrix) != n_traits || ncol(cor_matrix) != n_traits) {
      stop(sprintf("cor_matrix must be a %d x %d matrix.", n_traits, n_traits))
    }
    if (is.null(rownames(cor_matrix)) || is.null(colnames(cor_matrix))) {
      message("Note: 'cor_matrix' lacks names. Assuming order: ", paste(trait_names, collapse = ", "))
      rownames(cor_matrix) <- trait_names; colnames(cor_matrix) <- trait_names
    } else {
      if (!all(trait_names %in% rownames(cor_matrix)) || !all(trait_names %in% colnames(cor_matrix))) {
        stop("Row/Col names of 'cor_matrix' do not match 'key_matrix[[trait_col]]'.")
      }
      cor_matrix <- cor_matrix[trait_names, trait_names, drop = FALSE]
    }
  }
  
  # --- Build Pairs Matrix ---
  Pairs <- matrix(0, nrow = n_pairs, ncol = 2)
  pair_idx <- 1
  for (b in 1:n_blocks) {
    for (i in 1:(block_size - 1)) {
      for (k in (i + 1):block_size) {
        Pairs[pair_idx, 1] <- (b - 1) * block_size + i
        Pairs[pair_idx, 2] <- (b - 1) * block_size + k
        pair_idx <- pair_idx + 1
      }
    }
  }
  pair_names <- paste0("i", Pairs[, 1], "i", Pairs[, 2])
  
  syn <- character()
  
  # =====================================================================
  # PATH A: SECOND-ORDER THURSTONIAN FACTOR MODEL
  # =====================================================================
  if (model_type == "TFM") {
    
    syn <- c(syn, "# --- SECOND-ORDER THURSTONIAN FACTOR MODEL ---")
    
    # 1. First-Order Utilities
    syn <- c(syn, "\n# 1. First-Order Utilities (y* = t_i - t_k)")
    for (item in 1:n_items) {
      pos_pairs <- pair_names[Pairs[, 1] == item]
      neg_pairs <- pair_names[Pairs[, 2] == item]
      terms <- c()
      if (length(pos_pairs) > 0) terms <- c(terms, paste0("1*", pos_pairs))
      if (length(neg_pairs) > 0) terms <- c(terms, paste0("-1*", neg_pairs))
      syn <- c(syn, paste0("t", item, " =~ ", paste(terms, collapse = " + ")))
    }
    
    # 2. Second-Order Structure
    syn <- c(syn, "\n# 2. Second-Order Structure (Traits load onto Utilities)")
    for (t_idx in 1:n_traits) {
      t_name <- trait_names[t_idx]
      items_for_t <- which(as.character(key_matrix[[trait_col]]) == t_name)
      terms <- sapply(items_for_t, function(i) paste0("NA*t", i, " + start(", key_matrix[[key_col]][i], ")*t", i))
      syn <- c(syn, paste0(t_name, " =~ ", paste(terms, collapse = " + ")))
    }
    
    # 3. Trait Variances & Covariances
    syn <- c(syn, "\n# 3. Trait Variances (Fixed to 1) and Covariances")
    for (t_name in trait_names) syn <- c(syn, paste0(t_name, " ~~ 1 * ", t_name))
    if (n_traits > 1) {
      for (i in 1:(n_traits - 1)) {
        for (j in (i + 1):n_traits) {
          t1 <- trait_names[i]; t2 <- trait_names[j]; cor_val <- cor_matrix[t1, t2]
          syn <- c(syn, paste0(t1, " ~~ start(", cor_val, ") * ", t2))
        }
      }
    }
    
    # 4. Utility Uniquenesses
    syn <- c(syn, "\n# 4. Utility Uniquenesses (Fixed for identification)")
    for (b in 1:n_blocks) {
      items_in_block <- ((b - 1) * block_size + 1):(b * block_size)
      if (block_size == 2) {
        for (i in items_in_block) syn <- c(syn, paste0("t", i, " ~~ 0.5 * t", i))
      } else {
        for (j in 2:(block_size)) syn <- c(syn, paste0("t", items_in_block[j], " ~~ var", items_in_block[j], " * t", items_in_block[j]))
        syn <- c(syn, paste0("t", items_in_block[1], " ~~ 1 * t", items_in_block[1]))
      }
    }
    
    # 5. Observed Pair Residual Variances
    syn <- c(syn, "\n# 5. Observed Pair Residual Variances (Fixed to 0 for pure model)")
    for (p in pair_names) syn <- c(syn, paste0(p, " ~~ 0 * ", p))
    
    # 6. Prevent Heywood Cases
    if (force_positive_variances && block_size > 2) {
      syn <- c(syn, "\n# 6. Prevent Heywood Cases (Variances > 0)")
      for (b in 1:n_blocks) {
        items_in_block <- ((b - 1) * block_size + 1):(b * block_size)
        for (j in 2:(block_size)) syn <- c(syn, paste0("var", items_in_block[j], " > 0.0001"))
      }
    }
    
    # =====================================================================
    # PATH B: FIRST-ORDER THURSTONIAN IRT MODEL (CLASSIC)
    # =====================================================================
  } else if (model_type == "TIRT") {
    
    syn <- c(syn, "# --- FIRST-ORDER THURSTONIAN IRT MODEL ---")
    
    # 1. Factor Loadings (Traits load directly onto Pairs)
    syn <- c(syn, "\n# 1. Factor Loadings (Traits load onto pairs)")
    for (t_idx in 1:n_traits) {
      t_name <- trait_names[t_idx]
      terms <- c()
      
      for (p in 1:n_pairs) {
        i <- Pairs[p, 1]
        k <- Pairs[p, 2]
        
        if (as.character(key_matrix[[trait_col]][i]) == t_name) {
          label <- paste0("L", i)
          start <- key_matrix[[key_col]][i]
          terms <- c(terms, paste0("NA*", pair_names[p], " + ", label, "*", pair_names[p], " + start(", start, ")*", pair_names[p]))
        }
        if (as.character(key_matrix[[trait_col]][k]) == t_name) {
          label <- paste0("L", k, "_n")
          start <- -1 * key_matrix[[key_col]][k]
          terms <- c(terms, paste0("NA*", pair_names[p], " + ", label, "*", pair_names[p], " + start(", start, ")*", pair_names[p]))
        }
      }
      if (length(terms) > 0) syn <- c(syn, paste0(t_name, " =~ ", paste(terms, collapse = " + \n    ")))
    }
    
    # 2. Trait Covariances
    syn <- c(syn, "\n# 2. Trait Variances (Fixed to 1) and Covariances")
    for (t_name in trait_names) syn <- c(syn, paste0(t_name, " ~~ 1 * ", t_name))
    if (n_traits > 1) {
      for (i in 1:(n_traits - 1)) {
        for (j in (i + 1):n_traits) {
          t1 <- trait_names[i]; t2 <- trait_names[j]; cor_val <- cor_matrix[t1, t2]
          syn <- c(syn, paste0(t1, " ~~ start(", cor_val, ") * ", t2))
        }
      }
    }
    
    # 3. Residual Variances & Correlated Errors
    syn <- c(syn, "\n# 3. Pair Uniquenesses & Correlated Errors")
    if (block_size == 2) {
      syn <- c(syn, "# Fixed for identification in blocks of 2")
      for (p in pair_names) syn <- c(syn, paste0(p, " ~~ 1 * ", p))
    } else {
      for (p in 1:n_pairs) {
        i <- Pairs[p, 1]; k <- Pairs[p, 2]
        syn <- c(syn, paste0(pair_names[p], " ~~ e", i, "e", k, " * ", pair_names[p], " + start(2) * ", pair_names[p]))
      }
      
      syn <- c(syn, "\n# Correlated Errors (Structured Local Dependencies)")
      for (b in 1:n_blocks) {
        start_pair <- (b - 1) * block_size * (block_size - 1) / 2 + 1
        end_pair <- b * block_size * (block_size - 1) / 2
        for (p1 in start_pair:(end_pair - 1)) {
          for (p2 in (p1 + 1):end_pair) {
            i1 <- Pairs[p1, 1]; k1 <- Pairs[p1, 2]
            i2 <- Pairs[p2, 1]; k2 <- Pairs[p2, 2]
            
            if (i1 == i2) {
              syn <- c(syn, paste0(pair_names[p1], " ~~ e", i1, " * ", pair_names[p2], " + start(1)*", pair_names[p2]))
            } else if (k1 == i2) {
              syn <- c(syn, paste0(pair_names[p1], " ~~ e", k1, "_n * ", pair_names[p2], " + start(-1)*", pair_names[p2]))
            } else if (k1 == k2) {
              syn <- c(syn, paste0(pair_names[p1], " ~~ e", k1, " * ", pair_names[p2], " + start(1)*", pair_names[p2]))
            }
          }
        }
      }
    }
    
    # 4. Constraints
    if (block_size > 2) {
      syn <- c(syn, "\n# 4. Model Constraints")
      for (b in 1:n_blocks) {
        for (i in 2:(block_size - 1)) {
          item <- (b - 1) * block_size + i
          syn <- c(syn, paste0("L", item, "_n == -L", item))
        }
      }
      if (block_size > 3) {
        for (b in 1:n_blocks) {
          for (i in 2:(block_size - 1)) {
            item <- (b - 1) * block_size + i
            syn <- c(syn, paste0("e", item, "_n == -e", item))
          }
        }
      }
      for (p in 1:n_pairs) {
        i <- Pairs[p, 1]; k <- Pairs[p, 2]
        e_str <- paste0("e", i, "e", k, " == ")
        if (block_size != 3) {
          syn <- c(syn, paste0(e_str, "e", i, " + e", k))
        } else {
          if (((p - 1) %% 3) == 0) syn <- c(syn, paste0(e_str, "e", i, " - e", k, "_n"))
          else if ((p %% 3) == 0) syn <- c(syn, paste0(e_str, "-e", i, "_n + e", k))
          else syn <- c(syn, paste0(e_str, "e", i, " + e", k))
        }
      }
      syn <- c(syn, "\n# Identification")
      for (b in 1:n_blocks) {
        first_item <- (b - 1) * block_size + 1
        syn <- c(syn, paste0("e", first_item, " == 1"))
      }
      
      # 5. Prevent Heywood Cases (First-Order Specific)
      if (force_positive_variances) {
        syn <- c(syn, "\n# 5. Prevent Heywood Cases (Force utility variances > 0)")
        for (b in 1:n_blocks) {
          for (i in 2:block_size) {
            item <- (b - 1) * block_size + i
            # In triplets, item 2's variance is stored as the negative version (-e2_n)
            if (block_size == 3 && i == 2) {
              syn <- c(syn, paste0("e", item, "_n < -0.0001"))
            } else {
              syn <- c(syn, paste0("e", item, " > 0.0001"))
            }
          }
        }
      }
    }
    syn <- c(syn, more_constraints)
  }
  
  return(paste(syn, collapse = "\n"))
}
