#' Generate Mplus Syntax for Thurstonian IRT (TIRT) or Thurstonian Factor Model (TFM)
#'
#' @param title Character. Title of the Mplus model.
#' @param data_file Character. Name of the input data file.
#' @param n_blocks Integer. Number of blocks in the questionnaire.
#' @param block_size Integer. Number of items per block.
#' @param key_matrix A data.frame with two columns, one indicating item trait,
#'        another indicating item sign.
#' @param trait_col Character string. The name of the column in \code{key_matrix} indicating the trait
#'        measured by the item. 
#' @param key_col Character string. The name of the column in \code{key_matrix} indicating the keying 
#'        direction of the item (e.g., positive/negative). 
#' @param cor_matrix Optional matrix. Starting values for trait correlations.
#' @param model_type Character. Choose "TIRT" for the First-Order parameterization 
#'        or "TFM" for the Second-Order parameterization.
#' @param tfm_free_pair_residuals Logical. In the TFM model, should the residual variance
#'        of the observed pairs be freely estimated? Defaults to TRUE.
#' @param estimator Character. The Mplus estimator to use ("WLSMV", "ULSMV", "ULS"). 
#' @param missing_code Numeric or Character. The value representing missing data.
#' @param force_positive_variances Logical. Constrain residual variances to be 
#'        strictly positive (> 0.001/0) to prevent Heywood cases. Defaults to TRUE.
#' @param quick_run Logical. Specify TRUE to speed up estimation by not producing 
#'        Chi-Square and standard errors. Defaults to FALSE.
#' @param out_file Character. The filename to save the generated Mplus syntax.
#'
#' @export
generate_tirt_mplus_syntax <- function(title = "TIRT Model", 
                                       data_file, 
                                       n_blocks, 
                                       block_size, 
                                       key_matrix, 
                                       trait_col,
                                       key_col,
                                       cor_matrix = NULL,
                                       model_type = c("TIRT", "TFM"),
                                       tfm_free_pair_residuals = TRUE,
                                       estimator = c("WLSMV", "ULSMV", "ULS"),
                                       missing_code = "*",
                                       force_positive_variances = TRUE,
                                       quick_run = FALSE,
                                       out_file = "tirt_model.inp") {
  
  estimator <- match.arg(toupper(estimator), choices = c("WLSMV", "ULSMV", "ULS"))
  model_type <- match.arg(toupper(model_type), choices = c("TIRT", "TFM"))
  
  n_items <- n_blocks * block_size
  n_pairs <- n_blocks * (block_size * (block_size - 1) / 2)
  
  if (nrow(key_matrix) != n_items) stop("key_matrix must have exactly ", n_items, " rows.")
  
  raw_traits <- unique(as.character(key_matrix$trait))
  n_traits <- length(raw_traits)
  mplus_traits <- gsub("[^A-Za-z0-9_]", "", raw_traits)
  
  if (is.null(cor_matrix)) {
    cor_matrix <- diag(n_traits)
    rownames(cor_matrix) <- mplus_traits; colnames(cor_matrix) <- mplus_traits
  } else {
    if (is.null(rownames(cor_matrix)) || is.null(colnames(cor_matrix))) {
      rownames(cor_matrix) <- mplus_traits; colnames(cor_matrix) <- mplus_traits
    } else {
      rownames(cor_matrix) <- gsub("[^A-Za-z0-9_]", "", rownames(cor_matrix))
      colnames(cor_matrix) <- gsub("[^A-Za-z0-9_]", "", colnames(cor_matrix))
      cor_matrix <- cor_matrix[mplus_traits, mplus_traits, drop = FALSE]
    }
  }
  
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
  add_line <- function(...) syn <<- c(syn, paste0(...))
  
  add_line("TITLE: ", title)
  add_line("DATA: FILE IS '", data_file, "';")
  
  add_line("VARIABLE:")
  add_line("  NAMES ARE")
  wrapped_names <- strwrap(paste(pair_names, collapse = " "), width = 75)
  for (w in wrapped_names) add_line("    ", w)
  add_line("  ;")
  add_line("  USEVARIABLES ARE ", pair_names[1], "-", pair_names[n_pairs], ";")
  add_line("  CATEGORICAL ARE ALL;")
  
  if (!is.na(missing_code) && missing_code != "") {
    add_line(sprintf("  MISSING ARE %s;", missing_code))
  } else {
    add_line("  MISSING ARE (NA);")
  }
  
  add_line("ANALYSIS:")
  add_line("  ESTIMATOR = ", estimator, ";")
  add_line("  PARAMETERIZATION = THETA;")
  
  add_line("MODEL:")
  
  # =========================================================================
  # OPTION 1: TIRT 
  # =========================================================================
  if (model_type == "TIRT") {
    
    add_line("\n  ! 1. Factor Loadings")
    for (t_idx in 1:n_traits) {
      t_name <- mplus_traits[t_idx]; raw_name <- raw_traits[t_idx]
      add_line("  ", t_name, " BY")
      
      terms <- c()
      for (p in 1:n_pairs) {
        i <- Pairs[p, 1]; k <- Pairs[p, 2]
        if (key_matrix$trait[i] == raw_name) {
          terms <- c(terms, paste0("    ", pair_names[p], "*", key_matrix$sign[i], " (L", i, ")"))
        }
        if (key_matrix$trait[k] == raw_name) {
          terms <- c(terms, paste0("    ", pair_names[p], "*", -1 * key_matrix$sign[k], " (L", k, "_n)"))
        }
      }
      for (term in terms) add_line(term)
      syn[length(syn)] <- paste0(syn[length(syn)], ";")
    }
    
    add_line("\n  ! 2. Trait Covariances")
    for (t_name in mplus_traits) add_line("  ", t_name, "@1;")
    if (n_traits > 1) {
      for (i in 1:(n_traits - 1)) {
        for (j in (i + 1):n_traits) add_line("  ", mplus_traits[i], " WITH ", mplus_traits[j], "*", cor_matrix[i, j], ";")
      }
    }
    
    add_line("\n  ! 3. Pair Uniquenesses & Correlated Errors")
    if (block_size == 2) {
      wrapped_pairs <- strwrap(paste(pair_names, collapse = " "), width = 75)
      for (w in wrapped_pairs) add_line("  ", w, "@1;")
    } else {
      for (p in 1:n_pairs) add_line("  ", pair_names[p], "*2 (e", Pairs[p, 1], "e", Pairs[p, 2], ");")
      
      add_line("\n  ! Correlated Errors within blocks")
      for (b in 1:n_blocks) {
        start_pair <- (b - 1) * (block_size * (block_size - 1) / 2) + 1
        end_pair <- b * (block_size * (block_size - 1) / 2)
        for (p1 in start_pair:(end_pair - 1)) {
          for (p2 in (p1 + 1):end_pair) {
            i1 <- Pairs[p1, 1]; k1 <- Pairs[p1, 2]
            i2 <- Pairs[p2, 1]; k2 <- Pairs[p2, 2]
            if (i1 == i2) add_line("  ", pair_names[p1], " WITH ", pair_names[p2], "*1 (e", i1, ");")
            else if (k1 == i2) add_line("  ", pair_names[p1], " WITH ", pair_names[p2], "*-1 (e", k1, "_n);")
            else if (k1 == k2) add_line("  ", pair_names[p1], " WITH ", pair_names[p2], "*1 (e", k1, ");")
          }
        }
      }
    }
    
    if (block_size > 2) {
      add_line("\nMODEL CONSTRAINT:")
      for (b in 1:n_blocks) {
        for (i in 2:(block_size - 1)) add_line("  L", (b - 1) * block_size + i, "_n = -L", (b - 1) * block_size + i, ";")
      }
      if (block_size > 3) {
        for (b in 1:n_blocks) {
          for (i in 2:(block_size - 1)) add_line("  e", (b - 1) * block_size + i, "_n = -e", (b - 1) * block_size + i, ";")
        }
      }
      for (p in 1:n_pairs) {
        i <- Pairs[p, 1]; k <- Pairs[p, 2]
        e_str <- paste0("  e", i, "e", k, " = ")
        if (block_size != 3) add_line(e_str, "e", i, " + e", k, ";")
        else {
          if (((p - 1) %% 3) == 0) add_line(e_str, "e", i, " - e", k, "_n;")
          else if ((p %% 3) == 0) add_line(e_str, "-e", i, "_n + e", k, ";")
          else add_line(e_str, "e", i, " + e", k, ";")
        }
      }
      add_line("\n  ! Identification Constraints")
      for (b in 1:n_blocks) add_line("  e", (b - 1) * block_size + 1, " = 1;")
      
      if (force_positive_variances) {
        add_line("\n  ! Prevent Heywood Cases (Strictly bound utility variances)")
        for (b in 1:n_blocks) {
          for (i in 2:block_size) {
            item <- (b - 1) * block_size + i
            if (block_size == 3 && i == 2) add_line("  e", item, "_n < 0;") 
            else add_line("  e", item, " > 0;")
          }
        }
      }
    }
  } 
  # =========================================================================
  # OPTION 2: TFM 
  # =========================================================================
  else if (model_type == "TFM") {
    
    add_line("\n  ! 1. First-Order Utilities (y* = t_i - t_k)")
    for (item in 1:n_items) {
      pos_pairs <- pair_names[Pairs[, 1] == item]; neg_pairs <- pair_names[Pairs[, 2] == item]
      line <- paste0("  t", item, " BY")
      if (length(pos_pairs) > 0) line <- paste0(line, " ", paste0(pos_pairs, "@1", collapse = " "))
      if (length(neg_pairs) > 0) line <- paste0(line, " ", paste0(neg_pairs, "@-1", collapse = " "))
      for (w in strwrap(line, width = 75, exdent = 4)) add_line(w)
      syn[length(syn)] <- paste0(syn[length(syn)], ";")
    }
    
    add_line("\n  ! 2. Second-Order Structure (Traits load onto Utilities)")
    for (t_idx in 1:n_traits) {
      t_name <- mplus_traits[t_idx]; raw_name <- raw_traits[t_idx]
      for (i in which(as.character(key_matrix$trait) == raw_name)) {
        add_line("  ", t_name, " BY t", i, "*", key_matrix$sign[i], " (L", i, ");")
      }
    }
    
    add_line("\n  ! 3. Trait Variances (Fixed to 1) and Covariances")
    for (t_name in mplus_traits) add_line("  ", t_name, "@1;")
    if (n_traits > 1) {
      for (i in 1:(n_traits - 1)) {
        for (j in (i + 1):n_traits) add_line("  ", mplus_traits[i], " WITH ", mplus_traits[j], "*", cor_matrix[i, j], ";")
      }
    }
    
    add_line("\n  ! 4. Utility Uniquenesses")
    for (b in 1:n_blocks) {
      items_in_block <- ((b - 1) * block_size + 1):(b * block_size)
      if (block_size == 2) {
        for (i in items_in_block) add_line("  t", i, "@0.5;")
      } else {
        for (j in 2:block_size) add_line("  t", items_in_block[j], "*1 (v", items_in_block[j], ");")
        add_line("  t", items_in_block[1], "@1;")
      }
    }
    
    add_line("\n  ! 5. Pair Residual Variances")
    add_line("  ! FIXED to 0 to be consistent with TFM conceptualization")
    for (w in pair_names) add_line("  ", w, "@0;")
  
    
    if (force_positive_variances && block_size > 2) {
      add_line("\nMODEL CONSTRAINT:")
      add_line("  ! Prevent Heywood Cases on Utilities")
      for (b in 1:n_blocks) {
        for (j in 2:block_size) add_line("  v", ((b - 1) * block_size + j), " > 0;")
      }
      }
      
    
  }
  
  # --- OUTPUT ---
  add_line("\nOUTPUT:")
  if (n_traits > 10 || quick_run) add_line("  NOSERR; NOCHISQUARE;")
  
  # if (!is.null(score_file)) {
  #   add_line("\nSAVEDATA:")
  #   add_line("  ESTIMATES = 'calibration_estimates.dat';")
  #   
  #   if (model_type == "TIRT") {
  #     add_line("  FILE IS '", score_file, "';")
  #     add_line("  SAVE = FSCORES;")
  #   } else {
  #     add_line("  ! NOTE: FSCORES suppressed for TFM because Mplus crashes when")
  #     add_line("  ! categorical residuals are 0. Use the R scoring function instead!")
  #   }
  # }
  
  writeLines(syn, out_file)
  invisible(syn)
}