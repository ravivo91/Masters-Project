# -----------------------------------------------------------------------------
#
# Title: Advanced Correlation and Regression Analysis for Vision Science Data
# Author: Gemini (Revised from user's script)
# Date: 2025-09-07
#
# Purpose:
#   This script provides a comprehensive framework for exploring relationships
#   in clinical vision science data. It performs three levels of analysis:
#   1. Bivariate Correlation: Pearson's r for all pairs.
#   2. Univariate Regression: Simple linear models for all pairs (y ~ x).
#   3. Multivariate Regression: Multiple linear models for each outcome against
#      ALL predictors AND specified covariates (e.g., Age, Sex) simultaneously.
#
# Key Features:
#   - Automated discovery of outcome, predictor, and covariate variables.
#   - Ability to load and merge a separate demographics file from a specific sheet.
#   - Tidy output tables for all analyses, ready for reporting.
#   - Publication-quality visualizations that are adjusted for covariates.
#
# -----------------------------------------------------------------------------

# ------------------------------
# 1. SETUP & CONFIGURATION
# ------------------------------

# Install and load required packages
required_pkgs <- c(
  "tidyverse", # For data manipulation (dplyr, tidyr) and plotting (ggplot2)
  "readxl",    # To read Excel files
  "ggpubr",    # For adding stats (R, p-value) to plots
  "ggcorrplot",# For creating beautiful correlation heatmaps
  "ggforce",   # For paginating ggplot facets
  "broom"      # For tidying model outputs
)

# Check for and install missing packages
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)

# Load libraries for the session
lapply(required_pkgs, library, character.only = TRUE)

# --- User Configuration ---

# Set your project directory and file paths
# NOTE: R prefers forward slashes "/" for paths, even on Windows.
project_dir <- "E:/iCloudDrive/Desktop/Rstudio/Correlation Code"
input_file  <- "Merged_AllSubjects_Wide_V2.xlsx"

# --- NEW: Configuration for Demographics/Covariates ---
# Set the filename for your demographics data.
demographics_file <- "E:/iCloudDrive/Desktop/Rstudio/RLS_MasterDataForm_Rfriendly.xlsx" 
# Set the specific sheet name within the Excel file to load.
demographics_sheet <- "VisionTests"

# Set the exact column name for the subject identifier.
# This column name must be identical in both your main and demographics files.
subject_id_col    <- "SubjectID"

# --- NEW: Create a timestamped folder for each run ---
# Define the base directory for all results
base_output_dir <- file.path(project_dir, "Advanced_Correlation_Results")
# Create a timestamp string (e.g., "2025-09-07_12-06-00")
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
# Combine the base directory and timestamp to create a unique output folder for this run
output_dir <- file.path(base_output_dir, timestamp)


# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- Plotting Options ---
facets_per_page <- 12
facet_ncol      <- 4

# ------------------------------
# 2. LOAD & PREPARE DATA
# ------------------------------
full_path <- file.path(project_dir, input_file)
if (!file.exists(full_path)) stop("Input file not found at: ", full_path)
raw_data <- read_excel(full_path) %>% as.data.frame()

# Clean column names to make them valid R variable names
original_names <- names(raw_data)
# Ensure Subject ID column name is consistent before cleaning
names(raw_data)[names(raw_data) == subject_id_col] <- "SubjectID"
names(raw_data) <- make.names(names(raw_data), unique = TRUE)
name_key <- data.frame(original_name = original_names, clean_name = names(raw_data))
# Set the cleaned subject ID column name for merging
subject_id_col_clean <- make.names(subject_id_col)


# --- NEW: MERGE DEMOGRAPHIC DATA ---
demographics_data_loaded <- FALSE
if (!is.null(demographics_file) && nzchar(demographics_file)) {
  demographics_path <- file.path(project_dir, demographics_file)
  if (file.exists(demographics_path)) {
    message(paste("Loading demographic data from sheet:", demographics_sheet))
    demographics_data <- read_excel(demographics_path, sheet = demographics_sheet) %>% as.data.frame()
    
    # Ensure Subject ID column name is consistent before cleaning
    names(demographics_data)[names(demographics_data) == subject_id_col] <- "SubjectID"
    names(demographics_data) <- make.names(names(demographics_data), unique = TRUE)
    
    if (subject_id_col_clean %in% names(raw_data) && subject_id_col_clean %in% names(demographics_data)) {
      # Select only new columns from demographics to avoid duplication, except for the key
      new_cols <- names(demographics_data)[!names(demographics_data) %in% names(raw_data)]
      raw_data <- left_join(raw_data, demographics_data[, c(subject_id_col_clean, new_cols)], by = subject_id_col_clean)
      message("Successfully merged demographic data.")
      demographics_data_loaded <- TRUE
    } else {
      warning(paste("Could not merge. Make sure both files have the ID column:", subject_id_col))
    }
  } else {
    warning("Demographics file specified but not found. Proceeding without it.")
  }
}

# --- TRANSFORM THRESHOLD TO LOG THRESHOLD ---
# Find all columns related to contrast or threshold
threshold_cols <- grep("(?i)contrast|threshold", names(raw_data), value = TRUE)
# Exclude any 'on.off' ratio columns from transformation
pat_exclude_on_off  <- "(?i)on[._]?off"
threshold_cols <- threshold_cols[!grepl(pat_exclude_on_off, threshold_cols)]

message("Transforming the following columns to Log Threshold:")
message(paste(" -", threshold_cols, collapse = "\n"))

# Loop through the identified columns and apply the transformation
for (col in threshold_cols) {
  raw_data[[col]] <- as.numeric(raw_data[[col]])
  raw_data[[col]][raw_data[[col]] <= 0] <- NA 
  raw_data[[col]] <- log10(raw_data[[col]])
  new_name <- gsub("(?i)contrast.threshold|contrast|threshold", "log.threshold", col, perl = TRUE)
  names(raw_data)[names(raw_data) == col] <- new_name
  name_key$clean_name[name_key$clean_name == col] <- new_name
}

write.csv(name_key, file.path(output_dir, "_00_column_name_key.csv"), row.names = FALSE)


# ------------------------------
# 3. VARIABLE DISCOVERY (REVISED FOR MATCHED ANALYSIS)
# ------------------------------
all_clean_names <- names(raw_data)

# --- Predictor Discovery ---
erg_predictors_all <- grep("(?i)ERG", all_clean_names, value = TRUE)
vep_predictors_all <- grep("(?i)VEP", all_clean_names, value = TRUE)
all_predictors_base <- unique(c(erg_predictors_all, vep_predictors_all))
all_predictors <- all_predictors_base[!grepl(pat_exclude_on_off, all_predictors_base)]

# --- Split predictors into ON and OFF groups ---
pat_on       <- "(?i)(^|[_.-])on([_.-]|$)"
pat_off      <- "(?i)(^|[_.-])off([_.-]|$)"
on_predictors  <- all_predictors[grepl(pat_on, all_predictors) & !grepl(pat_exclude_on_off, all_predictors)]
off_predictors <- all_predictors[grepl(pat_off, all_predictors) & !grepl(pat_exclude_on_off, all_predictors)]

# --- Outcome Discovery ---
pat_near_on  <- "(?i)near[._]?on[._]?log.threshold"
pat_near_off <- "(?i)near[._]?off[._]?log.threshold"
pat_far_on   <- "(?i)far[._]?on[._]?log.threshold"
pat_far_off  <- "(?i)far[._]?off[._]?log.threshold"

near_on_outcomes  <- grep(pat_near_on, all_clean_names, value = TRUE)
near_off_outcomes <- grep(pat_near_off, all_clean_names, value = TRUE)
far_on_outcomes   <- grep(pat_far_on, all_clean_names, value = TRUE)
far_off_outcomes  <- grep(pat_far_off, all_clean_names, value = TRUE)

# --- NEW: Covariate Discovery ---
pat_covariates <- "(?i)age|sex|axial|refractive|length|error|AXL|IOP|CCT|ACD|K1|K2|Pupil"
all_covariates <- grep(pat_covariates, all_clean_names, value = TRUE)
# Exclude any variables that are actually predictors or outcomes
all_covariates <- all_covariates[!all_covariates %in% c(all_predictors, unlist(outcome_groups))]
# Exclude subject ID from covariates
all_covariates <- all_covariates[!grepl("(?i)subject", all_covariates)]


# --- Final Grouping ---
outcome_groups <- list(
  Near_ON_Threshold  = near_on_outcomes,
  Near_OFF_Threshold = near_off_outcomes,
  Far_ON_Threshold   = far_on_outcomes,
  Far_OFF_Threshold  = far_off_outcomes
)
outcome_groups <- Filter(function(x) length(x) > 0, outcome_groups)

# --- Verification Report ---
report_lines <- c(
  "--- VARIABLE DISCOVERY REPORT ---",
  "\n[ON PREDICTORS]", if(length(on_predictors)>0) paste0("  - ", paste(on_predictors, collapse=", ")) else "  - None Found",
  "\n[OFF PREDICTORS]", if(length(off_predictors)>0) paste0("  - ", paste(off_predictors, collapse=", ")) else "  - None Found"
)
for (group_name in names(outcome_groups)) {
  report_lines <- c(report_lines,
                    paste0("\n[OUTCOMES - ", gsub("_", " ", group_name), "]"),
                    if(length(outcome_groups[[group_name]]) > 0) paste0("  - ", paste(outcome_groups[[group_name]], collapse=", ")) else "  - None Found")
}
report_lines <- c(report_lines, "\n[COVARIATES]", if(length(all_covariates)>0) paste0("  - ", paste(all_covariates, collapse=", ")) else "  - None Found")
writeLines(report_lines, file.path(output_dir, "_01_variable_report.txt"))

analysis_data <- raw_data

# ---------------------------------------
# 4. STATISTICAL ANALYSIS FUNCTIONS
# ---------------------------------------
calculate_all_correlations <- function(data, outcome_list, predictor_list) {
  if (length(unlist(outcome_list)) == 0 || length(predictor_list) == 0) return(data.frame())
  numeric_data <- data %>% mutate(across(everything(), ~suppressWarnings(as.numeric(.))))
  tidyr::expand_grid(outcome = unlist(outcome_list) %>% unique(), predictor = predictor_list) %>%
    rowwise() %>%
    mutate(test = list(tryCatch({
      ct <- cor.test(numeric_data[[outcome]], numeric_data[[predictor]], method = "pearson")
      data.frame(r = ct$estimate, p.value = ct$p.value, n = sum(complete.cases(numeric_data[, c(outcome, predictor)])))
    }, error = function(e) { data.frame(r = NA, p.value = NA, n = NA) }))) %>%
    ungroup() %>% tidyr::unnest(test)
}
run_multivariate_model <- function(data, outcome_var, predictor_vars, covariate_vars = NULL) {
  all_model_vars <- c(predictor_vars, covariate_vars)
  if (length(all_model_vars) == 0) return(NULL)
  numeric_data <- data %>% mutate(across(everything(), ~suppressWarnings(as.numeric(.))))
  predictor_formula <- paste0("`", all_model_vars, "`", collapse = " + ")
  formula <- as.formula(paste0("`", outcome_var, "` ~ ", predictor_formula))
  tryCatch({
    model <- lm(formula, data = numeric_data)
    list(
      coeffs = broom::tidy(model, conf.int = TRUE),
      glance = broom::glance(model)
    )
  }, error = function(e) {
    message("Could not fit multivariate model for: ", outcome_var, ". Error: ", e$message)
    return(NULL)
  })
}

# ------------------------------
# 5. PLOTTING FUNCTIONS
# ------------------------------
# REVISED FUNCTION: Generates a summary heatmap with Pearson's R, for ON or OFF pathways
generate_summary_heatmap <- function(correlation_df, out_dir, pathway_type) {
  if (is.null(correlation_df) || nrow(correlation_df) == 0) {
    message(paste("--> Skipping", pathway_type, "summary heatmap: no correlation data to plot."))
    return(NULL)
  }
  
  plot_title <- paste("Correlation Heatmap:", pathway_type, "Pathway")
  
  # Clean labels for plotting
  plot_data <- correlation_df %>%
    mutate(
      outcome = gsub("(?i)log[._]threshold", "Log Sensitivity", outcome),
      outcome = gsub("[._]", " ", outcome),
      predictor = gsub("[._]", " ", predictor)
    )
  
  p <- ggplot(plot_data, aes(x = outcome, y = predictor, fill = r)) +
    geom_tile(color = "grey50", lwd = 0.5) +
    geom_text(aes(label = round(r, 2)), color = "black", size = 3) + 
    scale_fill_gradient2(
      low = "#053061", mid = "white", high = "#67001f",
      midpoint = 0, limit = c(-1, 1), name = "Pearson's R"
    ) +
    labs(
      title = plot_title,
      subtitle = "Color indicates correlation direction/strength; text is Pearson's R",
      x = "Psychophysics Outcome (Log Sensitivity)",
      y = "ERG / VEP Predictor"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
      axis.text.y = element_text(size = 10, face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "right",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(
    filename = file.path(out_dir, paste0("A_Summary_Heatmap_", pathway_type, ".png")),
    plot = p,
    width = 12,
    height = max(8, 0.4 * n_distinct(correlation_df$predictor)),
    dpi = 300,
    limitsize = FALSE
  )
}
plot_multivariate_coeffs <- function(model_coeffs, outcome_name, out_dir, file_suffix = "", covariates_to_exclude = NULL) {
  if (is.null(model_coeffs) || nrow(model_coeffs) == 0) return(NULL)
  
  # Exclude covariates from the plot for clarity
  plot_data <- model_coeffs %>%
    filter(term != "(Intercept)") %>%
    filter(!term %in% covariates_to_exclude) %>%
    mutate(term = gsub("[._]", " ", term))
  
  subtitle_text <- paste("Outcome:", gsub("[._]", " ", gsub("(?i)log[._]threshold", "Log Sensitivity", outcome_name)))
  if (!is.null(covariates_to_exclude) && length(covariates_to_exclude) > 0) {
    subtitle_text <- paste0(subtitle_text, "\n(Model controlled for covariates like Age, Sex, etc.)")
  }
  
  p <- ggplot(plot_data, aes(x = estimate, y = reorder(term, estimate))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(color = "darkblue", size = 3) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "darkblue") +
    labs(
      title = "Multivariate Model Coefficients",
      subtitle = subtitle_text,
      x = "Coefficient Estimate (Effect Size)",
      y = "Predictor Variable"
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  ggsave(
    filename = file.path(out_dir, paste0("C_MultiCoeffs_", make.names(outcome_name), file_suffix, ".png")),
    plot = p, width = 8, height = max(4, 0.25 * nrow(plot_data)), dpi = 300, limitsize = FALSE
  )
}
generate_model_heatmap <- function(model_results_df, out_dir, covariates_to_exclude = NULL) {
  if(is.null(model_results_df) || nrow(model_results_df) == 0) {
    message("--> Skipping multivariate heatmap: no model results to plot.")
    return(NULL)
  }
  
  heatmap_data <- model_results_df %>%
    filter(term != "(Intercept)") %>%
    filter(!term %in% covariates_to_exclude) %>%
    mutate(
      p_category = cut(p.value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("<0.001", "<0.01", "<0.05", "NS")),
      estimate_label = round(estimate, 2),
      term = gsub("[._]", " ", term),
      outcome = gsub("(?i)log[._]threshold", "Log Sensitivity", outcome),
      outcome = gsub("[._]", " ", outcome)
    )
  
  subtitle_text <- "Cell color indicates p-value; text shows coefficient estimate"
  if (!is.null(covariates_to_exclude) && length(covariates_to_exclude) > 0) {
    subtitle_text <- paste0(subtitle_text, "\n(All models controlled for covariates like Age, Sex, etc.)")
  }
  
  p <- ggplot(heatmap_data, aes(x = term, y = outcome, fill = p_category)) +
    geom_tile(color = "white", lwd = 1.5) +
    geom_text(aes(label = estimate_label), color = "black", size = 3) +
    scale_fill_manual(values = c("<0.001" = "firebrick", "<0.01" = "salmon", "<0.05" = "lightpink", "NS" = "grey90"), name = "P-value") +
    labs(
      title = "Multivariate Model Results Heatmap",
      subtitle = subtitle_text,
      x = "Predictor Variable (ERG / VEP)",
      y = "Outcome Variable (Log Sensitivity)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(filename = file.path(out_dir, "D_Multivariate_Model_Heatmap.png"), plot = p, width = 12, height = 8, dpi = 300)
}
generate_megaplot <- function(data, outcomes_to_plot, predictors_to_plot, group_name, out_dir) {
  if (length(outcomes_to_plot) == 0 || length(predictors_to_plot) == 0) { message("Skipping megaplot for ", group_name, " as no outcome variables were found."); return(NULL) }
  
  long_data <- data %>% 
    dplyr::select(all_of(c(outcomes_to_plot, predictors_to_plot))) %>% 
    mutate(across(everything(), ~suppressWarnings(as.numeric(.)))) %>% 
    tidyr::pivot_longer(cols = all_of(predictors_to_plot), names_to = "predictor_variable", values_to = "predictor_value") %>% 
    tidyr::pivot_longer(cols = all_of(outcomes_to_plot), names_to = "outcome_variable", values_to = "outcome_value") %>% 
    filter(complete.cases(.)) %>%
    mutate(
      predictor_variable = gsub("[._]", " ", predictor_variable),
      outcome_variable = gsub("(?i)log[._]threshold", "Log Sensitivity", outcome_variable),
      outcome_variable = gsub("[._]", " ", outcome_variable)
    )
  
  if (nrow(long_data) == 0) { message("Skipping megaplot for ", group_name, " due to lack of complete data pairs."); return(NULL) }
  
  p <- ggplot(long_data, aes(x = predictor_value, y = outcome_value)) + 
    geom_point(alpha = 0.7, color = "steelblue") + 
    geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 0.7) + 
    stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", p.accuracy = 0.001, r.accuracy = 0.01, size = 2.5) + 
    theme_bw(base_size = 10) + 
    theme(
      strip.background = element_rect(fill = "gray90"), 
      strip.text = element_text(face = "bold", size = 7),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color = NA)
    ) + 
    labs(x = "Predictor Value", y = "Outcome Value (Log Sensitivity)", title = paste("Detailed Scatterplots:", gsub("(?i)Threshold", "Sensitivity", gsub("_", " ", group_name))))
  facet_rows <- ceiling(facets_per_page / facet_ncol)
  
  # Calculate the number of pages needed
  paginated_plot <- p + facet_wrap_paginate(~ outcome_variable + predictor_variable, ncol = facet_ncol, nrow = facet_rows, scales = "free")
  n_pages <- n_pages(paginated_plot)
  
  # Loop through each page if there are any to plot
  if (n_pages > 0) {
    for (i in 1:n_pages) {
      # Create the specific plot for the current page by adding the paginated facet to the base plot `p`
      plot_for_page <- p + facet_wrap_paginate(~ outcome_variable + predictor_variable, 
                                               ncol = facet_ncol, nrow = facet_rows, 
                                               scales = "free", page = i)
      
      ggsave(
        filename = file.path(out_dir, paste0("B_MegaPlot_", group_name, "_Page_", i, ".png")),
        plot = plot_for_page, 
        width = 11, height = 8.5, dpi = 300
      )
    }
  }
}

# ------------------------------
# 6. EXECUTE BIVARIATE ANALYSIS & PLOTS
# ------------------------------
message("Running Bivariate Correlations and Scatterplots (Matched ON/OFF)...")
bivariate_results_list <- list()
for (group_name in names(outcome_groups)) {
  current_outcomes <- outcome_groups[[group_name]]
  
  if (grepl("ON", group_name, ignore.case = TRUE)) {
    predictors_to_use <- on_predictors
  } else if (grepl("OFF", group_name, ignore.case = TRUE)) {
    predictors_to_use <- off_predictors
  } else {
    next
  }
  
  if(length(predictors_to_use) > 0 && length(current_outcomes) > 0) {
    message(paste("  Processing bivariate analysis for", group_name))
    # Add group name to the results for easier filtering later
    cor_results <- calculate_all_correlations(analysis_data, current_outcomes, predictors_to_use)
    if(nrow(cor_results) > 0) {
      cor_results$group <- group_name
      bivariate_results_list[[group_name]] <- cor_results
    }
    
    # Generate the detailed scatterplots (B plots)
    generate_megaplot(analysis_data, current_outcomes, predictors_to_use, group_name, output_dir)
  }
}
all_correlations_table <- bind_rows(bivariate_results_list)
if(nrow(all_correlations_table) > 0) {
  write.csv(all_correlations_table, file.path(output_dir, "_02_bivariate_correlations.csv"), row.names = FALSE)
} else {
  message("--> Bivariate correlation table is empty. No file written.")
}

# ----------------------------------------------------
# 6.1. GENERATE SUMMARY CORRELATION HEATMAPS (REVISED)
# ----------------------------------------------------
message("\nGenerating summary heatmaps for Psychophysics vs ERG/VEP...")

# Filter data for ON and OFF pathways
on_cor_data <- all_correlations_table %>% filter(grepl("ON", group, ignore.case = TRUE))
off_cor_data <- all_correlations_table %>% filter(grepl("OFF", group, ignore.case = TRUE))

# Generate separate heatmaps
generate_summary_heatmap(on_cor_data, output_dir, pathway_type = "ON")
generate_summary_heatmap(off_cor_data, output_dir, pathway_type = "OFF")

message("  Summary heatmaps saved.")

# ----------------------------------------------------
# 7. MULTIVARIATE ANALYSES FOR EACH OUTCOME
# ----------------------------------------------------
message("\nRunning multivariate analyses for each psychophysics outcome (Matched ON/OFF)...")
all_multivariate_results_list <- list()
file_prefix_counter <- 3

for (outcome_group_name in names(outcome_groups)) {
  specific_outcomes <- outcome_groups[[outcome_group_name]]
  
  if (grepl("ON", outcome_group_name, ignore.case = TRUE)) {
    predictors_to_use <- on_predictors
  } else if (grepl("OFF", outcome_group_name, ignore.case = TRUE)) {
    predictors_to_use <- off_predictors
  } else {
    next
  }
  
  # Loop through each outcome in the group
  for (specific_outcome in specific_outcomes) {
    message(paste0("\n-- Processing model for: ", specific_outcome, " --"))
    if (length(predictors_to_use) > 0) {
      specific_model_output <- run_multivariate_model(analysis_data, specific_outcome, predictors_to_use, covariate_vars = all_covariates)
      
      if (!is.null(specific_model_output)) {
        specific_results_df <- specific_model_output$coeffs
        specific_results_df$outcome <- specific_outcome # Add outcome column
        all_multivariate_results_list[[specific_outcome]] <- specific_results_df
        
        file_name_csv <- paste0("_", sprintf("%02d", file_prefix_counter), "_multivariate_", make.names(specific_outcome), ".csv")
        write.csv(specific_results_df, file.path(output_dir, file_name_csv), row.names = FALSE)
        message("  Successfully saved multivariate results to ", file_name_csv)
        
        plot_multivariate_coeffs(specific_results_df, outcome_name = specific_outcome, out_dir = output_dir, covariates_to_exclude = all_covariates)
        message("  Successfully generated specific coefficient plot.")
        
        file_prefix_counter <- file_prefix_counter + 1
      } else {
        message("--> Could not generate the multivariate model for '", specific_outcome, "'.")
      }
    } else {
      message("--> Skipping multivariate analysis for '", specific_outcome, "': no matched predictors available.")
    }
  }
}

# ----------------------------------------------------
# 8. GENERATE MULTIVARIATE RESULTS HEATMAP
# ----------------------------------------------------
message("\nGenerating heatmap of all multivariate model results...")
all_multivariate_results_df <- bind_rows(all_multivariate_results_list)
generate_model_heatmap(all_multivariate_results_df, output_dir, covariates_to_exclude = all_covariates)


message("\nAnalysis complete! Check the '", output_dir, "' folder for results.")

