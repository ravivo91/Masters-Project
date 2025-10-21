# Author: Raviv (Rewritten by Gemini)
# Creation Date: 2024-08-20 19:12:47
# Last Modified: 2025-09-07
# Description: This script analyzes psychophysical data to assess the relationship
#              between axial length, refractive error, and visual sensitivity.
#              It performs log sensitivity transformation, specific outlier removal
#              for far-field data, correlation analysis, and median-split group
#              comparisons.
#
# Key Changes in This Version:
# 1. Switched from R-squared to R-value for correlation results.
# 2. Added t-statistic and degrees of freedom to the summary for t-tests.
# 3. Removed OFF/ON Index analysis to avoid redundant results.
# 4. Added median split analysis for Near/Far ON/OFF sensitivity data.
# 5. Unified plotting function for streamlined analysis.
# 6. Creates 'With Outliers' and 'Without Outliers' subfolders.
# 7. Final summary report is sorted by P-Value.

# --- Load necessary libraries ---
library(ggplot2)
library(ggpubr)

# --- Define file paths ---
# Set the base path for the analysis folder
base_path <- "E:/iCloudDrive/Desktop/Rstudio/Psychophysics Results/Chris Analysis/"
file_path <- file.path(base_path, "analysis_psychophysical_data.csv")

# --- Create a timestamped output directory and subdirectories ---
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
output_folder <- file.path(base_path, "Log Sensitivity Folder", timestamp)
output_folder_with_outliers <- file.path(output_folder, "With Outliers")
output_folder_without_outliers <- file.path(output_folder, "Without Outliers (Far Conditions)")

if (!dir.exists(output_folder)) {
  dir.create(output_folder_with_outliers, recursive = TRUE)
  dir.create(output_folder_without_outliers, recursive = TRUE)
  cat("Created directory:", output_folder, "and subdirectories.\n")
}

# --- Load the CSV file ---
tryCatch({
  data_raw <- read.csv(file_path)
  cat("Successfully loaded data from:", file_path, "\n")
}, error = function(e) {
  stop("Error loading the CSV file. Please check the file_path: ", file_path, "\nOriginal error: ", e$message)
})

# --- Create a second dataframe with specific far-field outliers removed ---
cat("Filtering specific outliers for far_on and far_off conditions...\n")
initial_rows <- nrow(data_raw)
data_filtered_specific <- data_raw[!(data_raw$far_on %in% c(0.05, 0.20) | data_raw$far_off %in% c(0.05, 0.20)), ]
final_rows <- nrow(data_filtered_specific)
cat(paste("Removed", initial_rows - final_rows, "rows where far_on or far_off was 0.05 or 0.20.\n"))

# --- Function to process data ---
process_data <- function(df, take_log = TRUE) {
  if (take_log) {
    df$near_on <- -log10(df$near_on)
    df$near_off <- -log10(df$near_off)
    df$far_on <- -log10(df$far_on)
    df$far_off <- -log10(df$far_off)
  }
  
  df$on_off_index_near <- (df$near_on - df$near_off) / (df$near_on + df$near_off)
  df$on_off_index_far <- (df$far_on - df$far_off) / (df$far_on + df$far_off)
  
  df$relative_sensitivity_near <- df$near_on / df$near_off
  df$relative_sensitivity_far <- df$far_on / df$far_off
  
  return(df)
}

# --- Process both the full and the filtered datasets ---
cat("Processing both full and filtered datasets...\n")
data_full_processed <- process_data(data_raw, take_log = TRUE)
data_filtered_processed <- process_data(data_filtered_specific, take_log = TRUE)
cat("Data processing complete.\n")

# --- Initialize a dataframe to store all statistical results ---
results_summary <- data.frame(
  AnalysisGroup = character(), DataType = character(), Predictor = character(),
  TestType = character(), Statistic_Name = character(), Statistic_Value = numeric(),
  Degrees_of_Freedom = numeric(),
  P_Value = numeric(), N_Points = integer(), stringsAsFactors = FALSE
)

# --- Export the processed data to the main output folder ---
output_csv_full_path <- file.path(output_folder, "analysis_results_WITH_OUTLIERS.csv")
write.csv(data_full_processed, output_csv_full_path, row.names = FALSE)
cat("Exported data with outliers to:", output_csv_full_path, "\n")

output_csv_filtered_path <- file.path(output_folder, "analysis_results_WITHOUT_OUTLIERS.csv")
write.csv(data_filtered_processed, output_csv_filtered_path, row.names = FALSE)
cat("Exported data without outliers to:", output_csv_filtered_path, "\n")

# --- Function to remove outliers based on IQR ---
remove_outliers <- function(df, col) {
  if (!col %in% names(df)) {
    warning(paste("Column", col, "not found. Skipping outlier removal."))
    return(df)
  }
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  df[df[[col]] >= lower_bound & df[[col]] <= upper_bound, ]
}

# --- UNIFIED Function to run analysis, create plots, and save results ---
run_analysis_and_create_plots <- function(data, column1, column2, column_re, output_dir, data_label, plot_title_suffix = "", perform_iqr_outlier_removal = FALSE) {
  
  if (perform_iqr_outlier_removal) {
    cat("--> Performing IQR-based outlier removal for this analysis.\n")
    data <- remove_outliers(data, column1)
    data <- remove_outliers(data, column2)
    data <- remove_outliers(data, column_re)
  }
  n_points <- nrow(data)
  
  nice_labels <- list(
    "od_ax_length" = "Axial Length (mm)", "od_re" = "Refractive Error (D)",
    "far_off" = "Far OFF", "far_on" = "Far ON", "near_off" = "Near OFF", "near_on" = "Near ON",
    "relative_sensitivity_far" = "Relative Sensitivity (Far)", "relative_sensitivity_near" = "Relative Sensitivity (Near)",
    "on_off_index_far" = "ON/OFF Index (Far)", "on_off_index_near" = "ON/OFF Index (Near)"
  )
  
  y_axis_label <- if (grepl("relative_sensitivity", column2)) {
    "Relative Sensitivity (logS_ON / logS_OFF)"
  } else if (grepl("on_off_index", column2)) { "Index (from LogS)"
  } else { "Log Sensitivity (log[1/Threshold])" }
  
  shapiro_test_result <- shapiro.test(data[[column2]])
  
  # Correlation analysis: ax_length vs. sensitivity data
  cor_test1 <- cor.test(data[[column1]], data[[column2]])
  results_summary <<- rbind(results_summary, data.frame(
    AnalysisGroup = nice_labels[[column2]], DataType = data_label, Predictor = nice_labels[[column1]],
    TestType = "Pearson Correlation", Statistic_Name = "R-value", Statistic_Value = cor_test1$estimate,
    Degrees_of_Freedom = NA, P_Value = cor_test1$p.value, N_Points = n_points
  ))
  
  scatter_plot1 <- ggplot(data, aes_string(x = column1, y = column2)) +
    geom_point(color = "blue", alpha = 0.6) + geom_smooth(method = "lm", color = "red") +
    labs(title = paste(nice_labels[[column2]], "vs.", nice_labels[[column1]], plot_title_suffix), x = nice_labels[[column1]], y = y_axis_label) +
    annotate("text", x = Inf, y = Inf, label = paste0("R = ", round(cor_test1$estimate, 3), "\np = ", signif(cor_test1$p.value, 3), "\nn = ", n_points), hjust = 1.1, vjust = 1.1, size = 4)
  
  # Correlation analysis: re vs. sensitivity data
  cor_test2 <- cor.test(data[[column_re]], data[[column2]])
  results_summary <<- rbind(results_summary, data.frame(
    AnalysisGroup = nice_labels[[column2]], DataType = data_label, Predictor = nice_labels[[column_re]],
    TestType = "Pearson Correlation", Statistic_Name = "R-value", Statistic_Value = cor_test2$estimate,
    Degrees_of_Freedom = NA, P_Value = cor_test2$p.value, N_Points = n_points
  ))
  
  scatter_plot2 <- ggplot(data, aes_string(x = column_re, y = column2)) +
    geom_point(color = "green", alpha = 0.6) + geom_smooth(method = "lm", color = "orange") +
    labs(title = paste(nice_labels[[column2]], "vs.", nice_labels[[column_re]], plot_title_suffix), x = nice_labels[[column_re]], y = y_axis_label) +
    annotate("text", x = Inf, y = Inf, label = paste0("R = ", round(cor_test2$estimate, 3), "\np = ", signif(cor_test2$p.value, 3), "\nn = ", n_points), hjust = 1.1, vjust = 1.1, size = 4)
  
  # Median split (ax_length)
  median_value1 <- median(data[[column1]], na.rm = TRUE)
  data$group1 <- ifelse(data[[column1]] <= median_value1, "Low", "High")
  
  if (shapiro_test_result$p.value > 0.05) {
    test_result1 <- t.test(data[[column2]] ~ data$group1)
    test_name1 <- "T-test"; stat_name1 <- "t-statistic"; df1 <- test_result1$parameter
  } else {
    test_result1 <- wilcox.test(data[[column2]] ~ data$group1)
    test_name1 <- "Wilcoxon Test"; stat_name1 <- "W-statistic"; df1 <- NA
  }
  
  results_summary <<- rbind(results_summary, data.frame(
    AnalysisGroup = nice_labels[[column2]], DataType = data_label, Predictor = paste("Median Split by", nice_labels[[column1]]),
    TestType = test_name1, Statistic_Name = stat_name1, Statistic_Value = test_result1$statistic,
    Degrees_of_Freedom = df1, P_Value = test_result1$p.value, N_Points = n_points
  ))
  
  boxplot1 <- ggplot(data, aes(x = group1, y = .data[[column2]])) +
    geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2, alpha = 0.6) +
    labs(title = paste(nice_labels[[column2]], "by Axial Length Group", plot_title_suffix), x = "Axial Length Group", y = y_axis_label) +
    stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "red") +
    annotate("text", x = 1.5, y = max(data[[column2]], na.rm = TRUE) * 0.9, label = paste0("p = ", signif(test_result1$p.value, 3)), size = 4)
  
  # Median split (re)
  median_value2 <- median(data[[column_re]], na.rm = TRUE)
  data$group2 <- ifelse(data[[column_re]] <= median_value2, "Low", "High")
  
  if (shapiro_test_result$p.value > 0.05) {
    test_result2 <- t.test(data[[column2]] ~ data$group2)
    test_name2 <- "T-test"; stat_name2 <- "t-statistic"; df2 <- test_result2$parameter
  } else {
    test_result2 <- wilcox.test(data[[column2]] ~ data$group2)
    test_name2 <- "Wilcoxon Test"; stat_name2 <- "W-statistic"; df2 <- NA
  }
  
  results_summary <<- rbind(results_summary, data.frame(
    AnalysisGroup = nice_labels[[column2]], DataType = data_label, Predictor = paste("Median Split by", nice_labels[[column_re]]),
    TestType = test_name2, Statistic_Name = stat_name2, Statistic_Value = test_result2$statistic,
    Degrees_of_Freedom = df2, P_Value = test_result2$p.value, N_Points = n_points
  ))
  
  boxplot2 <- ggplot(data, aes(x = group2, y = .data[[column2]])) +
    geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2, alpha = 0.6) +
    labs(title = paste(nice_labels[[column2]], "by Refractive Error Group", plot_title_suffix), x = "Refractive Error Group", y = y_axis_label) +
    stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "red") +
    annotate("text", x = 1.5, y = max(data[[column2]], na.rm = TRUE) * 0.9, label = paste0("p = ", signif(test_result2$p.value, 3)), size = 4)
  
  # Add standardized Y-axis limits for sensitivity plots
  if (column2 %in% c("far_on", "far_off")) {
    scatter_plot1 <- scatter_plot1 + coord_cartesian(ylim = c(0.80, 1.30))
    scatter_plot2 <- scatter_plot2 + coord_cartesian(ylim = c(0.80, 1.30))
  } else if (column2 %in% c("near_on", "near_off")) {
    scatter_plot1 <- scatter_plot1 + coord_cartesian(ylim = c(1.45, 2.00))
    scatter_plot2 <- scatter_plot2 + coord_cartesian(ylim = c(1.45, 2.00))
  }
  
  combined_plot <- ggarrange(scatter_plot1, scatter_plot2, boxplot1, boxplot2, ncol = 2, nrow = 2)
  output_filename <- file.path(output_dir, paste0("analysis_plots_", column2, ".png"))
  ggsave(output_filename, plot = combined_plot, width = 12, height = 10, dpi = 300)
  cat("Saved combined plot to:", output_filename, "\n")
}

# --- Function to create a single plot for Axial Length vs. Refractive Error ---
create_ax_vs_re_plot <- function(data, output_dir) {
  data_no_outliers <- remove_outliers(data, "od_ax_length")
  data_no_outliers <- remove_outliers(data_no_outliers, "od_re")
  n_points <- nrow(data_no_outliers)
  cor_test <- cor.test(data_no_outliers$od_ax_length, data_no_outliers$od_re)
  
  results_summary <<- rbind(results_summary, data.frame(
    AnalysisGroup = "Refractive Error (D)", DataType = "With Outliers", Predictor = "Axial Length (mm)",
    TestType = "Pearson Correlation", Statistic_Name = "R-value", Statistic_Value = cor_test$estimate,
    Degrees_of_Freedom = NA, P_Value = cor_test$p.value, N_Points = n_points
  ))
  
  ax_re_plot <- ggplot(data_no_outliers, aes(x = od_ax_length, y = od_re)) +
    geom_point(color = "purple", alpha = 0.6) + geom_smooth(method = "lm", color = "black") +
    labs(title = "Refractive Error vs. Axial Length", x = "Axial Length (mm)", y = "Refractive Error (D)") +
    annotate("text", x = Inf, y = Inf, label = paste0("R = ", round(cor_test$estimate, 3), "\np = ", signif(cor_test$p.value, 3), "\nn = ", n_points), hjust = 1.1, vjust = 1.1, size = 4) +
    theme_minimal()
  
  output_filename <- file.path(output_dir, "axial_length_vs_refractive_error.png")
  ggsave(output_filename, plot = ax_re_plot, width = 8, height = 6, dpi = 300)
  cat("Saved plot to:", output_filename, "\n")
}

# --- Define groups for analysis ---
groups_to_process <- c("relative_sensitivity_far", "relative_sensitivity_near",
                       "on_off_index_far", "on_off_index_near")

groups_for_sensitivity <- c("near_on", "near_off", "far_on", "far_off")

# --- Loop 1: Analysis for derived metrics (WITH IQR outlier removal) ---
for (group in groups_to_process) {
  cat("\n\n--- Processing Full Analysis for DERIVED group:", group, "---\n")
  
  run_analysis_and_create_plots(data_full_processed, "od_ax_length", group, "od_re",
                                output_dir = output_folder_with_outliers, data_label = "With Outliers",
                                perform_iqr_outlier_removal = TRUE
  )
  
  if (grepl("_far$", group)) {
    cat("--> Also processing", group, "without specific far-field outliers.\n")
    run_analysis_and_create_plots(data_filtered_processed, "od_ax_length", group, "od_re",
                                  output_dir = output_folder_without_outliers, data_label = "Without Outliers",
                                  plot_title_suffix = " (Far Outliers Removed)", perform_iqr_outlier_removal = TRUE
    )
  }
}

# --- Loop 2: Full analysis for SENSITIVITY data (WITHOUT IQR outlier removal) ---
for (group in groups_for_sensitivity) {
  cat("\n\n--- Processing Full Analysis for SENSITIVITY group:", group, "---\n")
  
  run_analysis_and_create_plots(data_full_processed, "od_ax_length", group, "od_re",
                                output_dir = output_folder_with_outliers, data_label = "With Outliers",
                                perform_iqr_outlier_removal = FALSE
  )
  
  if (grepl("far_", group)) {
    cat("--> Also processing", group, "without specific far-field outliers.\n")
    run_analysis_and_create_plots(data_filtered_processed, "od_ax_length", group, "od_re",
                                  output_dir = output_folder_without_outliers, data_label = "Without Outliers",
                                  plot_title_suffix = " (Far Outliers Removed)", perform_iqr_outlier_removal = FALSE
    )
  }
}

# --- Final Plot: Axial Length vs. Refractive Error (saved to main folder) ---
create_ax_vs_re_plot(data_full_processed, output_folder)

# --- Sort and Export the summary of all statistical results ---
cat("\nSorting statistical results by P-value...\n")
results_summary_sorted <- results_summary[order(results_summary$P_Value), ]

output_csv_summary_path <- file.path(output_folder, "Psychophysics statistical results summary.csv")
write.csv(results_summary_sorted, output_csv_summary_path, row.names = FALSE)
cat("Exported sorted summary of all statistical results to:", output_csv_summary_path, "\n")

cat("\n\n--- Analysis complete. All files saved to:", output_folder, "---\n")

