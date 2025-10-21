library(ggplot2)
library(readxl)

# --- Directory Setup ---
# Define the main parent folder for the Flash correlation analysis
parentFolder <- "E:/iCloudDrive/Desktop/Rstudio/ERG_VEP/Flash Correlation"
if (!dir.exists(parentFolder)) {
  dir.create(parentFolder, recursive = TRUE)
}

# Create a timestamp for a unique subfolder for this specific run
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
resultsSubFolder <- file.path(parentFolder, paste0("Results_", timestamp))
if (!dir.exists(resultsSubFolder)) {
  dir.create(resultsSubFolder)
}

# --- Create a dedicated folder for distribution plots ---
distributionPlotsFolder <- file.path(resultsSubFolder, "Data_Distributions")
if (!dir.exists(distributionPlotsFolder)) {
  dir.create(distributionPlotsFolder)
}
# --- END Directory Setup ---

# --- DATA LOADING AND PREPARATION ---
# Read the Flash Excel file
filePath <- "E:/iCloudDrive/Desktop/Rstudio/ERG_VEP/AllSubjectsResultsFlash.xlsx"
allData  <- readxl::read_excel(filePath)

# Define predictor variables to loop through
predictors <- list(
  "Axial Length (mm)" = "Axial_Length_OD",
  "Refractive Error (D)" = "re"
)

# Determine which columns to correlate (response variables)
excludeCols <- c("SubjectID", names(allData)[names(allData) %in% unlist(predictors)])
testVarNames  <- setdiff(names(allData), excludeCols)


# --- MODIFIED: Set up results storage for a single, combined table ---
all_results <- data.frame(
  Analysis_Group        = character(0),
  Data_Type             = character(0),
  Predictor             = character(0),
  Response_Variable     = character(0),
  Test_Type             = character(0),
  Statistic_Name        = character(0),
  Statistic_Value       = numeric(0),
  P_Value               = numeric(0),
  Correlation_Coefficient = numeric(0),
  N                     = integer(0),
  stringsAsFactors      = FALSE
)
# --- END results storage setup ---

# --- Generate and Save Distribution Plots ---
for (v in testVarNames) {
  # Create a temporary data frame for plotting, removing NA values
  df_plot <- data.frame(value = allData[[v]])
  df_plot <- na.omit(df_plot)
  
  # Ensure there is data to plot
  if (nrow(df_plot) < 3) next
  
  # Create a safe filename for the variable
  safeName <- gsub("[^A-Za-z0-9]", "_", v)
  
  # 1. Create the Q-Q plot
  p_qq <- ggplot(df_plot, aes(sample = value)) +
    geom_qq() +
    geom_qq_line(color = "red") +
    labs(
      title = paste("Q-Q Plot for", v),
      y = "Sample Quantiles",
      x = "Theoretical Quantiles"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Save the Q-Q plot
  ggsave(
    filename = file.path(distributionPlotsFolder, paste0(safeName, "_qqplot.png")),
    plot = p_qq,
    width = 5,
    height = 5,
    dpi = 150
  )
  
  # 2. Create the Histogram with a Density Plot
  p_hist <- ggplot(df_plot, aes(x = value)) +
    geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
    geom_density(color = "red", size = 1) +
    labs(
      title = paste("Histogram for", v),
      x = v,
      y = "Density"
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Save the Histogram
  ggsave(
    filename = file.path(distributionPlotsFolder, paste0(safeName, "_histogram.png")),
    plot = p_hist,
    width = 6,
    height = 4,
    dpi = 150
  )
}
# --- END Distribution Plots ---


# --- Main Analysis Loop ---
# Loop over each predictor variable
for (predictor_name in names(predictors)) {
  predictor_col <- predictors[[predictor_name]]
  
  # Create a subfolder for the current predictor's graphs
  safe_predictor_name <- gsub("[^A-Za-z0-9]", "_", predictor_name)
  predictorGraphFolder <- file.path(resultsSubFolder, safe_predictor_name)
  if (!dir.exists(predictorGraphFolder)) {
    dir.create(predictorGraphFolder)
  }
  
  x <- allData[[predictor_col]]
  
  # Loop over each response variable
  for (var in testVarNames) {
    y <- allData[[var]]
    # Ensure both predictor and response data are available and finite
    ok <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
    xClean <- x[ok]
    yClean <- y[ok]
    
    n_val <- length(xClean)
    
    # Determine Data Type (ERG or VEP) from variable name
    dataType <- "Other"
    if (grepl("ERG", var, ignore.case = TRUE)) {
      dataType <- "ERG"
    } else if (grepl("VEP", var, ignore.case = TRUE)) {
      dataType <- "VEP"
    }
    
    if (n_val > 2) {
      # --- MODIFIED: Run both Pearson and Spearman tests ---
      ct_pearson <- cor.test(xClean, yClean, method = "pearson")
      ct_spearman <- cor.test(xClean, yClean, method = "spearman", exact = FALSE)
      
      # Append Pearson results to the combined table
      all_results <- rbind(
        all_results,
        data.frame(
          Analysis_Group   = "Flash Metrics Correlation",
          Data_Type        = dataType,
          Predictor        = predictor_name,
          Response_Variable= var,
          Test_Type        = "Pearson Correlation",
          Statistic_Name   = "t-statistic",
          Statistic_Value  = ct_pearson$statistic,
          P_Value          = ct_pearson$p.value,
          Correlation_Coefficient = ct_pearson$estimate,
          N                = n_val,
          stringsAsFactors = FALSE
        )
      )
      
      # Append Spearman results to the combined table
      all_results <- rbind(
        all_results,
        data.frame(
          Analysis_Group   = "Flash Metrics Correlation",
          Data_Type        = dataType,
          Predictor        = predictor_name,
          Response_Variable= var,
          Test_Type        = "Spearman Correlation",
          Statistic_Name   = "S-statistic",
          Statistic_Value  = ct_spearman$statistic,
          P_Value          = ct_spearman$p.value,
          Correlation_Coefficient = ct_spearman$estimate,
          N                = n_val,
          stringsAsFactors = FALSE
        )
      )
      
      # --- MODIFIED: Create and save plot with both correlation stats ---
      label <- gsub("_", " ", var)
      df_plot <- data.frame(Predictor = xClean, Measure = yClean)
      
      # Format text for plot caption
      pearson_line <- sprintf("Pearson: r = %.3f, p = %.4f", ct_pearson$estimate, ct_pearson$p.value)
      spearman_line <- sprintf("Spearman: rho = %.3f, p = %.4f", ct_spearman$estimate, ct_spearman$p.value)
      n_line <- sprintf("n = %d", n_val)
      stats_caption <- paste(pearson_line, spearman_line, n_line, sep = "\n")
      
      # Plot with the current predictor
      p_main <- ggplot(df_plot, aes(x = Predictor, y = Measure)) +
        geom_point(color = "black") +
        geom_smooth(method = "lm", color = "blue", fill = "lightblue", se = TRUE) +
        labs(
          x       = predictor_name,
          y       = label,
          title   = paste(predictor_name, "vs", label),
          caption = stats_caption
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.caption = element_text(hjust = 0, size = 10) # Left-align caption
        )
      
      # Create a safe filename and save the plot in the predictor's folder
      safeName <- gsub("[^A-Za-z0-9]", "_", var)
      ggsave(
        filename = file.path(predictorGraphFolder, paste0(safeName, ".png")),
        plot     = p_main,
        width    = 6.5, height = 4.5, units = "in"
      )
    }
  }
}
# --- END Main Analysis Loop ---

# --- MODIFIED: Final Output for the single, combined file ---
# Prepare combined results for export
results_for_export <- all_results
names(results_for_export) <- c(
  "Analysis Group", "Data Type", "Predictor", "Response Variable", "Test Type",
  "Statistic Name", "Statistic Value", "P value", "Correlation Coefficient", "N (number of points)"
)
# Write the combined results to a single CSV file
write.csv(
  results_for_export,
  file = file.path(resultsSubFolder, "Combined_CorrelationResults.csv"),
  row.names = FALSE
)
# --- END Final Output ---

