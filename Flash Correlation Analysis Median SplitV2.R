library(ggplot2)
library(readxl)

# --- Directory Setup ---
# Define the main parent folder for the Flash analysis
parentFolder <- "E:/iCloudDrive/Desktop/Rstudio/ERG_VEP/Flash Median Split Analysis"
if (!dir.exists(parentFolder)) {
  dir.create(parentFolder, recursive = TRUE)
}

# Create a timestamp for a unique subfolder for this specific run
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
resultsSubFolder <- file.path(parentFolder, paste0("Results_", timestamp))
if (!dir.exists(resultsSubFolder)) {
  dir.create(resultsSubFolder)
}

# Create a dedicated folder for distribution plots
distributionPlotsFolder <- file.path(resultsSubFolder, "Data_Distributions")
if (!dir.exists(distributionPlotsFolder)) {
  dir.create(distributionPlotsFolder)
}
# --- END Directory Setup ---

# --- DATA LOADING AND PREPARATION ---
# Read the Flash Excel file
filePath <- "AllSubjectsResultsFlash.xlsx"
allData  <- readxl::read_excel(filePath)

# Define grouping variables to loop through
grouping_vars <- list(
  "Axial Length (mm)" = "Axial_Length_OD",
  "Refractive Error (D)" = "re"
)

# Determine which columns to test (response variables)
# Exclude SubjectID and the grouping variables themselves
excludeCols <- c("SubjectID", names(allData)[names(allData) %in% unlist(grouping_vars)])
# Get all other column names to loop through as test variables
testVars <- setdiff(names(allData), excludeCols)


# --- Set up results storage with the detailed format ---
results <- data.frame(
  Analysis_Group   = character(0),
  Data_Type        = character(0),
  Predictor        = character(0),
  Response_Variable= character(0),
  Test_Type        = character(0),
  Statistic_Name   = character(0),
  Statistic_Value  = numeric(0),
  P_Value          = numeric(0),
  N                = integer(0),
  stringsAsFactors = FALSE
)
# --- END results storage setup ---

# --- Generate and Save Distribution Plots (Q-Q Plots) ---
for (v in testVars) {
  # Create a temporary data frame for plotting, removing NA values
  df_plot <- data.frame(value = allData[[v]])
  df_plot <- na.omit(df_plot)
  
  # Ensure there is data to plot
  if (nrow(df_plot) < 3) next
  
  # Create the Q-Q plot
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
  
  # Create a safe filename and save the plot
  safeName <- gsub("[^A-Za-z0-9]", "_", v)
  ggsave(
    filename = file.path(distributionPlotsFolder, paste0(safeName, "_qqplot.png")),
    plot = p_qq,
    width = 5,
    height = 5,
    dpi = 150
  )
}
# --- END Distribution Plots ---


# --- Main Analysis Loop ---
# Loop over each grouping variable (predictor)
for (group_name in names(grouping_vars)) {
  group_col <- grouping_vars[[group_name]]
  
  # Create a subfolder for the current predictor's graphs
  safe_predictor_name <- gsub("[^A-Za-z0-9]", "_", group_name)
  predictorGraphFolder <- file.path(resultsSubFolder, safe_predictor_name)
  if (!dir.exists(predictorGraphFolder)) {
    dir.create(predictorGraphFolder)
  }
  
  # Perform the median split for the current grouping variable
  data_copy <- allData # Use a copy to avoid altering the original data frame
  median_val <- median(data_copy[[group_col]], na.rm = TRUE)
  data_copy$Group <- ifelse(data_copy[[group_col]] <= median_val, "Low", "High")
  data_copy$Group <- factor(data_copy$Group, levels = c("Low", "High")) # Ensure order
  
  # Loop through every other numeric column (response variable)
  for (v in testVars) {
    vec <- data_copy[[v]]
    df  <- data_copy[!is.na(vec) & !is.na(data_copy[[group_col]]), ]
    
    if (length(unique(df$Group)) < 2 || nrow(df) < 3) next
    
    # Simple outlier removal
    remove_outliers <- function(x) {
      Q1 <- quantile(x, .25, na.rm=TRUE)
      Q3 <- quantile(x, .75, na.rm=TRUE)
      IQR <- Q3 - Q1
      x[x >= (Q1 - 1.5*IQR) & x <= (Q3 + 1.5*IQR)]
    }
    df <- df[df[[v]] %in% remove_outliers(df[[v]]), ]
    
    # Check again after outlier removal to ensure there are still two groups
    if (length(unique(df$Group)) < 2 || nrow(df) < 3 || min(table(df$Group)) < 2) next
    
    # t-test & Wilcoxon
    t_res   <- t.test(df[[v]] ~ df$Group)
    wil_res <- wilcox.test(df[[v]] ~ df$Group)
    
    # Determine Data Type (ERG or VEP) from variable name
    dataType <- "Other"
    if (grepl("ERG", v, ignore.case = TRUE)) {
      dataType <- "ERG"
    } else if (grepl("VEP", v, ignore.case = TRUE)) {
      dataType <- "VEP"
    }
    
    # Store results for t-test
    results <- rbind(
      results,
      data.frame(
        Analysis_Group   = "Median Split Analysis",
        Data_Type        = dataType,
        Predictor        = group_name,
        Response_Variable= v,
        Test_Type        = "Student's t-test",
        Statistic_Name   = "t-statistic",
        Statistic_Value  = t_res$statistic,
        P_Value          = t_res$p.value,
        N                = nrow(df),
        stringsAsFactors = FALSE
      )
    )
    
    # Store results for Wilcoxon test
    results <- rbind(
      results,
      data.frame(
        Analysis_Group   = "Median Split Analysis",
        Data_Type        = dataType,
        Predictor        = group_name,
        Response_Variable= v,
        Test_Type        = "Wilcoxon rank sum test",
        Statistic_Name   = "W-statistic",
        Statistic_Value  = wil_res$statistic,
        P_Value          = wil_res$p.value,
        N                = nrow(df),
        stringsAsFactors = FALSE
      )
    )
    
    # Boxplot
    p <- ggplot(df, aes(x = Group, y = .data[[v]])) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.6) +
      labs(
        title = paste("Median split on", group_name, ":", v),
        x     = paste("Group (<= vs >", round(median_val, 2), ")"),
        y     = v
      ) +
      annotate("text", x = 1.5, y = max(df[[v]], na.rm = TRUE),
               label = paste0("t-p=", signif(t_res$p.value, 3),
                              "\nW-p=", signif(wil_res$p.value, 3)),
               vjust = 1.2, size = 3.5, color = "blue") +
      theme(
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        plot.margin = margin(10, 12, 10, 12)
      )
    
    # Save each plot to the correct predictor-specific folder
    safeName <- gsub("[^A-Za-z0-9]", "_", v)
    ggsave(
      filename = file.path(predictorGraphFolder, paste0(safeName, "_medsplit.png")),
      plot     = p,
      width = 7,
      height = 4.5,
      dpi = 300
    )
  }
}
# --- END Main Analysis Loop ---

# --- Final Output ---
# Create a new data frame for export with user-friendly column names
results_for_export <- results
names(results_for_export) <- c(
  "Analysis Group",
  "Data Type",
  "Predictor",
  "Response Variable",
  "Test Type",
  "Statistic Name",
  "Statistic Value",
  "P value",
  "N (number of points)"
)

# Write out the median-split summary to the main timestamped folder
write.csv(
  results_for_export,
  file = file.path(resultsSubFolder, "MedianSplit_Results.csv"),
  row.names = FALSE
)
# --- END Final Output ---

