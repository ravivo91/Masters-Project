library(ggplot2)
library(readxl)
library(gridExtra)

# Set the working directory (adjust this path to your local machine)
# It's often better to use R projects to manage working directories automatically.
# setwd("E:/iCloudDrive/Desktop/Rstudio/ERG_VEP")

# --- Directory Setup ---
# Define the main parent folder for all output using the full path
parentFolder <- "E:/iCloudDrive/Desktop/Rstudio/ERG_VEP/Flicker Correlation"
if (!dir.exists(parentFolder)) {
  dir.create(parentFolder, recursive = TRUE) # Use recursive = TRUE to create parent folders if they don't exist
}

# Create a timestamp for a unique subfolder for this specific run
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
resultsSubFolder <- file.path(parentFolder, paste0("Results_", timestamp))
if (!dir.exists(resultsSubFolder)) {
  dir.create(resultsSubFolder)
}
# --- END Directory Setup ---


# --- DATA LOADING AND PREPARATION (No changes from your original) ---
# Read the Excel file
# Ensure this file is in your working directory or provide the full path
filePath <- "AllSubjectsResultsFlicker.xlsx"
allData  <- read_excel(filePath)

# Create ON/OFF ratio variables
allData$`ON/OFF Ratio ERG AMP 10 HZ`   <- allData$ON_Flicker_ERG_Amp_10Hz   / allData$OFF_Flicker_ERG_Amp_10Hz
allData$`ON/OFF Ratio ERG AMP 20 HZ`   <- allData$ON_Flicker_ERG_Amp_20Hz   / allData$OFF_Flicker_ERG_Amp_20Hz
allData$`ON/OFF Ratio ERG Phase 10 HZ` <- allData$ON_Flicker_ERG_Phase_10Hz / allData$OFF_Flicker_ERG_Phase_10Hz
allData$`ON/OFF Ratio ERG Phase 20 HZ` <- allData$ON_Flicker_ERG_Phase_20Hz / allData$OFF_Flicker_ERG_Phase_20Hz

allData$`ON/OFF Ratio VEP AMP 10 HZ`   <- allData$ON_Flicker_VEP_Amp_10Hz   / allData$OFF_Flicker_VEP_Amp_10Hz
allData$`ON/OFF Ratio VEP AMP 20 HZ`   <- allData$ON_Flicker_VEP_Amp_20Hz   / allData$OFF_Flicker_VEP_Amp_20Hz
allData$`ON/OFF Ratio VEP Phase 10 HZ` <- allData$ON_Flicker_VEP_Phase_10Hz / allData$OFF_Flicker_VEP_Phase_10Hz
allData$`ON/OFF Ratio VEP Phase 20 HZ` <- allData$ON_Flicker_VEP_Phase_20Hz / allData$OFF_Flicker_VEP_Phase_20Hz

# Define predictor variables
predictors <- list(
  "Axial Length (mm)" = "Axial_Length_OD",
  "Refractive Error (D)" = "re"  
)

# Determine which columns to correlate (drop ID and predictor columns)
excludeCols   <- c("SubjectID", names(allData)[names(allData) %in% unlist(predictors)])
testVarNames  <- setdiff(names(allData), excludeCols)
# --- END DATA LOADING ---


# --- Results Table Preparation (No changes from your original) ---
# Prepare results container with the desired column structure
results <- data.frame(
  Analysis_Group   = character(0),
  Data_Type        = character(0),
  Predictor        = character(0),
  Response_Variable= character(0), # Keeping this for clarity in the raw data
  Test_Type        = character(0),
  Statistic_Name   = character(0),
  Statistic_Value  = numeric(0),
  P_Value          = numeric(0),
  R_Value          = numeric(0),
  N                = integer(0),
  stringsAsFactors = FALSE
)
# --- END Results Table Preparation ---


# --- Main Analysis Loop ---
# Loop over each predictor variable
for (predictor_name in names(predictors)) {
  
  # --- MODIFIED SECTION: Create a subfolder for the current predictor's graphs ---
  # Create a "safe" name for the folder by removing special characters
  safe_predictor_name <- gsub("[^A-Za-z0-9]", "_", predictor_name)
  predictorGraphFolder <- file.path(resultsSubFolder, safe_predictor_name)
  if (!dir.exists(predictorGraphFolder)) {
    dir.create(predictorGraphFolder)
  }
  # --- END MODIFIED SECTION ---
  
  predictor_col <- predictors[[predictor_name]]
  x <- allData[[predictor_col]]
  
  # Loop over each response variable
  for (var in testVarNames) {
    y <- allData[[var]]
    # Ensure both predictor and response data are available
    ok <- !is.na(x) & !is.na(y)
    xClean <- x[ok]
    yClean <- y[ok]
    
    # Initialize variables to store results for this iteration
    t_stat <- NA
    p_val <- NA
    r_val <- NA
    n_val <- length(xClean)
    
    if (n_val > 2) {
      ct     <- cor.test(xClean, yClean, method = "pearson")
      t_stat <- ct$statistic
      p_val  <- ct$p.value
      r_val  <- ct$estimate
    }
    
    # Determine Data Type (ERG or VEP) from variable name
    dataType <- "Other" # Default value
    if (grepl("ERG", var, ignore.case = TRUE)) {
      dataType <- "ERG"
    } else if (grepl("VEP", var, ignore.case = TRUE)) {
      dataType <- "VEP"
    }
    
    # Append results in the new, more descriptive format
    results <- rbind(
      results,
      data.frame(
        Analysis_Group   = "Flicker Metrics Correlation",
        Data_Type        = dataType,
        Predictor        = predictor_name,
        Response_Variable= var,
        Test_Type        = "Pearson Correlation",
        Statistic_Name   = "t-statistic",
        Statistic_Value  = t_stat,
        P_Value          = p_val,
        R_Value          = r_val,
        N                = n_val,
        stringsAsFactors = FALSE
      )
    )
    
    # --- PLOTTING ---
    if (n_val > 2) {
      label <- gsub("_", " ", var)
      df_plot <- data.frame(Predictor = xClean, Measure = yClean)
      
      y_limits <- NULL
      y_label <- label
      if (grepl("ON/OFF Ratio", var, ignore.case = TRUE)) {
        y_limits <- c(0, 2.5)
      } else if (grepl("VEP_Amp_Ratio_10_20Hz", var, ignore.case = TRUE)) {
        y_limits <- c(0, 18)
      } else if (grepl("Phase", var, ignore.case = TRUE)) {
        y_limits <- c(-pi, pi)
        y_label <- paste0(label, " (radians)")
      } else if (grepl("SNR", var, ignore.case = TRUE)) {
        y_limits <- c(0, 130)
      } else if (grepl("Amp", var, ignore.case = TRUE)) {
        y_limits <- c(0, 12000)
      }
      
      p_text <- sprintf("p = %.4f", p_val)
      r_text <- sprintf("r = %.3f", r_val)
      n_text <- sprintf("n = %d", n_val)
      
      stats_caption <- paste(p_text, r_text, n_text, sep = "   ")
      
      p_main <- ggplot(df_plot, aes(x = Predictor, y = Measure)) +
        geom_point(color = "black") +
        geom_smooth(method = "lm", color = "blue", fill = "gray", se = TRUE) +
        labs(
          x       = predictor_name,
          y       = y_label,
          title   = paste(predictor_name, "vs", label),
          caption = stats_caption
        ) +
        theme(
          plot.margin = unit(c(1, 1, 2, 1), "lines"),
          plot.caption = element_text(hjust = 0.5, size = 10)
        )
      
      if (grepl("SNR", var, ignore.case = TRUE)) {
        p_main <- p_main + geom_hline(yintercept = 2, linetype = "dashed", color = "red")
      }
      
      if (!is.null(y_limits)) {
        p_main <- p_main + coord_cartesian(ylim = y_limits)
      }
      
      safeName <- paste0(gsub("[^A-Za-z0-9]", "_", predictor_col), "_vs_", gsub("[^A-Za-z0-9]", "_", var))
      
      # --- MODIFIED SECTION: Save plot to the predictor-specific folder ---
      ggsave(
        filename = file.path(predictorGraphFolder, paste0(safeName, ".png")),
        plot     = p_main,
        width    = 6.5, height = 4.5, units = "in"
      )
      # --- END MODIFIED SECTION ---
    }
  }
}
# --- END Main Analysis Loop ---


# --- Final Output (No changes from your original) ---
# Create a new data frame for export with the exact column names requested
results_for_export <- results
# Rename columns to match the desired output format with spaces
names(results_for_export) <- c(
  "Analysis Group",
  "Data Type",
  "Predictor",
  "Response Variable", 
  "Test Type",
  "Statistic Name",
  "Statistic Value",
  "P value",
  "R value",
  "N (number of points)"
)

# Write the results to a CSV file inside the main timestamped subfolder
write.csv(
  results_for_export,
  file = file.path(resultsSubFolder, "CorrelationResults.csv"),
  row.names = FALSE
)

# --- END Final Output ---

