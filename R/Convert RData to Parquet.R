## ----------------------------------------------------------------
## Convert RData to parquet
##
##      Authors: Shelby Golden, MS from Yale's YSPH DSDE group
## Date Created: August 11th, 2025
## 
## Description: One of the original files from the repository was too
##              large to git track and distribute on GitHub. This
##              script converts this file to parquet. This code was
##              written with the support of Yale's AI Clarity.

## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

suppressPackageStartupMessages({
  library("arrow")
})




## ----------------------------------------------------------------
## FUNCTION

# Function to convert .RData file objects to Parquet
convert_rdata_to_parquet <- function(rdata_file_path, output_directory) {
  # Load the .RData file
  load(rdata_file_path)
  
  # Get the names of the objects loaded from the .RData file
  objects <- ls()
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  
  for (object_name in objects) {
    # Retrieve the object
    data <- get(object_name)
    
    # Check if object is a data frame
    if (is.data.frame(data)) {
      # Define Parquet file path
      parquet_file_path <- file.path(output_directory, paste0(object_name, ".parquet"))
      
      # Write the data frame to a Parquet file
      write_parquet(data, parquet_file_path)
      
      # Notify user of success
      cat("Successfully converted", object_name, "to", parquet_file_path, "\n")
    } else {
      cat(object_name, "is not a data frame and was not converted.\n")
    }
    
    # Remove the object from the environment
    rm(list = object_name)
  }
}




## ----------------------------------------------------------------
## FUNCTION

rdata_file <- file.path(getwd(), "play2_data/opioid_data.cached.RData")
output_directory <- file.path(getwd(), "play2_data")

convert_rdata_to_parquet(rdata_file, output_directory)

