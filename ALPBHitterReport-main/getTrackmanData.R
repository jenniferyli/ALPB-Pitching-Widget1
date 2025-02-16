# function to take in all of the Trackman data and return a workable dataframe

get_trackman_data <- function() {
  
  ### API access will go here
  
  # Local version for now. Data is in the data folder in the current working directory
  
  folder_path <- "data"
  
  # Get a list of all CSV files in the data folder
  
  csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
  
  # Read all CSV files and combine them into one dataframe
  
  combined_df <- csv_files %>%
    lapply(read.csv) %>%
    bind_rows()
  
  return(combined_df)
  
}


####### EXTRA CODE FOR TESTING #############

# Replace this with the actual path to your downloads folder
#folder_path <- "/Users/angeloferrara/Desktop/June 17th ALPB Files 2"

