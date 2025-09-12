# run-current-data.R
# Ask the user where the Dropbox folder is and run the qmd file
# If on GitHub Actions, use the testing folder
# BBL September 2025

#Roberta Peixoto September 2025 adapting Ben's code for windows and folders in different directories

choose_dir <- function(prompt_msg = "Select folder") {
  if (.Platform$OS.type == "windows") {
    return(choose.dir(caption = prompt_msg))
  } else {
    return(dirname(file.choose()))
  }
}

if(Sys.getenv("CI") == "") {
  message("Please select the Dropbox folder")
  DROPBOX <- choose_dir("Select the Dropbox folder")
  message("Selected Dropbox folder: ", DROPBOX)
  
  message("Please select the folder containing current-data.qmd")
  QMD_FOLDER <- choose_dir("Please select the folder containing current-data.qmd")
  message("Selected current-data.qmd folder: ", QMD_FOLDER)
  
  QMD_FILE <- file.path(QMD_FOLDER, "current-data.qmd")
  
} else {
  message("Running on GitHub Actions!")
  DROPBOX <- "./testing/"
  QMD_FILE <- "./current-data.qmd"
}

library(quarto)
quarto_render(QMD_FILE, execute_params = list(DROPBOX = DROPBOX))

message("All done")
