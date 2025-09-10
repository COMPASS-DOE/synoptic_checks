# run-current-data.R
# Ask the user where the Dropbox folder is and run the qmd file
# If on GitHub Actions, use the testing folder
# BBL September 2025


if(Sys.getenv("CI") == "") {
  # Normal usage
    message("Please select any file in Dropbox top level")
    DROPBOX <- dirname(file.choose())
} else {
  # Running on GitHub Actions
  message("Running on GitHub Actions!")
  DROPBOX <- "./testing/"
}

library(quarto)
quarto_render("current-data.qmd", execute_params = list(DROPBOX = DROPBOX))

message("All done")
