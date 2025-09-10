# run-current-data.R
# Ask the user where the Dropbox folder is and run the qmd file
# If on GitHub Actions, use the testing folder
# BBL September 2025


if(Sys.getenv("CI") == "") {
  # Normal usage
  DROPBOX <- "~/Dropbox"
  if(file.exists(DROPBOX)) {
    message("Found Dropbox")
  } else {
    message("Please select a file in Dropbox")
    DROPBOX <- dirname(file.choose())
  }
} else {
  # Running on GitHub Actions
  message("Running on GitHub Actions!")
  DROPBOX <- "./testing/"
}

library(quarto)
quarto_render("current-data.qmd", execute_params = list(DROPBOX = DROPBOX))

message("All done")
