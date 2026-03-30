# run-current-data.R
# Ask the user where the Dropbox folder is and run the qmd file
# If on GitHub Actions, use the testing folder
# BBL September 2025

library(quarto)

if(Sys.getenv("CI") == "") {
  # Normal usage
    message("Please select any file in Dropbox top level")
    DROPBOX <- dirname(file.choose())
    SITES <- readline("Sites to plot: ")
} else {
  # Running on GitHub Actions
  message("Running on GitHub Actions!")
  DROPBOX <- "./testing/"
  SITES <- "TESTING"
}

quarto_render("current-data-zr.qmd", 
              execute_params = list(DROPBOX = DROPBOX, SITES = SITES))
                                                        
message("All done")

#maybe SW will add: 
#current-data-CB/LE-20250904 
#if running CB put it in CB-checks 
#if running LE put it in Le-checks
#quarto_render probably has 
