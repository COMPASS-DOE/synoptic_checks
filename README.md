# synoptic_checks

Code for automating sensor data checks

Note there are two ways to run this code:

1. Render the Quarto `current-data.qmd` file.

However, the Quarto file assumes that the Dropbox is located at
`~/Library/CloudStorage/Dropbox` (which is true for a Mac, but
not on other platforms). So you can also

2. Source the `run-current-data.R` file. This will let you
specify/find the Dropbox folder and _then_ run the Quarto file.
Of course, you can also

3. Edit the Quarto file for your particular path/environment.
