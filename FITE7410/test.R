# Set up custom library path to avoid permission issues
.libPaths(Sys.getenv("R_LIBS_USER"))
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)

# Install packages to the user library
if (!require("dplyr", character.only = TRUE, quietly = TRUE)) {
  cat("Installing dplyr to user library...\n")
  install.packages("dplyr", lib = Sys.getenv("R_LIBS_USER"), 
                   repos = "https://cran.r-project.org")
}

# Load library from the correct location
library(dplyr, lib.loc = Sys.getenv("R_LIBS_USER"))
cat("✓ dplyr loaded successfully!\n")