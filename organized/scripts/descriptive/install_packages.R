# ==============================================================================
# Install Required R Packages for Chart Generation
# ==============================================================================

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Function to install packages if not already installed
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
    library(pkg, character.only = TRUE)
    cat(paste(pkg, "installed successfully!\n"))
  } else {
    cat(paste(pkg, "already installed.\n"))
  }
}

# Cairo graphics for high-quality output
install_if_missing("Cairo")

# Core visualization packages
install_if_missing("ggplot2")
install_if_missing("dplyr")
install_if_missing("readr")

# Publication-ready plots
install_if_missing("ggpubr")

# APA formatting
install_if_missing("apaTables")

# Additional useful packages
install_if_missing("corrplot")  # for correlation heatmaps
install_if_missing("RColorBrewer")  # for color palettes
install_if_missing("scales")  # for better axis formatting
install_if_missing("gridExtra")  # for combining plots

cat("\n=== All packages installed successfully! ===\n") 