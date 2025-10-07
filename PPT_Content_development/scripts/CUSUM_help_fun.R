Title: CUSUM Control Chart for Upward Shift Detection in R
# Title: CUSUM Control Chart Implementation in R
# Author: LIN PPT Team (Malawi)/AI Assistant (Google AI Studio)
# Date: 30 September, 2025
# Description:
#   This script provides an R implementation of a CUSUM (Cumulative Sum)
#   control chart function to detect shifts in the process mean.
#   It includes a function 'cusum_chart' and an example of its usage
#   and plotting.

#' CUSUM Control Chart Function for Upward Shifts
#'
#' Implements a *one-sided* CUSUM control chart to detect *upward shifts*
#' in the mean of a process. The CUSUM sum is reset to 0 after
#' a signal is detected.
#'
#' @param x A numeric vector of observed data.
#' @param climit The control limit (h) for the CUSUM chart. A signal is
#'   generated when the CUSUM value exceeds this limit. Default is 5.
#' @param mshift The magnitude of the shift (k) in the mean that you want
#'   to detect, expressed in standard deviation units (often 0.5 * sigma).
#'   This 'reference value' is subtracted from observations to make the CUSUM
#'   sensitive to upward shifts. Default is 0.5.
#' @param tmean The target mean (mu0) of the process when it is in-control.
#'   If not provided, the mean of the input data 'x' is used.
#'
#' @return A list containing:
#'   \item{iupper}{Indices where the upper CUSUM signaled an out-of-control condition.}
#'   \item{uppersum}{A numeric vector of the upper CUSUM values at each point.}
#' @export
#'
#' @examples
#' # Example: Data with an upward shift
#' set.seed(123)
#' data_up <- c(rnorm(50, mean = 0, sd = 1), rnorm(50, mean = 2, sd = 1))
#' cusum_results_up <- cusum_upward_shift(data_up)
#' plot_cusum_upward(data_up, cusum_results_up) 
# Using a new plotting function




cusum_upward_shift <- function(x, climit = 5, mshift = 0.5, tmean = NULL) {
  if (is.null(tmean)) {
    tmean <- mean(x)
  }
  
  n <- length(x)
  uppersum <- numeric(n)
  iupper <- integer(0)
  
  # CUSUM calculations start from the second element (index 1 in R)
  for (i in 2:n) {
    # Upper CUSUM (detects increases in mean)
    # Su_i = MAX(0, Su_{i-1} + (x_i - tmean - mshift))
    uppersum[i] <- max(0, uppersum[i - 1] + (x[i] - tmean - mshift))
    
    # Check for signals and reset CUSUM sum
    if (uppersum[i] > climit) {
      iupper <- c(iupper, i)
      uppersum[i] <- 0 # Reset to 0 after signal
    }
  }
  
  list(iupper = iupper, uppersum = uppersum) # Only return relevant components
}


#' Plot CUSUM Chart Results for Upward Shift
#'
#' A helper function to plot the raw data, only the Upper CUSUM sum,
#' and signal points for upward shifts.
#'
#' @param data The original numeric vector of data.
#' @param cusum_results The list returned by the cusum_upward_shift function.
#' @param title An optional title for the plot.
#' @param xlab An optional label for the x-axis.
#' @param ylab An optional label for the y-axis.
#' @export
plot_cusum_upward <- function(data, cusum_results,
                              title = "CUSUM Control Chart (Upward Shift)",
                              xlab = "Sample Index",
                              ylab = "Value") {
  
  # Ensure ggplot2 is installed and loaded
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("Installing 'ggplot2' package...")
    install.packages("ggplot2", dependencies = TRUE)
  }
  library(ggplot2)
  
  # Create a data frame for plotting
  plot_df <- data.frame(
    Index = 1:length(data),
    Data = data,
    Upper_CUSUM = cusum_results$uppersum
  )
  
  # Base plot with data and Upper CUSUM sum
  p <- ggplot(plot_df, aes(x = Index)) +
    geom_line(aes(y = Data, color = "Data"), size = 0.8) +
    geom_line(aes(y = Upper_CUSUM, color = "Upper CUSUM"), size = 0.8) +
    labs(title = title, x = xlab, y = ylab) +
    scale_color_manual(
      name = "Legend",
      values = c("Data" = "darkgrey", "Upper CUSUM" = "darkgreen"),
      breaks = c("Data", "Upper CUSUM")
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Add upper signal points
  if (length(cusum_results$iupper) > 0) {
    p <- p + geom_point(
      data = plot_df[cusum_results$iupper, ],
      aes(y = Data, color = "Upper Signal"),
      shape = 21, size = 3, stroke = 1, fill = "orange"
    )
  }
  
  # Update color manual for signals if they exist
  signal_colors <- c("Data" = "darkgrey", "Upper CUSUM" = "darkgreen")
  signal_breaks <- c("Data", "Upper CUSUM")
  
  if (length(cusum_results$iupper) > 0) {
    signal_colors["Upper Signal"] <- "orange"
    signal_breaks <- c(signal_breaks, "Upper Signal")
  }
  
  p <- p + scale_color_manual(
    name = "Legend",
    values = signal_colors,
    breaks = signal_breaks
  )
  
  print(p)
}


# --- Example Usage for Upward Shift Detection ---
if (interactive()) {
  # Generate example data with an upward shift
  set.seed(42) # for reproducibility
  process_data_upward <- c(rnorm(50, mean = 10, sd = 1), rnorm(50, mean = 12, sd = 1))
  
  # Run the one-sided CUSUM chart for upward shift
  results_upward <- cusum_upward_shift(process_data_upward, climit = 5, mshift = 0.5)
  
  cat("--- One-Sided CUSUM (Upward Shift) Example ---\n")
  cat("Upper CUSUM Signals at indices:", results_upward$iupper, "\n")
  plot_cusum_upward(process_data_upward, results_upward)
}