#' Detect Steps from Verisense Accelerometer Data
#'
#' @description
#' Implements a step detection algorithm based on the root-mean-square (RMS) of 3-axis acceleration,
#' following the methodology described by Gu et al. (2017), with enhancements for real-world data.
#' Steps are identified from peaks in the RMS signal using a series of signal quality and temporal filters.
#'
#' @details
#' The algorithm involves:
#' \itemize{
#'   \item Calculating the RMS acceleration from the 3D signal.
#'   \item Splitting the signal into segments of size \code{k} and detecting local peaks.
#'   \item Filtering peaks by magnitude, periodicity, similarity, and signal continuity.
#'   \item Counting the remaining valid peaks as steps.
#' }
#' It returns the number of steps per second, compatible with use in packages like \code{GGIR}.
#'
#' @param input_data A numeric matrix or data frame with 3 columns (X, Y, Z accelerations).
#'                   Defaults to a random 3-axis signal of length 500 if not specified.
#' @param coeffs A numeric vector of 8 algorithm parameters:
#'   \describe{
#'     \item{1. \code{k}}{Segment size (in samples) for local peak search.}
#'     \item{2. \code{period_min}}{Minimum allowable period between steps (in samples).}
#'     \item{3. \code{period_max}}{Maximum allowable period between steps (in samples).}
#'     \item{4. \code{sim_thres}}{Similarity threshold for second-order peak height differences.}
#'     \item{5. \code{cont_win_size}}{Number of continuous valid windows required.}
#'     \item{6. \code{cont_thres}}{Number of steps to consider in the continuity window.}
#'     \item{7. \code{var_thres}}{Minimum variance in the windowed acceleration signal.}
#'     \item{8. \code{mag_thres}}{Minimum peak magnitude to consider a step.}
#'   }
#'
#' @return A numeric vector representing the number of detected steps per second.
#'
#' @references
#' Gu, F., et al. (2017). "A new approach for detecting walking steps using wearable sensors:
#' A validation study." \emph{Sensors}.
#'
#' @author
#' Original algorithm by Matthew R. Patterson (\email{mpatterson@@shimmersensing.com})
#'
#' @examples
#' # Simulated 3-axis data
#' fake_data <- matrix(rnorm(1500), ncol = 3)
#' coeffs <- c(30, 10, 60, -0.25, 3, 5, 0.001, 0.15)
#' verisense_count_steps(input_data = fake_data, coeffs = coeffs)
#' 
#' @importFrom stats sd var
#'
#' @export
verisense_count_steps <- function(input_data = c(0,1,0), coeffs = c(0,0,0)) {
  # by Matthew R Patterson, mpatterson@shimmersensing.com
  ## Find peaks of RMS acceleration signal according to Gu et al, 2017 method
  # This method is based off finding peaks in the summed and squared acceleration signal
  # and then using multiple thresholds to determine if each peak is a step or an artefact.
  # An additional magnitude threshold was added to the algorithm to prevent false positives 
  # in free living data. 
  #
  # returns sample location of each step
  fs = 15 # temporary for now, this is manually set
  acc <- sqrt(input_data[,1]^2 + input_data[,2]^2 + input_data[,3]^2)
  
  if (stats::sd(acc) < 0.025) {
    # acceleration too low, no steps
    num_seconds = round(length(acc) / fs)
    steps_per_sec = rep(0,num_seconds)
  } else {
    # Search for steps
    # Thresholds
    k <- coeffs[[1]]
    period_min <- coeffs[[2]]
    period_max <- coeffs[[3]]
    sim_thres <- coeffs[[4]]      # similarity threshold
    cont_win_size <- coeffs[[5]]  # continuity window size
    cont_thres <- coeffs[[6]]     # continuity threshold
    var_thres <- coeffs[[7]]      # variance threshold
    mag_thres <- coeffs[[8]]
    
    # find the peak rms value is every range of k
    half_k <- round(k/2)
    segments <- floor(length(acc) / k)
    peak_info <- matrix(NA, nrow = segments, ncol = 5)
    # peak_info[,1] - peak location
    # peak_info[,2] - acc magnitude
    # peak_info[,3] - periodicity (samples)
    # peak_info[,4] - similarity
    # peak_info[,5] - continuity
    
    # for each segment find the peak location
    for (i in 1:segments) {
      start_idx <- (i - 1) * k + 1
      end_idx <- start_idx + (k - 1)
      tmp_loc_a <- which.max(acc[start_idx:end_idx])
      tmp_loc_b <- (i - 1) * k + tmp_loc_a
      # only save if this is a peak value in range of -k/2:+K/2
      start_idx_ctr <- tmp_loc_b - half_k
      if (start_idx_ctr < 1) {
        start_idx_ctr <- 1
      }
      end_idx_ctr <- tmp_loc_b + half_k
      if (end_idx_ctr > length(acc)) {
        end_idx_ctr <- length(acc)
      }
      check_loc <- which.max(acc[start_idx_ctr:end_idx_ctr])
      if (check_loc == (half_k + 1)) {
        peak_info[i,1] <- tmp_loc_b
        peak_info[i,2] <- max(acc[start_idx:end_idx])
      }
    }
    peak_info <- peak_info[is.na(peak_info[,1]) != TRUE,] # get rid of na rows
    
    # filter peak_info[,2] based on mag_thres
    peak_info <- peak_info[peak_info[,2] > mag_thres,]
    if (length(peak_info) > 10) {  # there must be at least two steps
      num_peaks <- length(peak_info[,1])
      
      no_steps = FALSE
      if (num_peaks > 2) {
        # Calculate Features (periodicity, similarity, continuity)
        peak_info[1:(num_peaks - 1),3] <- diff(peak_info[,1]) # calculate periodicity
        peak_info <- peak_info[peak_info[,3] > period_min,] # filter peaks based on period_min
        peak_info <- peak_info[peak_info[,3] < period_max,]   # filter peaks based on period_max 
      } else {
        no_steps = TRUE
      }
    } else {
      no_steps = TRUE
    }
    
    if ( length(peak_info) == 0 || length(peak_info) == sum(is.na(peak_info)) || no_steps == TRUE) {
      # no steps found
      num_seconds = round(length(acc) / fs)
      steps_per_sec = rep(0,num_seconds)
    } else {
      # calculate similarity
      num_peaks <- length(peak_info[,1])
      peak_info[1:(num_peaks - 2),4] <- -abs(diff(peak_info[,2], 2)) # calculate similarity
      peak_info <- peak_info[peak_info[,4] > sim_thres,]  # filter based on sim_thres
      peak_info <- peak_info[is.na(peak_info[,1]) != TRUE,] # previous statement can result in an NA in col-1
      
      # calculate continuity
      if (length(peak_info[,3]) > 5) {
        end_for <- length(peak_info[,3]) - 1
        for (i in cont_thres:end_for) {
          # for each bw peak period calculate acc var
          v_count <- 0 # count how many windows were over the variance threshold
          for (x in 1:cont_thres) {
            if (stats::var(acc[peak_info[i - x + 1, 1]:peak_info[i - x + 2, 1]]) > var_thres) {
              v_count = v_count + 1
            }
          }
          if (v_count >= cont_win_size) {
            peak_info[i,5] <- 1 # set continuity to 1, otherwise, 0
          } else {
            peak_info[i,5] <- 0
          }
        }
      } 
      peak_info <- peak_info[peak_info[,5] == 1,1] # continuity test - only keep locations after this
      peak_info <- peak_info[is.na(peak_info) != TRUE] # previous statement can result in an NA in col-1
      
      if (length(peak_info) == 0) {
        # no steps found
        num_seconds = round(length(acc) / fs)
        steps_per_sec = rep(0,num_seconds)
      } else {
        
        # debug plot
        # is_plot = F
        # if (is_plot) {
        #   library(ggplot2)
        #   library(plotly)
        #   acc.df <- data.frame(acc=acc, det_step=integer(length(acc)))
        #   acc.df$det_step[peak_info] <- 1  # to plot annotations, prepare a 0/1 column on dataframe
        #   acc.df$idx <- as.numeric(row.names(acc.df))
        #   pl <- ggplot(data=acc.df,aes(x=idx,y=acc)) 
        #   pl2 <- pl + geom_line()
        #   pl3 <- pl2 + geom_point(data=subset(acc.df,det_step==1),aes(x=idx,y=acc),color='red',size=1,alpha=0.7)
        #   pl4 <- ggplotly(pl3)
        #   print(pl4)  
        # }
        
        # for GGIR, output the number of steps in 1 second chunks
        start_idx_vec <- seq(from = 1, to = length(acc), by = fs)
        steps_per_sec <- table(factor(findInterval(peak_info, start_idx_vec), 
                                      levels = seq_along(start_idx_vec)))
        steps_per_sec <- as.numeric(steps_per_sec)
      }
    }
  }
  
  return(steps_per_sec)
}
