library(zoo)

cusum_1 <- function(data) {
  cusum <- numeric(length(data))
  n <- length(data)
  
  for (i in 8:n) {
    mean_prev7 <- mean(data[(i-7):(i-1)], na.rm = TRUE)
    sd_prev7 <- sd(data[(i-7):(i-1)], na.rm = TRUE)
    
    if (is.na(sd_prev7) || sd_prev7 == 0) {
      increment <- 0   # or NA if you prefer to mark missing cases
    } else {
      increment <- (data[i] - (mean_prev7 + 3 * sd_prev7)) / sd_prev7
    }
    
    cusum[i] <- max(0, cusum[i-1] + increment)
  }
  
  cusum
}





cusum_2 <- function(data) {
  cusum <- numeric(length(data))
  n <- length(data)
  
  for (i in 8:n) {
    mean_prev7 <- mean(data[(i-7):(i-1)], na.rm = TRUE)
    sd_prev7 <- sd(data[(i-7):(i-1)], na.rm = TRUE)
    if (sd_prev7 == 0) {
      increment <- 0   # or you may use NA if preferred
    } else {
      increment <- (data[i] - (mean_prev7 + 3 * sd_prev7)) / sd_prev7
    }
    cusum[i] <- max(0, cusum[i-1] + increment)
  }
  cusum
}



cusum_3 <- function(data) {
  cusum <- numeric(length(data))
  n <- length(data)
  
  for (i in 4:n) {
    cusum[i] <- sum(data[(i-3):(i-1)], na.rm = TRUE)
  }
  
  cusum
}


signal <- function(data) {
  ifelse(data > 3, 1, 0)
}


signal_2 <- function(data) {
  ifelse(data > 2, 1, 0)
}

signal_4<- function(x, y) {
   x*y
}



signal_6<- function(x, y, z) {
  x*y*z
}



# Modifify cusum_fun


# Calculation are using the functions defined in CUSUM_help_fun.R


cusum_fun <- function(data){
  
  data %>%
  mutate(c1_value =cusum_1(daily_cases), 
         c2_value=cusum_1(daily_cases), 
         c3_value=cusum_3(c2_value), 
         c1_signal=signal(c1_value), 
         c2_signal=signal(c2_value), 
         c3_signal=signal_2(c3_value), 
         c4_signal=signal_4(c1_signal, c3_signal), 
         c5_signal=signal_4(c2_signal, c3_signal), 
         c6_signal=signal_6(c2_signal, c3_signal,c1_signal ))
  
  
}

