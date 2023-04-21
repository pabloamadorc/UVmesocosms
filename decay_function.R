deg_curve <- function(dates, concentrations) {
  data <- data.frame(date = dates, conc = concentrations)
  data <- data[order(data$date), ]
  data$decay <- c(NA, diff(log(data$conc)) / diff(data$date))
  seq_dates <- seq(min(data$date), max(data$date), by = 1)
  interp_conc <- rep(NA, length(seq_dates))
  measure_numba <- 1:(nrow(data)-1)
  
  for (z in measure_numba) {
    start_date <- data$date[z]
    end_date <- data$date[z+1]
    decay <- data$decay[z+1]
    
    interp_dates <- seq(start_date, end_date, by = 1)
    interp_conc[seq_dates == start_date] <- data$conc[z]
    for (i in 2:length(interp_dates)) {
      prev_date <- interp_dates[i-1]
      prev_conc <- interp_conc[seq_dates == prev_date]
      next_conc <- prev_conc * exp(decay)
      interp_conc[seq_dates == interp_dates[i]] <- next_conc
    }
  }
  q = ggplot(data.frame(date = seq_dates, conc = interp_conc), aes(x = date, y = conc)) +
    geom_line() +
    labs(x = "Date", y = "Concentration", title = "Degradation curve") +
    theme_bw()
  print(q)
  return(interp_conc)
}

dates <- c(0, 7, 13, 14, 21, 56)
concentrations <- c(1.9659406, 1.3826208, 0.8696116, 2.5825205, 1.6871537, 0.4838216)
ts_interp <- deg_curve(dates, concentrations)




