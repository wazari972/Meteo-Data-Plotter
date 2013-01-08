get_common_stats  <-function(date, data, options, units) {
  sum <- summary(data)
  sum <- sum[-2] #remove 1st quarter
  sum <- sum[-4] #remove 3rd quarter
  return(sum)
}

print_pluie <-function(name, date, pluie, options) {
  summ <- get_common_stats(date, pluie, options, "mm")
  summ <- summ[-1] #remove min
  summ[["Total"]] <- sum(pluie)
  
  print(summ)
}

print_a_temp <- function(name, date, temp, options, temp_name) {
  cat("-----", temp_name, "-----\n")
  print(get_common_stats(date, temp, options, "*"))
}

print_temp <-function(name, date, max, min, options) {
  print_a_temp(name, date, max, options, "Maximum")
  print_a_temp(name, date, min, options, "Minimum")
}

print_pression <-function(name, date, pressure, options) {
  print(get_common_stats(date, pressure, options, "hPa"))
}

print_hygro <-function(name, date, hygro, options) {
  print(get_common_stats(date, hygro, options, "%"))
}