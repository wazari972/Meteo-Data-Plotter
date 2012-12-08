cumul = function(lst) {
  current <- lst[1]
  for (i in 2:length(lst)) {
    current <- current + lst[i]
    lst[i] <- current
  }
  return(lst)
  
}

fill_gaps_zero = function(lst){
  for (i in 1:length(lst)) {
    if (is.null(lst[i]) || is.na(lst[i])) {
      lst[i] = 0
    }
  }
  return(lst)
}

fill_gaps_linear = function(lst){
  max <- length(lst)
  for (i in 1:max) {
    #if we have an invalid value
    if (!(is.null(lst[i]) || is.na(lst[i]))) {
      next
    }
    
    #pick up the prev value, or NA if there's no
    if (i != 1) {
      prev <- lst[i-1]
    } else {
      prev <- NA
    }	  
    #set j to next valid value
    j <- i + 1
    while (j < max && is.na(lst[j])) {
      j <- j+1
    }
    # i == j impossible
    
    # if we didn't reach the end of the list,
    if (j != max) {
      nxt = lst[j]
    } else {
      # use the last valid value
      nxt = prev		
    }
    
    #if we had no previous value, use the next valid one
    if (is.null(prev) || is.na(prev)) {
      prev = nxt
    }
    
    # fill the gap
    slice = (nxt-prev)/(j-i+1)
    for (k in i:(j)) {
      # with linearily increasing values, between prev and nxt
      lst[k] = prev + slice*(k-i+1)
    }
  }
  return(lst)
}

increase_to = function(lst, value){
  for (i in 1:length(lst)) {
    if (lst[i] < value)
      lst[i] <- lst[i] + value
    else 
      break
  }
  return(lst)
}

get_med = function(lst1, lst2){
  medium <- c()
  for (t in 1:length(lst1)) {
    medium <- c(medium, (lst1[t] + lst2[t]) / 2)
  }
  return(medium)
}

complete = function(lst, val, totalLength){
  diff <- totalLength - length(lst)
  if (diff == 0) {
    return(lst)
  } else {
    return(c(lst, seq(val, val, length=diff)))
  }
}

fix_data = function(data) {
  data$Date <- as.Date(data$Date, format="%d/%m/%Y")
  size <- length(data$Date)
  
  data$Pluie <- fill_gaps_zero(data$Pluie)
  data$Pluie <- complete(data$Pluie, 0, size)
  
  data$Hygrometrie <- fill_gaps_linear(data$Hygrometrie)
  hygro <- complete(data$Hygrometrie, mean(data$Hygrometrie), size)
  
  data$Temp.max <- fill_gaps_linear(data$Temp.max)
  data$Temp.min <- fill_gaps_linear(data$Temp.min)
  data$Temp.max <- complete(data$Temp.max, mean(data$Temp.max), size)
  data$Temp.min <- complete(data$Temp.min, mean(data$Temp.min), size)
  
  data$Pression <- fill_gaps_linear(data$Pression)
  data$Pression <- increase_to(data$Pression, 1000)
  
  return(data)
}