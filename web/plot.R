source("config.R")

init_graph = function (xlim, ylim, axe=2, new=TRUE, legend=NULL) {
  if (new) {
    plot.new()
  } else {
    par(new=TRUE)
  }
  
  plot.window(xlim,  ylim, xaxs='i', yaxs='i')
  axis (axe)
  
  if (!is.null(legend)) {
    mtext (legend, 2, line=3)
  }
  
  box()
}

plot_pluie = function (dates, pluie, with_daily, with_mean, with_cumul, with_reg, reg_coeff) {
  size  <- length(dates)
  
  pluie <- fill_gaps_zero(pluie)
  
  if (with_mean) {
	  meanPluie <- mean(pluie)
  }
  
	pluie <- complete(pluie, 0, size)
  
  init_graph(xlim=c(0, size),  ylim=c(0, max(pluie)), legend='Precipitations (mm)')
  
  if (with_daily) {
    lines(pluie, xaxt = "n",  type='h', lwd=2, lend=4, col='blue')
  }
  
  if (with_reg) {
    add_regression_curve(pluie, "red", reg_coeff)
  }
  
  if (with_mean) {
    abline(h=meanPluie, col="blue", lty=2)
  }
  
  if (with_cumul) {
    cumuled <- cumul(pluie)
    
    init_graph(xlim=c(0, size),  ylim=c(0, cumuled[length(pluie)]), axe=4, new=FALSE)
    
    lines(cumuled, col="darkblue")
  }
  
  
}

plot_hygro = function (dates, hygro, with_daily, with_mean, with_reg, reg_coeff) {
  size  <- length(dates)
  
  hygro <- fill_gaps_linear(hygro)
  
	meanHygro <- mean(hygro)
	
	hygro <- complete(hygro, meanHygro, size)
  
	hygroRange <- range(hygro)
	
  init_graph(xlim=c(0, size),  ylim=c(min(hygroRange), 100), legend='Hygro (%)')
  
  if (with_daily) {
	  lines(seq(0.5, size-0.5, 1), hygro, col="seagreen4")
  }
  
  if (with_mean)
	  abline(h=meanHygro, col="seagreen4", lty=2)
  
  if (with_reg)
    add_regression_curve(hygro, "purple", reg_coeff)
  
	box()
}

plot_temp = function (dates, max, min, with_daily, with_mean, with_min, with_max, with_med, with_reg, reg_coeff) {
  size  <- length(dates)
  
  max <- fill_gaps_linear(max)
  min <- fill_gaps_linear(min)
  
	tempRange <- range(max, min)
	tempRange <- range(min(0, min(tempRange)), max(tempRange, 35))

	meanMax <- mean(max)
	meanMin <- mean(min)

	max <- complete(max, meanMax, size)
	min <- complete(min, meanMin, size)

  init_graph(xlim=c(0, size),  ylim=tempRange, legend='Temp (°C)')
  
  if (with_max) {
    if (with_daily) {
	    lines (seq(0.5, size-0.5, 1), max, col="tomato2")
    }
	  
    if (with_reg) {
	    add_regression_curve(max, "royalblue", reg_coeff)
    }
    
    if (with_mean) {
	    abline(h=meanMax, col="tomato2", lty=2)
    }
  }
  
  if (with_min) {
    if (with_daily) {
	    lines (seq(0.5, size-0.5, 1), min, col="royalblue")
    }
    
	  if (with_reg) {
	    add_regression_curve(min, "tomato2", reg_coeff)
	  }
    
    if (with_mean) {
	    abline(h=meanMin, col="royalblue",lty=2)
    }
  }
  
  if (with_med) {
    medium <- get_med(min, max)
    
    if (with_daily) {
	    lines (seq(0.5, size-0.5, 1), medium, col="seagreen4")
    }
    
    if (with_reg) {
      add_regression_curve(medium, "purple", reg_coeff)
    }
    
    if (with_mean) {
      meanMed <- mean(medium)
      abline(h=meanMed, col="seagreen4",lty=2)
    }
  }
  
	box()
}

plot_pression = function (dates, pression, with_daily, with_mean, with_reg, reg_coeff) {
  size  <- length(pression)
  
  pression <- fill_gaps_linear(pression)
  
	pression <- increase_to(pression, 1000)
	pressionRange <- range(max(pression)+1, min(pression)-1) 
  
	meanPression <- mean(pression)

  init_graph(xlim=c(0, size),  ylim=pressionRange, legend='Pression (hPa)')
  
  if (with_daily) {
	  lines(seq(0.5, size-0.5, 1), pression, col="orange3")
  }
  
  if (with_mean) {
	  abline(h=meanPression, col="orange3",lty=2)
  }
  
  if (with_reg) {
    add_regression_curve(pression, "purple", reg_coeff)
  }
}

add_regression_curve = function (data, color, coeff) {
  curve <- smooth.spline(data, spar=coeff)
  lines(curve, col=color)
  return (curve)
}

cumul = function (lst) {
  current <- lst[1]
  for (i in 2:length(lst)) {
    current <- current + lst[i]
    lst[i] <- current
  }
  return (lst)

}

fill_gaps_zero = function (lst){
	for (i in 1:length(lst)) {
	  if (is.null(lst[i]) || is.na(lst[i])) {
	    lst[i] = 0
	  }
	}
	return (lst)
}

fill_gaps_linear = function (lst){
  size <- length(lst)
	for (i in 1:size) {
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
		while (j < size && is.na(lst[j])) {
		  j <- j+1
		}
		# i == j impossible
		
		# if we didn't reach the end of the list,
		if (j != size) {
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
	return (lst)
}

increase_to = function (lst, value){
	for (i in 1:length(lst)) {
		if (lst[i] < value)
			lst[i] <- lst[i] + value
		else 
			break
	}
	return (lst)
}

get_med = function (lst1, lst2){
	medium <- c()
	for (t in 1:length(lst1)) {
	  medium <- c(medium, (lst1[t] + lst2[t]) / 2)
	}
	return (medium)
}

complete = function (lst, val, totalLength){
	diff <- totalLength - length(lst)
	if (diff == 0) {
		return (lst)
	} else {
		return (c(lst, seq(val, val, length=diff)))
	}
}
