#! /usr/bin/Rscript

require(lattice)

plot_summary = function (data) {
  #--Define plot titles:
  lab.bar <-  "Air pressure (hpa)"
  lab.hum <-  "Humidity (%)"
  lab.rain <- "Rainfall (mm/day)"
  lab.T.min <- as.expression(expression( paste("Minimum temperature (",
                                               degree*C, ")") ))
  lab.T.max <- as.expression(expression( paste("Maximum temperature (",
                                               degree*C, ")") ))

  
  #--Custom strip function:
  # (NB the colour used is the default lattice strip background colour)
  my.strip <- function(which.given, which.panel, ...) {
    strip.labels <- c(lab.T.min, lab.T.max, lab.hum, lab.rain, lab.bar)
    panel.rect(0, 0, 1, 1, col="#ffe5cc", border=1)
    panel.text(x=0.5, y=0.5, adj=c(0.5, 0.55), cex=0.95,
               lab=strip.labels[which.panel[which.given]])
  }
  
  #--Define X axis date range:
  xlim <- range(data$Date)
  
  #--Define annual quarters for plot grid line markers:
  d <- seq(from=xlim[1], to=xlim[2], by=365/4)
  
  #--Define colours for raw & smoothed data:
  col.raw <- "#377EB8"    #colset[2] } see note above
  col.smo <- "#E41A1C"    #colset[1] }
  col.lm <- "grey20"
  
  #--Create multipanel plot:
  xyplot(Temp.max + Temp.min + Pluie + Hygrometrie ~ Date, data=data,
         scales=list(y="free", rot=0), xlim=xlim,
         strip=my.strip, outer=TRUE, layout=c(1, 4, 1), ylab="",
         panel=function(x, y, ...) {panel.grid(h=-1, v=0) # plot default horizontal gridlines
                                    panel.abline(v=d, col="grey90") # custom vertical gridlines
                                    panel.xyplot(x, y, ..., type="l",
                                                 col=col.raw, lwd=0.5) # raw data
                                    panel.loess(x, y, ..., col=col.smo,
                                                span=0.14, lwd=0.5) # smoothed data
                                    panel.abline(h=median(y, na.rm=TRUE),
                                                 lty=2, col=col.lm, lwd=1) # median value
         },
         key=list(text=list(c("raw data", "smoothed curve", "median value")),
                  title="Birmingham Wast Hills Observatory average midday weather",
                  col=c(col.raw, col.smo, col.lm), lty=c(1, 1, 2),
                  columns=2, cex=0.95,
                  lines=TRUE
         ),
  )
}

plot_pluie = function(pluie, with_daily, with_mean, with_cumul, with_reg, reg_coeff) {
  pluie <- fill_gaps_zero(pluie)
  
  if (with_mean) {
	  meanPluie <- mean(pluie)
  }
  
	pluie <- complete(pluie, 0, size)

  plot.new()
  plot.window(xlim=c(0, size),  ylim=c(0, max(pluie)), xaxs='i', yaxs='i')
  axis (2)
  
  if (with_daily) {
    lines(pluie, type='h', lwd=2, lend=4, col='blue')
  }
  
  if (with_reg) {
    add_regression_curve(pluie, "red", reg_coeff)
  }
  
  if (with_mean) {
    abline(h=meanPluie, col="blue", lty=2)
  }
  
  if (with_cumul) {
    cumuled <- cumul(pluie)
    par(new=TRUE)
    
    plot.window(xlim=c(0, size), ylim=c(0, cumuled[length(pluie)]), xaxs='i', yaxs='i')
    axis(4, xlim=c(0, size), ylim=c(0, cumuled[length(pluie)]))
    
    lines(cumuled, col="darkblue")
  }
  
  mtext ('Precipitations (mm)', 2, line=3)
  box()
}

plot_hygro = function(hygro, with_daily, with_mean, with_reg, reg_coeff) {
  hygro <- fill_gaps_linear(hygro)
  
	meanHygro <- mean(hygro)
	
	hygro <- complete(hygro, meanHygro, size)
  
	hygroRange <- range(hygro)
	
	plot.new()
	plot.window(xlim=c(0, size), ylim=c(min(hygroRange),100), xaxs='i', yaxs='i')
	axis (2)
	mtext ('Hygro (%)', 2, line=3)
  
  if (with_daily) {
	  lines(seq(0.5, size-0.5, 1), hygro, col="seagreen4")
  }
  
  if (with_mean)
	  abline(h=meanHygro, col="seagreen4", lty=2)
  
  if (with_reg)
    add_regression_curve(hygro, "purple", reg_coeff)
  
	box()
}

plot_temp = function(max, min, with_daily, with_mean, with_min, with_max, with_med, with_reg, reg_coeff) {
  max <- fill_gaps_linear(max)
  min <- fill_gaps_linear(min)
  
	tempRange <- range(max, min)
	tempRange <- range(min(0, min(tempRange)), max(tempRange, 35))

	meanMax <- mean(max)
	meanMin <- mean(min)

	max <- complete(max, meanMax, size)
	min <- complete(min, meanMin, size)

	plot.new()
	plot.window(xlim=c(0,size), ylim=tempRange, xaxs='i', yaxs='i')
	axis (2)
	mtext ('Temp (??C)', 2, line=3)
  
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

plot_pression = function(pression, with_daily, with_mean, with_reg, reg_coeff) {
  pression <- fill_gaps_linear(pression)
  
	pression <- increase_to(pression, 1000)
	pressionRange <- range(max(pression)+1, min(pression)-1) 
  
	meanPression <- mean(pression)

	pression <- complete(pression, meanPression, size)
	plot(0, 0, xlim=c(0,size), ylim=pressionRange, xaxs='i', yaxs='i')
	axis (2)
	mtext('Pression (hPa)', 2, line=3)
  
  if (with_daily) {
	  lines(seq(0.5, size-0.5, 1), pression, col="orange3")
  }
  
  if (with_mean) {
	  abline(h=meanPression, col="orange3",lty=2)
  }
  
  if (with_reg) {
    add_regression_curve(pression, "purple", reg_coeff)
  }
  
	box()
}

add_regression_curve = function(data, color, coeff) {
  curve <- smooth.spline(data, spar=coeff)
  lines(curve, col=color)
  return(curve)
}

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
	  if (is.na(lst[i])) {
	    lst[i] = 0
	  }
	}
	return(lst)
}

fill_gaps_linear = function(lst){
  max <- length(lst)
	for (i in 1:max) {
	  #if we have an invalid value
	  if (!is.na(lst[i])) {
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
		if (is.na(prev)) {
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
	for (t in 1:size) {
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
