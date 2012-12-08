#! /usr/bin/Rscript

source("tools.R")

require(lattice)

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

# plot_summary(meteoData$Grenoble.csv)
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
    strip.labels <- c(lab.T.min, lab.T.max, lab.rain, lab.hum, lab.bar)
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
  xyplot(Temp.min + Temp.max + Pluie + Hygrometrie + Pression ~ Date, data=data,
         scales=list(y="free", rot=0), xlim=xlim,
         strip=my.strip, outer=TRUE, layout=c(1, 5, 1), ylab="",
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
                  title="Weather around Grenoble",
                  col=c(col.raw, col.smo, col.lm), lty=c(1, 1, 2),
                  columns=2, cex=0.95,
                  lines=TRUE
         ),
  )
}

plot_pluie = function (dates, pluie, options) {  
  size <- length(dates)
  
  if (options$with_mean) {
	  meanPluie <- mean(pluie)
  }
  
  init_graph(xlim=c(0, size),  ylim=c(0, max(pluie)), legend='Precipitations (mm)')
  
  if (options$with_daily) {
    lines(pluie, xaxt = "n",  type='h', lwd=2, lend=4, col='blue')
  }
  
  if (options$with_reg) {
    add_regression_curve(pluie, "red", options$reg_coeff)
  }
  
  if (options$with_mean) {
    abline(h=meanPluie, col="blue", lty=2)
  }
  
  if (options$with_cumul) {
    cumuled <- cumul(pluie)    
    par(new=TRUE)
    init_graph(xlim=c(0, size),  ylim=c(0, cumuled[length(pluie)]), axe=4, new=TRUE)
    
    lines(cumuled, col="darkblue")
  }  
}

plot_hygro = function(dates, hygro, options) {  
  size <- length(dates)
  
	meanHygro <- mean(hygro)  
	hygroRange <- range(hygro)
	
  init_graph(xlim=c(0, size),  ylim=c(min(hygroRange), 100), legend='Hygro (%)')
  
  if (options$with_daily) {
	  lines(seq(0.5, size-0.5, 1), hygro, col="seagreen4")
  }
  
  if (options$with_mean) {
	  abline(h=meanHygro, col="seagreen4", lty=2)
  }
  
  if (options$with_reg) {
    add_regression_curve(hygro, "purple", options$reg_coeff)
  }
  
	box()
}

plot_temp = function(dates, max, min, options) {
  size <- length(dates)
  
	tempRange <- c(min(0, min(min)), max(max, 35))

	meanMax <- mean(max)
	meanMin <- mean(min)

  init_graph(xlim=c(0, size),  ylim=tempRange, legend='Temp (°C)')
  
  if (options$with_max) {
    if (options$with_daily) {
	    lines (seq(0.5, size-0.5, 1), max, col="tomato2")
    }
	  
    if (options$with_reg) {
	    add_regression_curve(max, "royalblue", options$reg_coeff)
    }
    
    if (options$with_mean) {
	    abline(h=meanMax, col="tomato2", lty=2)
    }
  }
  
  if (options$with_min) {
    if (options$with_daily) {
	    lines (seq(0.5, size-0.5, 1), min, col="royalblue")
    }
    
	  if (options$with_reg) {
	    add_regression_curve(min, "tomato2", options$reg_coeff)
	  }
    
    if (options$with_mean) {
	    abline(h=meanMin, col="royalblue", lty=2)
    }
  }
  
  if (options$with_med) {
    medium <- get_med(min, max)
    
    if (options$with_daily) {
	    lines (seq(0.5, size-0.5, 1), medium, col="seagreen4")
    }
    
    if (options$with_reg) {
      add_regression_curve(medium, "purple", options$reg_coeff)
    }
    
    if (options$with_mean) {
      meanMed <- mean(medium)
      abline(h=meanMed, col="seagreen4", lty=2)
    }
  }
  
  if (options$with_zero) {
    abline(h=0, col="black", lty=2)
  }
  
	box()
}

plot_pression = function(dates, pression, options) {
  size <- length(dates)
  
	pressionRange <- range(pression) 
  pressionRange[1] <- pressionRange[1] - 1
  pressionRange[2] <- pressionRange[2] + 1
  
	meanPression <- mean(pression)

  init_graph(xlim=c(0, size),  ylim=pressionRange, legend='Pression (hPa)')
  
  if (options$with_daily) {
	  lines(seq(0.5, size-0.5, 1), pression, col="orange3")
  }
  
  if (options$with_mean) {
	  abline(h=meanPression, col="orange3",lty=2)
  }
  
  if (options$with_reg) {
    add_regression_curve(pression, "purple", options$reg_coeff)
  }
  
  box()
}

add_regression_curve = function(data, color, coeff) {
  curve <- smooth.spline(data, spar=coeff)
  lines(curve, col=color)
  return(curve)
}
