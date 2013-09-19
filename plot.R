#! /usr/bin/Rscript

source("tools.R")

require(lattice)

splitPlot <- function (idx, len) {
  if (idx == 1) {
    par(mfrow=c(len, 1))
  } else {
    #par()
  }
}

splitPlots <- function (dataset, do_plot) {
  iterateDataset(dataset, function(idx, name, data) {
    splitPlot(idx, length(dataset))
    do_plot(name, data)
  })
}

add_legends = function (axe=2, legend=NULL, name=NULL) {
  if (!is.null(name)) {
    title (name)
  }
  
  if (!is.null(legend)) {
    mtext (legend, axe, line=3)
  }
  
  box()
}

set_axes <- function (dates, ylim=NULL, yaxe=2) {  
  # Some date arithmetics
  all_days <- seq.Date( from=min(dates), to=max(dates), by=1 )
  months  <- all_days[ format(all_days, "%d") == "01" ]
  january <- all_days[ format(all_days, "%m-%d") == "01-01" ]
  april   <- all_days[ format(all_days, "%m-%d") == "04-01" ]
  july    <- all_days[ format(all_days, "%m-%d") == "07-01" ]
  october <- all_days[ format(all_days, "%m-%d") == "10-01" ]
  
  # Finally plot the axes
  axis.Date(1, at=months,  label=FALSE, tcl=-.3) 
  axis.Date(1, at=january, label=format(january, "%Y"))
  axis.Date(1, at=april,   label=format(april,   "%b"))
  axis.Date(1, at=july,    label=format(july,    "%b"))
  axis.Date(1, at=october, label=format(october, "%b"))
  
  
  if (!is.null(ylim)) {
    axis(yaxe, ylim)
  }
}

init_graph <- function (xlim, ylim, axe=2) {
    par(new=TRUE)
    
    plot.window(xlim, ylim)
    axis (axe)
}

plot_pluie = function (name, dates, pluie, options) {
  bad<-ifelse(pluie > options$rainthreshold, "blue","cornflowerblue")
  
  plot(dates, pluie,  type=ifelse (options$with_daily, 'h', 'n'),
       col=bad, xaxt='n', xlab="", ylab="")
  
  if (options$with_daily) {
    abline(h=options$rainthreshold, col="cornflowerblue", lwd=0.3)
  }
  
  set_axes(dates, c(0, max(pluie)))
  
  if (options$with_smooth) {
    add_regression_curve(dates, pluie, ifelse(options$with_daily, "purple", "blue"), options$smooth_coeff)
  }
  
  if (options$with_mean) {
    meanPluie <- mean(pluie)
    abline(h=meanPluie, col="blue", lty=2)
  }
  
  if (options$with_cumul) {
    cumuled <- cumul(pluie)
    init_graph(range(dates), ylim=c(0, cumuled[length(pluie)]), axe=4)
    
    lines(dates, cumuled, col="blue")
  } 
  
  add_legends(legend='Precipitations (mm)', name=name)
}

plot_hygro_temp = function(name, dates, hygro,  max, min, options) {
    plot(dates, hygro, col="seagreen4", type=ifelse (options$with_daily, 'l', 'n'),
         xaxt='n', xlab="", ylab="")
  
    set_axes(dates,  c(min(hygro, 0), 100))
    
    axis(side=1, at=1:length(dates), labels=strftime(dates, format="%b"), las=1)
    add_legends(legend='Hygro (%)', name=name)

    temp_range <- range(min, max)
    init_graph(range(dates), ylim=temp_range, axe=4)

    lines (dates, max, col="tomato2")
    lines (dates, min, col="royalblue")
    add_legends(legend='Temp (C)', name=name, axe=4)    
    
    box()
}

plot_hygro = function(name, dates, hygro, options) {  
  plot(dates, hygro, col="seagreen4", type=ifelse (options$with_daily, 'l', 'n'),
       xaxt='n', xlab="", ylab="")
  
  set_axes(dates,  c(min(hygro, 0), 100))
  
  axis(side=1, at=1:length(dates), labels=strftime(dates, format="%b"), las=1)
  add_legends(legend='Hygro (%)', name=name)
  
  if (options$with_mean) {
    meanHygro <- mean(hygro)  
	  abline(h=meanHygro, col="seagreen4", lty=2)
  }
  
  if (options$with_smooth) {
    add_regression_curve(dates, hygro, ifelse(options$with_daily, "purple", "seagreen4"), options$smooth_coeff)
  }
  
  box()
}

plot_temp = function(name, dates, max, min, options) {
  temp_range <- range(min, max)
  
  plot(dates, max, col="tomato2", type='n',
      xaxt='n', xlab="", ylab="", ylim=temp_range)
  
  add_legends(legend='Temp (C)', name=name)    
  set_axes(dates)
  
  if (options$with_max) {
    if (options$with_daily) {
      lines (dates, max, col="tomato2")
    }
	  
    if (options$with_smooth) {
	    add_regression_curve(dates, max, ifelse(options$with_daily, "royalblue", "tomato2"), options$smooth_coeff)
    }
    
    if (options$with_mean) {
      meanMax <- mean(max)
	    abline(h=meanMax, col="tomato2", lty=2)
    }
  }
  
  if (options$with_min) {
    if (options$with_daily) {
      lines (dates, min, col="royalblue")
    }
    
	  if (options$with_smooth) {
	    add_regression_curve(dates, min, ifelse(options$with_daily, "tomato2", "royalblue"), options$smooth_coeff)
	  }
    
    if (options$with_mean) {
      meanMin <- mean(min)
	    abline(h=meanMin, col="royalblue", lty=2)
    }
  }
  
  if (options$with_med) {
    medium <- get_med(min, max)
    
    if (options$with_daily) {
      lines (dates, medium, col="seagreen4")
    }
    
    if (options$with_smooth) {
      add_regression_curve(dates, medium, ifelse(options$with_daily, "purple", "seagreen4"), options$smooth_coeff)
    }
    
    if (options$with_mean) {
      meanMed <- mean(medium)
      abline(h=meanMed, col="seagreen4", lty=2)
    }
  }
  
  if (options$with_zero) {
    abline(h=0, col="black", lty=3)
  }
  
  box()
}

plot_pression = function(name, dates, pression, options) {
  plot(dates, pression, col="orange3", type=ifelse (options$with_daily, 'l', 'n'),
       xaxt='n', xlab="", ylab="")
  
  set_axes(dates)
  
  add_legends(legend='Pression (hPa)', name=name)
  
  if (options$with_mean) {
    meanPression <- mean(pression)
	  abline(h=meanPression, col="orange3",lty=2)
  }
  
  if (options$with_smooth) {
    add_regression_curve(dates, pression, ifelse(options$with_daily, "purple", "orange3"), options$reg_coeff)
  }
  
  box()
}

plot_summary = function (name, data, options) {
  plots = c(function() {
    plot_pluie(name, data$Date, data$Pluie, options)
  }, function() {
    plot_hygro(name, data$Date, data$Hygrometrie, options)
  }, function() {
    plot_temp(name, data$Date, data$Temp.max, data$Temp.min,  options)
  }, function() {
    plot_pression(name, data$Date, data$Pression, options)
  })
  
  nb_plots <- length(plots)
  for (idx in 1:nb_plots) {
    splitPlot(idx, nb_plots)
    plots[idx][[1]]()
  }
}

add_regression_curve = function(dates, data, color, coeff) {
  curve <- smooth.spline(dates, data, spar=coeff)
  lines(curve, col=color)
  return(curve)
}
