#! /usr/bin/Rscript

meteoRevel <- read.csv(file="Revel.csv",head=TRUE,sep=";")
meteoMque <- read.csv(file="Martinique.csv",head=TRUE,sep=";")
meteoBonnot <- read.csv(file="Montbonnot.csv",head=TRUE,sep=";")
meteoGrenoble <- read.csv(file="Grenoble.csv",head=TRUE,sep=";")


plot_pluie = function(pluie) {
  pluie <- fill_gaps_zero(pluie)
  
	meanPluie <- mean(pluie)
	
	pluie <- complete(pluie, 0, size)
  lty.o <- par("lty")

	par (mar=c(2, 4, 4, 4) + 0.1, lty=0)
	barplot (pluie, space=0,  col='blue', ylab='Precipitations (mm)', axes=FALSE)
  # Reset to old value
  par(lty = lty.o)
	abline(h=meanPluie, col="blue",lty=2)

	axis (2, at=seq(0, max(pluie), 5))
  
	box()
}

plot_hygro = function(hygro) {
  hygro <- fill_gaps_linear(hygro)
  
	meanHygro <- mean(hygro)
	
	hygro <- complete(hygro, meanHygro, size)
  
	hygroRange <- range(hygro)
	
	plot.new()
	plot.window(xlim=c(0,size), ylim=c(min(hygroRange),100), xaxs='i', yaxs='i')
	axis (2)
	mtext ('Hygro (%)', 2, line=3)
	lines (seq(from=0.5, to=size-0.5,by=1), hygro, col="seagreen4")
	abline(h=meanHygro, col="seagreen4", lty=2)
	box()
}

plot_temp = function(max, min) {
  max <- fill_gaps_linear(max)
  min <- fill_gaps_linear(min)
  
	tempRange <- range(max, min)
	tempRange <- range(min(0, min(tempRange)), max(tempRange, 35))

	meanMax <- mean(max)
	meanMin <- mean(min)

	max <- complete(max, meanMax, size)
	min <- complete(min, meanMin, size)
	
	average <- get_avg(min, max)

	plot.new()
	plot.window(xlim=c(0,size), ylim=tempRange, xaxs='i', yaxs='i')
	axis (2)
	mtext ('Temp (°C)', 2, line=3)
	lines (seq(0.5,size-0.5,1), max, col="tomato2")
	abline(h=meanMax, col="tomato2",lty=2)
	lines (seq(0.5,size-0.5,1), min, col="royalblue")
	abline(h=meanMin, col="royalblue",lty=2)
	lines (seq(0.5,size-0.5,1), average, col="royalblue")
	box()
}

plot_pression = function(pression) {
  pression <- fill_gaps_linear(pression)
  
	pression <- increase_to(pression, 1000)
	pressionRange <- range(max(pression)+1, min(pression)-1) 
	meanPression <- mean(pression)

	pression <- complete(pression, meanPression, size)
	plot(0, 0, xlim=c(0,size), ylim=pressionRange, xaxs='i', yaxs='i')
	axis (2)
	mtext ('Pression (hPa)', 2, line=3)
	lines (seq(0.5,size-0.5,1), pression, col="orange3")
	abline(h=meanPression, col="orange3",lty=2)
	box()
}

do_plot_single = function(data1) {
	size <<- length(data1$Date)
	
	plot_pluie(data1$Pluie)
	title("Pluie")
	
	plot_hygro(data1$Hygro)
	title("Hygrometrie")

	plot_temp(data1$Temp.max, data1$Temp.min)
	title("Temperature extérieure")
	#plot_temp(data1$Temp.int.max, data1$Temp.int.min)
	#title("Température intérieure")

	plot_pression(data1$Pression)
	title("Pression")
}

do_plot_multi = function(data1, data2) {
	size <- length(data1$Date)
	size <<- max(size, length(data2$Date))
	
	layout(matrix(1:2, 2, 1))
	plot_pluiehygro(data1$Pluie, data1$Hygro)
	title("Pluie et Hygrométrie (Revel)")
	plot_pluiehygro(data2$Pluie, data2$Hygro)
	title("(Martinique)")

	layout(matrix(1:3, 3, 1))
	plot_temp(data1$Temp.max, data1$Temp.min)
	title("Temperature extérieure (Revel)")
	#plot_temp(data1$Temp.int.max, data1$Temp.int.min)
	#title("Température intérieure (Revel)")
	plot_temp(data2$Temp.max, data2$Temp.min)
	title("(Martinique)")

	layout(matrix(1:2, 2, 1))
	plot_pression(data1$Pression)
	title("Pression (Revel)")
	plot_pression(data2$Pression)
	title("(Martinique)")
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
#fill_gaps_linear(meteoRevel$Pluie)

increase_to = function(lst, value){
	for (i in 1:length(lst)) {
		if (lst[i] < value)
			lst[i] <- lst[i] + value
		else 
			break
	}
	return(lst)
}
get_avg = function(lst1, lst2){
	average <- list()
	for (t in 1:size) {
		average <- c(average, (lst1[t] + lst1[t]) / 2)
	}
	return(average)
}

complete = function(lst, val, totalLength){
	diff <- totalLength - length(lst)
	if (diff == 0) {
		return(lst)
	} else {
		return(c(lst, seq(val, val, length=diff)))
	}
}

#do_plot_single(meteoBonnot)
do_plot_single(meteoGrenoble)

#do_plot_multi(meteoRevel, meteoBonnot)
