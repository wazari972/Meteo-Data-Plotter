#! /usr/bin/Rscript

meteoRevel <- read.csv(file="Revel.csv",head=TRUE,sep=";")
meteoMque <- read.csv(file="Martinique.csv",head=TRUE,sep=";")


plot_pluiehygro=function(pluie, hygro) {
	meanPluie <- mean(pluie)
	meanHygro <- mean(hygro)
	
	pluie <- complete(pluie, 0, size)
	hygro <- complete(hygro, meanHygro, size)

	par (mar=c(2, 4, 4, 4) + 0.1)
	barplot (pluie, space=0,  col='lightblue', ylab='Precipitations (mm)', axes=FALSE)
	grid (nx=size, ny=6, lty=1)

	abline(h=meanPluie, col="lightblue",lty=2)
	axis (2, at=seq(0,max(pluie),5))
	
	hygroRange <- range(hygro)
	plot.window(xlim=c(0,size), ylim=c(50,100), xaxs='i', yaxs='i')
	axis (4)
	mtext ('Hygro (%)', 4, line=3)
	lines (seq(from=0.5,to=size-0.5,by=1), hygro, col="seagreen4")
	abline(h=meanHygro, col="seagreen4",lty=2)
	box()
}

plot_temp=function(max, min) {
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
	grid (nx=size, ny=6, lty=1)
	mtext ('Temp (°C)', 2, line=3)
	lines (seq(0.5,size-0.5,1), max, col="tomato2")
	abline(h=meanMax, col="tomato2",lty=2)
	lines (seq(0.5,size-0.5,1), min, col="royalblue3")
	abline(h=meanMin, col="royalblue3",lty=2)
	lines (seq(0.5,size-0.5,1), average, col="orchid")
	box()
}

plot_pression=function(pression) {
	pression <- increase_to(pression, 1000)
	pressionRange <- range(max(pression)+1, min(pression)-1) 
	meanPression <- mean(pression)

	pression <- complete(pression, meanPression, size)
	plot(0, 0, xlim=c(0,size), ylim=pressionRange, xaxs='i', yaxs='i')
	axis (2)
	grid (nx=size, ny=6, lty=1)
	mtext ('Pression (hPa)', 2, line=3)
	lines (seq(0.5,size-0.5,1), pression, col="orange3")
	abline(h=meanPression, col="orange3",lty=2)
	box()
}

do_plot_single=function(data1) {
	size <<- length(data1$Date)
	
	plot_pluiehygro(data1$Pluie, data1$Hygro)
	title("Pluie et Hygrométrie")

	plot_temp(data1$Temp.max, data1$Temp.min)
	title("Temperature extérieure")
	#plot_temp(data1$Temp.int.max, data1$Temp.int.min)
	#title("Température intérieure")

	plot_pression(data1$Pression)
	title("Pression")
}

do_plot_multi=function(data1, data2) {
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

increase_to=function(lst, value){
	for (i in 1:length(lst)) {
		if (lst[i] < value)
			lst[i] <- lst[i] + value
		else 
			break
	}
	return(lst)
}
get_avg=function(lst1, lst2){
	average <- list()
	for (t in 1:size) {
		average <- c(average, (lst1[t] + lst1[t]) / 2)
	}
	return(average)
}
complete=function(lst, val, totalLength){
	diff <- totalLength - length(lst)
	if (diff == 0) {
		return(lst)
	} else {
		return(c(lst, seq(val, val, length=diff)))
	}
}

do_plot_single(meteoRevel)
#do_plot_multi(meteoRevel, meteoMque)
