The Meteo-Data-Plotter project is a R application showing the weather data I collected during a few years.

It's main interface is a [Shiny reactive website](http://www.rstudio.com/shiny/), as illustrated below.
It can also export PDF, through R native interface.

Source data [are formatted in CSV](https://raw.github.com/wazari972/Meteo-Data-Plotter/master/Grenoble.csv) and look like that:

Date|Temp.min|Temp.max|Hygrometrie|Pluie|Pression|Commentaires
----|--------|--------|-----------|-----|--------|------------
04/09/11|18|21|89|20|25|
05/09/11|15|20|80|40|25|Heavy rainfalls
06/09/11|12|22|62|0|24|
07/09/11|14|21|67|0|24|
08/09/11|15|23|68|0|25|

The graph part is neat and ready to use. Next step is to a "textual" information, like the day/month it most rained, 
temperature peak, etc.

For the graphs, possible improvements would be date selection or a better way to compare two file 
(currently they're just plotted one on top of the other).


![Meteo-Data-Plotter web interface](https://raw.github.com/wazari972/Meteo-Data-Plotter/master/web/Meteo.png)
