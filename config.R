files = c("Grenoble.csv", "Revel.csv", "Montbonnot.csv", "Martinique.csv")

meteoData <- frame()

for (i in 1:length(files)) {
  filename <- files[i]
  data <- read.csv(file=filename, head=TRUE, sep=";")
  
  meteoData[[filename]] <- fix_data(data)
}