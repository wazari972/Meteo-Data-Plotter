files = c("Grenoble.csv", "Revel.csv", "Montbonnot.csv", "Martinique.csv")

meteoData <- new.env(hash=T, parent=emptyenv())

for (i in 1:length(files)) {
  filename <- files[i]
  meteoData[[filename]] <- read.csv(file=filename, head=TRUE, sep=";")
}