files = c("Grenoble.csv", "Revel.csv", "Montbonnot.csv", "Martinique.csv")

meteoData <- frame()

for (i in 1:length(files)) {
  filename <- files[i]
  data <- read.csv(file=filename, head=TRUE, sep=";")
  
  meteoData[[filename]] <- fix_data(data)
}

getDatasets <- function (filenames) {
  ret = list(NULL)
  
  if (length(filenames) == 0)
    return (ret)
  
  for(i in 1:length(filenames)) {
    ret[[i]] <- list(filenames[i], meteoData[[filenames[i]]])
  }
  
  return (ret)
}