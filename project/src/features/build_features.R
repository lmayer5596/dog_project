#read in raw data
data <- fread('./project/volume/data/raw/data.csv')
format <- fread('./project/volume/data/raw/example_sub.csv')

#remove id column
data <- data[, !c('id')]

#save for modelling
fwrite(data, './project/volume/data/interim/data.csv')
fwrite(format, './project/volume/data/interim/submit.csv')
