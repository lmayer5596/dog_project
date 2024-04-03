data <- fread('./project/volume/data/raw/data.csv')
format <- fread('./project/volume/data/raw/example_sub.csv')
View(data)

data <- data[, !c('id')]



fwrite(data, './project/volume/data/interim/data.csv')
fwrite(format, './project/volume/data/interim/submit.csv')
