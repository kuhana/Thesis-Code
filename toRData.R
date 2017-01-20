loc = '/Users/hanaku/Documents/Thesis/Data/'
list = list.files(loc)

#f01001_1 = read.csv(file = "/Users/hanaku/Documents/Thesis/Data/NewRecoveredOriginPixel01001_1.csv", header = TRUE, sep = ',')


for(f in 1:length(list)){
#for(f in 1:1){ 
  file = read.csv(paste(loc, list[f],sep=""))
  name = paste0('f',substr(list[f],24,30))
  save(file, file = paste0('/Users/hanaku/Documents/Thesis/RData/',substr(list[f],24,30),'.RData'))
}