# NORMAL aTAXI OPERATION
# Great Circle Distance (GCD) > 2mi
library(fields)

# ANALYSIS PARAMETERS
DD = 300 #seconds
CD = 3 #trips     *if this changes, change vTrips initialization*
MC = 1.25 #%
speed = 30 #mph

speed = speed/(60*60) #miles per second
maxTime = 60*60*24 #seconds in a day 
  
#data from http://orf467.princeton.edu/NationWideTrips'16/NationWidePersonTrips_oPixel_oTime_SubCounty/
#personTrips
#f01001_1 = read.csv(file = "/Users/hanaku/Documents/Thesis/Data/NewRecoveredOriginPixel01001_1.csv", header = TRUE, sep = ',')
#save(f01001_1, file = "/Users/hanaku/Documents/Thesis/RData/f01001_1.Rdata")
load(file = "/Users/hanaku/Documents/Thesis/RData/f01001_1.Rdata")

pTrips = f01001_1[f01001_1$GCDistance >2,]
pTrips = pTrips[order(pTrips$ODepartureTime),]

min_OXCoord = min(pTrips$OXCoord)
min_OYCoord = min(pTrips$OYCoord)
max_OXCoord = max(pTrips$OXCoord)
max_OYCoord = max(pTrips$OYCoord)

# create vTrips file
vTrips = data.frame("OFIPS"=numeric(),"OXCoord"=numeric(),"OYCoord"=numeric(), "ODepartureTime"=numeric(), "CDcount"=numeric(), "Trip1"=integer(), "DXCoord1"=numeric(), "DYCoord1"=numeric(), "Riders1"=integer(), "Trip2"=integer(), "DXCoord2"=numeric(), "DYCoord2"=numeric(), "Riders2"=integer(), "Trip3"=integer(), "DXCoord3"=numeric(), "DYCoord3"=numeric(), "Riders3"=integer(), "TotalRiders" = integer(), "vMiles" = numeric(), "pMiles" = numeric(), "AVO" = numeric(), "DFIPS" = numeric(), "DXCoord" = numeric(), "DYCoord" = numeric(), "DArrivalTime" = numeric())
newTrip = rep(0,25)
n = 1  #n-th vehicle trip


#GCD
#rdist.earth.vec(testx1, testx2, miles = TRUE, R = 3958.75587)

#go through pTrips pixel by pixel
for(ypixel in min_OYCoord:max_OYCoord){
  for(xpixel in min_OXCoord:max_OXCoord){
    if(nrow(pTrips[(pTrips$OXCoord==xpixel) & (pTrips$OYCoord==ypixel), ]) > 0){
      # departures for a pixel
      xpixel= -1182
      ypixel = -891
      dep_pixel = pTrips[(pTrips$OXCoord==xpixel) & (pTrips$OYCoord==ypixel), ] 
      #dep_pixel[,"done"] <- NA
      
      #ride grouping

      
      
      
      
  
      }
    }
  }