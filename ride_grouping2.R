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
xpixel= -1182
ypixel = -891
dep_pixel = pTrips[(pTrips$OXCoord==xpixel) & (pTrips$OYCoord==ypixel), ] 

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

temp_vTrips = data.frame("OFIPS"=numeric(),"OXCoord"=numeric(),"OYCoord"=numeric(), "ODepartureTime"=numeric(), "CDcount"=numeric(), "Trip1"=integer(), "DXCoord1"=numeric(), "DYCoord1"=numeric(), "Riders1"=integer(), "Trip2"=integer(), "DXCoord2"=numeric(), "DYCoord2"=numeric(), "Riders2"=integer(), "Trip3"=integer(), "DXCoord3"=numeric(), "DYCoord3"=numeric(), "Riders3"=integer(), "TotalRiders" = integer(), "vMiles" = numeric(), "pMiles" = numeric(), "AVO" = numeric(), "DFIPS" = numeric(), "DXCoord" = numeric(), "DYCoord" = numeric(), "DArrivalTime" = numeric(), "DFIPS1" = numeric(), "DFIPS2" = numeric(), "DFIPS3" = numeric())
newTrip = rep(0,28)

cdist <- function(x1, y1, x2, y2){
  sqrt((x2-x1)^2 + (y2-y1)^2)*.5
}
tour2 <- function(ax, ay, bx, by, oa, ob){
  ab = cdist(ax, ay, bx, by)
  vDist = min(oa+ab, ob+ab)
  min_tour = which.min(cbind(oa+ab, ob+ab))
  if((min_tour==1) & (vDist <= MC * ob)){
    result = cbind("ab", vDist, ax, ay, bx, by)
  }
  else if((min_tour==2) & (vDist <= MC * oa)){
    result = cbind("ba", vDist, bx, by, ax, ay)
  }
  else{
    result = cbind("null",NA)
  }
  result
}
tour3 <- function(ax, ay, bx, by, cx, cy, oa, ob, oc){
  ab = cdist(ax, ay, bx, by) 
  ac = cdist(ax, ay, cx, cy) 
  bc = cdist(bx, by, cx, cy) 
  
  vDist = min(oa+ab+bc, 
               oa+ac+bc,
               ob+ab+ac,
               ob+bc+ac,
               oc+ac+ab,
               oc+bc+ab)
  
  min_tour = which.min(cbind(oa+ab+bc, oa+ac+bc,ob+ab+ac,ob+bc+ac,oc+ac+ab, oc+bc+ab))
  pMiles = oa+ob+oc
  
  if((min_tour == 1) & (oa+ab <= MC * ob) & (vDist <= MC * oc)){
    result = c("abc", vDist, ax, ay, bx, by, cx, cy)
  }
  else if((min_tour ==2) & (oa+ac <= MC * oc) & (vDist <= MC * ob)){
    result = c("acb", vDist, ax, ay,cx, cy, bx, by)
  }
  else if((min_tour ==3) & (ob+ab <= MC * oa) & (vDist <= MC * oc)){
    result = c("bac", vDist, bx, by, ax, ay, cx, cy)
  }
  else if((min_tour ==4) & (ob+bc <= MC * oc) & (vDist <= MC * oa)){
    result = c("bca", vDist, bx, by, cx, cy, ax, ay)
  }
  else if((min_tour ==5) & (oc+ac <= MC * oa) & (vDist <= MC * ob)){
    result = c("cab", vDist, cx, cy,ax, ay, bx, by)
  }
  else if((min_tour ==6) & (oc+bc <= MC * ob) & (vDist <= MC * oa)){
    result = c("cba", vDist, cx, cy, bx, by, ax, ay)
  }
  else{
    result = c("null", NA)
  }
  result
}
addTrip <- function(dep){
  result = newTrip
  result[1] = dep[6]
  result[2] = dep[9]
  result[3] = dep[10]
  result[4] = dep[11] + DD
  result[6] = 1
  result[7] = as.numeric(dep[17])
  result[8] = as.numeric(dep[18])
  result[9] = 1
  result[19] = cdist(as.numeric(result[2]), as.numeric(result[3]), as.numeric(result[7]), as.numeric(result[8]))
  result[20] = dep[19]
  result[26] = dep[14]
  result
}

ptm <- proc.time()
##NEED TO FILL OUT DFIPS
for(i in 1:nrow(dep_pixel)){
#for(i in 1:70) { #through dep_pixel
  #check if time as expired, and if so, publish vehicle tours
  #CAN OPTIMIZE, DONT NEED TO RUN THROUGH ENTIRE FOR LOOP? 
  j=1
  while(j <= nrow(temp_vTrips)){
    if(dep_pixel$ODepartureTime[i] > temp_vTrips$ODepartureTime[j]){
      currentCD =  max(temp_vTrips$Trip1[j], temp_vTrips$Trip2[j], temp_vTrips$Trip3[j])
      if(currentCD ==1){
        temp_vTrips$CDcount[j] = 1
        temp_vTrips$DXCoord[j] = temp_vTrips$DXCoord1[j]
        temp_vTrips$DYCoord[j] = temp_vTrips$DYCoord1[j]
        temp_vTrips$DFIPS[j] = temp_vTrips$DFIPS1[j]
      }
      if(currentCD ==2){
        temp_vTrips$CDcount[j] = 2
        temp_vTrips$DXCoord[j] = temp_vTrips$DXCoord2[j]
        temp_vTrips$DYCoord[j] = temp_vTrips$DYCoord2[j]
        temp_vTrips$DFIPS[j] = temp_vTrips$DFIPS2[j]
      }
      if(currentCD ==3){
        temp_vTrips$CDcount[j] = 3
        temp_vTrips$DXCoord[j] = temp_vTrips$DXCoord3[j]
        temp_vTrips$DYCoord[j] = temp_vTrips$DYCoord3[j]
        temp_vTrips$DFIPS[j] = temp_vTrips$DFIPS3[j]
      }
        
      temp_vTrips$CDcount[j] = currentCD
      temp_vTrips$TotalRiders[j] = temp_vTrips$Riders1[j] + temp_vTrips$Riders2[j] + temp_vTrips$Riders3[j]
      temp_vTrips$DArrivalTime[j] = temp_vTrips$ODepartureTime[j] + temp_vTrips$vMiles[j] * (1/speed)
      temp_vTrips$AVO[j] = temp_vTrips$pMiles[j] / temp_vTrips$vMiles[j]
      vTrips[nrow(vTrips)+1,] = temp_vTrips[j,1:25]
      temp_vTrips = temp_vTrips[-c(j),]
    }
    else{
      j= j+1
    }
  }

  if(nrow(temp_vTrips)==0){
    temp_vTrips[nrow(temp_vTrips)+1,] = newTrip
    temp_vTrips[nrow(temp_vTrips),] = addTrip(dep_pixel[i,])
  }
     
  else{ 
    flag = TRUE
    #if dest already listed, increment riders
    for(row in 1:nrow(temp_vTrips)){    
      currentCD =  max(temp_vTrips$Trip1[row], temp_vTrips$Trip2[row], temp_vTrips$Trip3[row])
      
      if(dep_pixel$DXCoord[i] == temp_vTrips$DXCoord1[row] && dep_pixel$DYCoord[i] == temp_vTrips$DYCoord1[row]){
        temp_vTrips$Riders1[row] = temp_vTrips$Riders1[row] + 1
        temp_vTrips$pMiles[row] = temp_vTrips$pMiles[row] + dep_pixel$GCDistance[i]
        flag = FALSE
        }
      else if((currentCD == 2 || currentCD == 3) && (dep_pixel$DXCoord[i] == temp_vTrips$DXCoord2[row] && dep_pixel$DYCoord[i] == temp_vTrips$DYCoord2[row])){
        temp_vTrips$Riders2[row] = temp_vTrips$Riders2[row] + 1
        temp_vTrips$pMiles[row] = temp_vTrips$pMiles[row] + dep_pixel$GCDistance[i]
        flag = FALSE
        }
      else if(currentCD == 3 && dep_pixel$DXCoord[i] == temp_vTrips$DXCoord3[row] && dep_pixel$DYCoord[i] == temp_vTrips$DYCoord3[row]){
        temp_vTrips$Riders3[row] = temp_vTrips$Riders3[row] + 1
        temp_vTrips$pMiles[row] = temp_vTrips$pMiles[row] + dep_pixel$GCDistance[i]
        flag = FALSE
      }
      if(flag == FALSE)
        break
      }
    
    # dest not found, add as a new destination
    if(flag == TRUE){
      for(j in 1:nrow(temp_vTrips)){ 
        currentCD =  max(temp_vTrips$Trip1[j], temp_vTrips$Trip2[j], temp_vTrips$Trip3[j])
        #add a 3rd dest
        if(currentCD == 2){
          oa = cdist(temp_vTrips$OXCoord[j], temp_vTrips$OYCoord[j], temp_vTrips$DXCoord1[j], temp_vTrips$DYCoord1[j])
          ob = cdist(temp_vTrips$OXCoord[j], temp_vTrips$OYCoord[j], temp_vTrips$DXCoord2[j], temp_vTrips$DYCoord2[j])
          oc = cdist(temp_vTrips$OXCoord[j], temp_vTrips$OYCoord[j], dep_pixel$DXCoord[i], dep_pixel$DYCoord[i])
          route = tour3(temp_vTrips$DXCoord1[j], temp_vTrips$DYCoord1[j], temp_vTrips$DXCoord2[j], temp_vTrips$DYCoord2[j], dep_pixel$DXCoord[i], dep_pixel$DYCoord[i], oa, ob, oc)
          if(route[1]!='null'){
            temp_vTrips$Trip3[j] = 3
            temp_vTrips$vMiles[j] = as.numeric(route[2])
            temp_vTrips$pMiles[j] = temp_vTrips$pMiles[j] + dep_pixel$GCDistance[i]
            if(route[1]=='abc'){
              temp_vTrips$Riders3[j] = 1
              temp_vTrips$DXCoord3[j] = as.numeric(route[7])
              temp_vTrips$DYCoord3[j] = as.numeric(route[8])
              temp_vTrips$DFIPS3[j] = dep_pixel$DFIPS[i]
              }
            if(route[1]=='acb'){
              temp_vTrips$Riders3[j] = temp_vTrips$Riders2[j]
              temp_vTrips$Riders2[j] = 1
              temp_vTrips$DXCoord2[j] = as.numeric(route[5])
              temp_vTrips$DYCoord2[j] = as.numeric(route[6])
              temp_vTrips$DXCoord3[j] = as.numeric(route[7])
              temp_vTrips$DYCoord3[j] = as.numeric(route[8])
              temp_vTrips$DFIPS3[j] = temp_vTrips$DFIPS2[j]
              temp_vTrips$DFIPS2[j] = dep_pixel$DFIPS[i] 
              }
            if(route[1]=='bac'){
              tempR = temp_vTrips$Riders1[j]
              temp_vTrips$Riders1[j] = temp_vTrips$Riders2[j]
              temp_vTrips$Riders2[j] = tempR
              temp_vTrips$Riders3[j] = 1
              temp_vTrips$DXCoord1[j] = as.numeric(route[3])
              temp_vTrips$DYCoord1[j] = as.numeric(route[4])
              temp_vTrips$DXCoord2[j] = as.numeric(route[5])
              temp_vTrips$DYCoord2[j] = as.numeric(route[6])
              temp_vTrips$DXCoord3[j] = as.numeric(route[7])
              temp_vTrips$DYCoord3[j] = as.numeric(route[8])
              tempDFIPS = temp_vTrips$DFIPS1[j]
              temp_vTrips$DFIPS1[j] = temp_vTrips$DFIPS2[j]
              temp_vTrips$DFIPS3[j] = dep_pixel$DFIPS[i]
              temp_vTrips$DFIPS2[j] = tempDFIPS
              }
            if(route[1]=='bca'){
              temp_vTrips$Riders3[j] = temp_vTrips$Riders1[j]
              temp_vTrips$Riders1[j] = temp_vTrips$Riders2[j]
              temp_vTrips$Riders2[j] = 1
              temp_vTrips$DXCoord1[j] = as.numeric(route[3])
              temp_vTrips$DYCoord1[j] = as.numeric(route[4])
              temp_vTrips$DXCoord2[j] = as.numeric(route[5])
              temp_vTrips$DYCoord2[j] = as.numeric(route[6])
              temp_vTrips$DXCoord3[j] = as.numeric(route[7])
              temp_vTrips$DYCoord3[j] = as.numeric(route[8])
              temp_vTrips$DFIPS3[j] = temp_vTrips$DFIPS1[j]
              temp_vTrips$DFIPS1[j] = temp_vTrips$DFIPS2[j]
              temp_vTrips$DFIPS2[j] = dep_pixel$DFIPS[i]
              }
            if(route[1]=='cab'){
              temp_vTrips$Riders3[j] = temp_vTrips$Riders2[j]
              temp_vTrips$Riders2[j] = temp_vTrips$Riders1[j]
              temp_vTrips$Riders1[j] = 1
              temp_vTrips$DXCoord1[j] = as.numeric(route[3])
              temp_vTrips$DYCoord1[j] = as.numeric(route[4])
              temp_vTrips$DXCoord2[j] = as.numeric(route[5])
              temp_vTrips$DYCoord2[j] = as.numeric(route[6])
              temp_vTrips$DXCoord3[j] = as.numeric(route[7])
              temp_vTrips$DYCoord3[j] = as.numeric(route[8])
              temp_vTrips$DFIPS3[j] = temp_vTrips$DFIPS2[j]
              temp_vTrips$DFIPS2[j] = temp_vTrips$DFIPS1[j]
              temp_vTrips$DFIPS1[j] = dep_pixel$DFIPS[i] 
              }
            if(route[1]=='cba'){
              temp_vTrips$Riders3[j] = temp_vTrips$Riders1[j]
              temp_vTrips$Riders1[j] = 1
              temp_vTrips$DXCoord1[j] = as.numeric(route[3])
              temp_vTrips$DYCoord1[j] = as.numeric(route[4])
              temp_vTrips$DXCoord3[j] = as.numeric(route[7])
              temp_vTrips$DYCoord3[j] = as.numeric(route[8])
              temp_vTrips$DFIPS3[j] = temp_vTrips$DFIPS1[j]
              temp_vTrips$DFIPS1[j] = dep_pixel$DFIPS[i]
              }
            flag = FALSE
          }
        }
        if(flag == FALSE)
          break
        
        #add a 2nd dest
        else if(currentCD == 1){
          oa = cdist(temp_vTrips$OXCoord[j], temp_vTrips$OYCoord[j], temp_vTrips$DXCoord1[j], temp_vTrips$DYCoord1[j])
          ob = cdist(temp_vTrips$OXCoord[j], temp_vTrips$OYCoord[j], dep_pixel$DXCoord[i], dep_pixel$DYCoord[i])
          route = tour2(temp_vTrips$DXCoord1[j], temp_vTrips$DYCoord1[j], dep_pixel$DXCoord[i], dep_pixel$DYCoord[i], oa, ob)
          if(route[1]!='null'){
            temp_vTrips$Trip2[j] = 2
            temp_vTrips$vMiles[j] = as.numeric(route[2])
            temp_vTrips$pMiles[j] = temp_vTrips$pMiles[j] + dep_pixel$GCDistance[i]
            if(route[1]=='ab'){
              temp_vTrips$Riders2[j] = 1
              temp_vTrips$DXCoord2[j] = as.numeric(route[5])
              temp_vTrips$DYCoord2[j] = as.numeric(route[6])
              temp_vTrips$DFIPS2[j] = dep_pixel$DFIPS[i]
              }
            if(route[1] == 'ba'){
              temp_vTrips$Riders2[j] = temp_vTrips$Riders1[j]
              temp_vTrips$Riders1[j] = 1
              temp_vTrips$DXCoord2[j] = as.numeric(route[5])
              temp_vTrips$DYCoord2[j] = as.numeric(route[6])
              temp_vTrips$DXCoord1[j] = as.numeric(route[3])
              temp_vTrips$DYCoord1[j] = as.numeric(route[4])
              temp_vTrips$DFIPS2[j] = temp_vTrips$DFIPS1[j]
              temp_vTrips$DFIPS1[j] = dep_pixel$DFIPS[i]
            }
            flag = FALSE
          }
        }
        if(flag == FALSE)
          break
      }
    }
    
    # dest not found, cannot be add to an existing trip
    if(flag == TRUE){
      temp_vTrips[nrow(temp_vTrips)+1,] = newTrip
      temp_vTrips[nrow(temp_vTrips),] = addTrip(dep_pixel[i,])
    }
  }
}

proc.time() - ptm
