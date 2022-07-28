#'Converts individual depth and velocity measurements into discharge data
#'
#'@return Discharge in m^3/s
#'
#'@param location The distance from the origin for the depth/velocity measurements in meters
#'@param depth Depth in meters
#'@param velocity Water velocity in m/s
#'
#'@export


discharge<-function(location,depth,velocity){
  if(length(location)==1|all(is.na(depth))){return(as.numeric(NA))}

  depth<-depth[order(location)]
  velocity<-velocity[order(location)]
  location<-location[order(location)]

  if(location[1]!=0){
    location<-c(0,location)
    velocity<-c(0,velocity)
    depth<-c(0,depth)
  }

  lDiff<-diff(location)

  ma <- function(x, n = 2){stats::filter(x, rep(1 / n, n), sides = 2)}

  secWidth<-c(lDiff[1]/2,ma(lDiff)[1:(length(lDiff)-1)],lDiff[length(lDiff)]/2)

  q<-sum(secWidth*depth*velocity)

  return(q)
}
