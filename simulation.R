
# Simulation Function --------------------------------------------------------------

nhpp_sim <- function(start,end,trials,l_func){
  k=1 #index for getting arrival times
  Customers <- NA #total arrival count from t to s
  arrivalTimes <- NA #time of arrival
  
  j=1
  while(j<trials+1){#Runs defined number of simulations
    arrivalvalue=0
    arrivals <- NA #array of arrivals
    i = 1 #used for indexing
    t <- start
    s <- end
    
    while (t < s) {
      #lambda to be changed between options
      l_value <- l_func(t) #lambda value
      arrivalvalue<-rexp(1,rate=l_value)
      #Jumps by incriments of .01 seconds, if the arrival occured in that time frame, document it
      if(arrivalvalue > .01) t=t+.01 else t=t+arrivalvalue
      if(arrivalvalue > .01){
        arrivals[i] <- NA
      } else {
        arrivals[i] <- t
        if(j==1){
          arrivalTimes[k]<-t
          k<-k+1
        }
      }
      i <- i +1
    }#End while s<t
    
    #Count of number of arrivals in duration
    N=length(arrivals[!is.na(arrivals)])
    Customers[j] <- N
    j=j+1 #Move to next simulation (of 100)
  }#End while j<101
  return(Customers)
}
