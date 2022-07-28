model{
  #pass 1 overall mean and variance for detection probability
  p1Mean~dnorm(0,0.01)
  p1Eps~dunif(0,10)
  p1Tau<-1/pow(p1Eps,2)


  # for(s in 1:nSpecies){
  #   for(r in 1:3){
  #     logitP[s,r]~dnorm(0,0.01)
  #   }
  # }
  #

  #each species p is drawn from distribution for pass 1, p2 is predicted from p1 with error, and p3 is predicted from p2 with error
  for(s in 1:nSpecies){
    logitP[s,1]~dnorm(p1Mean,p1Tau)
    logitP[s,2]~dnorm(p2Beta0+p2Beta1*logitP[s,1],p2Eps)
    # logitP[s,2]<-p2Beta0+p2Beta1*logitP[s,1]
    logitP[s,3]~dnorm(p3Beta0+p3Beta1*logitP[s,1],p3Eps)
    # logitP[s,3]<-p3Beta0+p3Beta1*logitP[s,1]
  }

  p2Beta0~dnorm(0,0.01)
  p2Beta1~dnorm(0,0.01)
  p2Eps~dunif(0,10)
  p2Tau<-1/pow(p2Eps,2)

  p3Beta0~dnorm(0,0.01)
  p3Beta1~dnorm(0,0.01)
  p3Eps~dunif(0,10)
  p3Tau<-1/pow(p3Eps,2)

  for(s in 1:nSpecies){
    for(r in 1:3){
      # logitP[s,r]<-beta0[s]+beta1[s]*(run[r]-1) #substract one so that the intercept is p for run 1
      p[s,r]<-1/(1+exp(-logitP[s,r]))
    }
  }

  for(i in 1:nRows){
    obs[i]~dbern(p[species[i],run[i]])
  }

}
