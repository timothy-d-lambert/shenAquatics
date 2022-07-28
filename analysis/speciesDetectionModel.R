model{
  for(s in 1:nSpecies){
    beta0[s]~dnorm(beta0Mean,beta0Tau)
    beta1[s]~dnorm(beta1Mean,beta1Tau)
  }

  beta0Mean~dnorm(0,0.01)
  beta0Eps~dunif(0,10)
  beta0Tau<-1/pow(beta0Eps,2)

  beta1Mean~dnorm(0,0.01)
  beta1Eps~dunif(0,10)
  beta1Tau<-1/pow(beta1Eps,2)

  for(s in 1:nSpecies){
    for(r in 1:3){
      logitP[s,r]<-beta0[s]+beta1[s]*(run[r]-1) #substract one so that the intercept is p for run 1
      p[s,r]<-1/(1+exp(-logitP[s,r]))
    }
  }

  for(i in 1:nRows){
    obs[i]~dbern(p[species[i],run[i]])
  }

}
