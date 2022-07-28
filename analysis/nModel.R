model{

  # Abundance model

  for(i in 1:nSites){
    for(j in 1:nYears){
      for(a in 1:2){
        N[i,j,a] ~ dpois(lambda[i,j,a])
        log(lambda[i,j,a]) <- mu[a] + siteRan[i,a] + yearRan[j,a] + eps[i,j,a]
      }
    }
  }

  ## priors
  for(a in 1:2){
    mu[a] ~ dnorm(0, 0.01)      # overall intercept
    # trend[a] ~ dnorm(0, 0.01)   # linear trend

    for(i in 1:nSites){
      siteRan[i,a] ~ dnorm(0,tauSite[a])     # random site effects
    }
    tauSite[a] <- pow(sdSite[a], -2)
    sdSite[a] ~ dunif(0,2)
    sd2Site[a] <- pow(sdSite[a], 2)

    for (j in 1:nYears){
      yearRan[j,a] ~ dnorm(0, tauYear[a])  # Random year effect
    }
    tauYear[a] <- pow(sdYear[a], -2)
    sdYear[a] ~ dunif(0,2)
    sd2Year[a] <- pow(sdYear[a], 2)

    for(i in 1:nSites){
      for(j in 1:nYears){
        eps[i,j,a] ~ dnorm(0, tau[a])  # Over-dispersion
      }
    }
    tau[a] <- pow(sigma[a], -2)
    sigma[a] ~ dunif(0, 2)
    sigma2[a] <- pow(sigma[a], 2)
  }

  # Detection model

  for(i in 1:nSites){
    for(j in 1:nYears){
      for(a in 1:2){
        y[i,j,1,a] ~ dbin(p[i,j,a], N[i,j,a])
        y[i,j,2,a] ~ dbin(p[i,j,a], N[i,j,a]-y[i,j,1,a])
        y[i,j,3,a] ~ dbin(p[i,j,a], N[i,j,a]-y[i,j,1,a]-y[i,j,2,a])
        y[i,j,4,a] ~ dbin(p[i,j,a], N[i,j,a]-y[i,j,1,a]-y[i,j,2,a]-y[i,j,3,a])

        p[i,j,a] <- 1/(1 + exp(-lp.lim[i,j,a]))
        lp.lim[i,j,a] <- min(999, max(-999, lp[i,j,a]))
        lp[i,j,a] <- pMu[a] + pBeta*pCov[i,j] + pSiteRan[i,a]
      }
    }
  }

  ## priors
  pBeta ~ dnorm(0, 0.37)
  for(a in 1:2){
    pMean[a] <-1/(1+exp(-pMu[a]))
    pMu[a] ~ dnorm(0,0.37)


    for(i in 1:nSites){
      pSiteRan[i,a] ~ dnorm(0,tauPSite[a]) #random variation in detection at the site level
    }
    tauPSite[a] <- pow(sdPSite[a], -2)
    sdPSite[a] ~ dunif(0,1)
    sd2PSite[a] <- pow(sdPSite[a], 2)
  }
}
