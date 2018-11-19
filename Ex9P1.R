library(mosaic)
library(fImport) 
source("http://jgscott.github.io/teaching/r/mvnorm/computereturns.R")

mystocks = c("SPY", "TLT", "LQD", "DBC", "VNQ")
myprices = yahooSeries(mystocks, from='2007-04-16', to='2017-04-27', frequency = 'daily')
head(myprices)
myreturns = computereturns(myprices)
head(myreturns)

##### SCENARIO 1 #######
boot1 = do(200)*
  { 
  horizon = 20
  totalwealth = 10000
  pweights = c(0.5, 0.0, 0.0, 0.0, 0.5)
  holdings = pweights * totalwealth
  wealthtracker = rep(0, horizon) # Set up a placeholder to track total wealth
    for(today in 1:horizon) {
	    return.today = resample(myreturns, 1, orig.ids=FALSE)
	    holdings = holdings*(1+return.today)
	    totalwealth = sum(holdings)
	    wealthtracker[today] = totalwealth
      }
  VaR1= unname(quantile(wealthtracker, p=.05))
  VaR1
  }

hist(boot1$result)
abline(v=VaR1)

##### SCENARIO 2 #######
boot2 = do(200)*
  { 
  horizon = 20
  totalwealth = 10000
  pweights = c(0.0, 0.5, 0.0, 0.5, 0.0)
  holdings = pweights * totalwealth
  wealthtracker = rep(0, horizon) # Set up a placeholder to track total wealth
  for(today in 1:horizon) {
    return.today = resample(myreturns, 1, orig.ids=FALSE)
    holdings = holdings*(1+return.today)
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    }
  VaR2= unname(quantile(wealthtracker, p=.05))
  VaR2
  }

hist(boot2$result)
abline(v=VaR2)

##### SCENARIO 3 #######
boot3 = do(200)*
  { 
  horizon = 20
  totalwealth = 10000
  pweights = c(0.2, 0.2, 0.2, 0.2, 0.2)
  holdings = pweights * totalwealth
  wealthtracker = rep(0, horizon) # Set up a placeholder to track total wealth
  for(today in 1:horizon) {
    return.today = resample(myreturns, 1, orig.ids=FALSE)
    holdings = holdings*(1+return.today)
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    }
  VaR3= unname(quantile(wealthtracker, p=.05))
  VaR3
  }

hist(boot3$result)
abline(v=VaR3)
