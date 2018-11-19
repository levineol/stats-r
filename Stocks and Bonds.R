source("http://jgscott.github.io/teaching/r/mvnorm/rbvnorm.R")

mu_stocks = 0.065
mu_bonds = 0.017
sd_stocks = 0.195
sd_bonds = 0.075
rho = -0.15 

returns = rbvnorm(50, mu_stocks, mu_bonds, sd_stocks, sd_bonds, rho)
plot(returns)

Wealth = 10000
Horizon = 40
for(year in 1:Horizon) {
  return_stocks = rnorm(1, mu_stocks, sd_stocks)
  Wealth = Wealth * (1 + return_stocks)
}
Wealth

total_wealth = 10000
weights = c(0.6, 0.4) # how much of your wealth in each asset?
wealth_by_asset = total_wealth * weights

Horizon = 40
for(year in 1:Horizon) {
  # Simulate a bivariate normal set of returns
  returns = rbvnorm(1, mu_stocks, mu_bonds, sd_stocks, sd_bonds, rho)
  # Update wealth in each asset
  wealth_by_asset = wealth_by_asset * (1 + returns)
  # rebalance
  total_wealth = sum(wealth_by_asset)
  wealth_by_asset = total_wealth * weights
}
total_wealth
