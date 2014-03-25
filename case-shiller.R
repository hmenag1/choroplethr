library(Quandl)
library(choroplethr)
Quandl.auth("XDGdKyHASYM3sp28RFdj")

## Getting data from Quandl ##

# example for how to read Quandl data to a df:
# df <- Quandl("FRED/AKSTHPI")

states <- state.abb
state_dfs <- list()

for (i in 1:length(state.abb))
{
  state = state.abb[i]
  print(state)
  q_code = paste0('FRED/', state, 'STHPI')
  state_dfs[[i]] = Quandl(q_code)
  state_dfs[[i]]$state = state
}

df = data.frame(Date=as.Date(character()), Value=numeric(), state=character())
for (i in 1:length(state_dfs)) {
	df <- rbind(df, state_dfs[[i]])
}

library(plyr)

choropleths = list()
dates = sort(unique(df$Date))
values = df$Value
probs=c(12.5,25,37.5,50,62.5,75,87.5,100) # equally sized buckets over the entire dataset
probs = probs/100
breaks = quantile(values, probs)
breaks = c(0, breaks)

for (i in 1:length(dates))
{
  map = df[df$Date == dates[i], ]
  map = rename(map, replace=c("state"="region", "Value"="value"))
  map$value = cut2(map$value, breaks)
  choropleths[[i]] = choroplethr(map, "state", 9, title=dates[i])
}

choroplethr_animate(choropleths)
