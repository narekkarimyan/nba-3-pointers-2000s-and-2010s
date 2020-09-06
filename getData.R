
library(httr)
library(dplyr)

data <- data.frame()

getData <- function(numPages, season){
  data_func <- data.frame()
  url <- paste0("https://www.balldontlie.io/api/v1/stats?seasons[]=", season)
  for(i in 1:numPages){
    request <- GET(url, 
                   query = list(page =	i, per_page = 100, postseason = TRUE ))
    
    content <- content(request)
    temp <- content$data
    
    for(j in 1:length(temp)){
      temp[[j]]$game <- temp[[j]]$game$season
      temp[[j]]$player <- paste(temp[[j]]$player$first_name,temp[[j]]$player$last_name)
      temp[[j]]$team <- temp[[j]]$team$full_name
    }
    
    temp <- rbindlist(temp)
    data_func <- rbind(data_func, temp)
  }
  return(data_func)
}

data <- rbind(data , getData(17, 2000))
data <- rbind(data , getData(17, 2001))
data <- rbind(data , getData(22, 2002))
data <- rbind(data , getData(20, 2003))
data <- rbind(data , getData(21, 2004))
data <- rbind(data , getData(22, 2005))
data <- rbind(data , getData(19, 2006))
data <- rbind(data , getData(21, 2007))
data <- rbind(data , getData(21, 2008))
data <- rbind(data , getData(20, 2009))
data <- rbind(data , getData(20, 2010))
data <- rbind(data , getData(21, 2011))
data <- rbind(data , getData(23, 2012))
data <- rbind(data , getData(24, 2013))
data <- rbind(data , getData(22, 2014))
data <- rbind(data , getData(23, 2015))
data <- rbind(data , getData(21, 2016))
data <- rbind(data , getData(22, 2017))
data <- rbind(data , getData(21, 2018))

df <- data %>%
  filter(!is.na(ast))

write.csv(df, 'nba.csv')




