## code to prepare `yield_data` dataset goes here
library(tidyverse)
yield_data <- read.csv('data-raw/yield_data.csv', header = T, col.names = c('Rank', 'ID', 'Name', 'Mint', 'Image', 'Total Yield'))
yield_data$Rank <- as.numeric(yield_data$Rank)
yield_data$Total.Yield <- as.numeric(yield_data$Total.Yield)
yield_data <- yield_data %>% mutate(type = ifelse(str_detect(Name, 'SolStein'), 'Solstein', 'QT'))
yield_data <- yield_data %>% mutate(Initial.Airdrop = ifelse(str_detect(Name, 'SolStein'), 500, 2000))
yield_data <- yield_data %>% mutate(Staking.Total = Total.Yield-Initial.Airdrop)


usethis::use_data(yield_data, overwrite = TRUE)




x=aggregate(list(Total.Yield = yield_data$Total.Yield,Initial.Airdrop = yield_data$Initial.Airdrop, Staking.Total=yield_data$Staking.Total),by = list(type = yield_data$type),  sum, drop = T)
reshape2::melt(x) %>% 
  ggplot(aes(x = variable, y = value, fill = type))+
  geom_bar(stat = 'identity')
subset(yield_data, subset = Name %in% c("SolStein #9805"))
