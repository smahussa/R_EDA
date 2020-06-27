# See R_EDA_Report for details 

# 1: Working with one variable and Data cleaning 

library(ggplot2)
td <- travel_dataset
table(td$`Purpose of trip`)

td_subset <- subset(td, `Purpose of trip`=='Business Development (BDO Use Only)' | 
                      `Purpose of trip`== 'Continuous Learning' |
                      `Purpose of trip`== 'Corporate Priorities' | 
                      `Purpose of trip`== 'Delivery of Mandate' | 
                      `Purpose of trip`== 'General' | 
                      `Purpose of trip`== 'Internal Governance' |
                      `Purpose of trip`== 'Miscellaneous' | 
                      `Purpose of trip`== 'Other' | 
                      `Purpose of trip`== 'Policy and Programs')

table(td_subset$`Purpose of trip`)
by(td_subset$Total, td_subset$`Purpose of trip`, sum)
by(td_subset$Total, td_subset$`Purpose of trip`, summary)

qplot(x=`Purpose of trip`, data = td_subset) + scale_x_discrete(labels = c('Bus_Dev', 'Cont. Learning',
                                                                           'Corp Priorities', 'DoM', 
                                                                           'General', 'IG', 'Misc', 'Other',
                                                                           'Policy & Programs')) +
  scale_y_continuous(breaks = seq(0,5000, 500))


# 2- Correlation and regression. The variables first need to be converted into numeric/factorized to find correlation

td_subset2 <- td_subset
td_subset2$Region = as.numeric(td_subset2$Region)
td_subset2$`Purpose of trip` = as.numeric(td_subset2$`Purpose of trip`)
td_subset2$Month = as.numeric(td_subset2$Month)
td_subset2$Nights = as.numeric(td_subset2$Nights)
td_subset2$Total = as.numeric(td_subset2$Total)
cor_data <- td_subset2[, c(2,3,5,8,19)]
cor(cor_data)

model <- lm(Total ~ Region + Month + `Purpose of trip` + Nights, data = td_subset2)
summary(model)


# 3- Working with 2 variables: ‘Purpose of trip’ and ‘Total’. 
# The first graph has overplotting so I have jittered the graph to solve this issue.
# The quantile function below looks at 95% of the data

ggplot(aes(`Purpose of trip`, Total), data = td_subset) + geom_point() + 
  scale_x_discrete(labels = c('Bus_Dev', 'Cont. Learning',
                              'Corp Priorities', 'DoM', 
                              'General', 'IG', 'Misc', 'Other',
                              'Policy & Programs')) +
scale_y_continuous(breaks = seq(0,10000, 1000), limits = c(0,10000)) 

ggplot(aes(`Purpose of trip`, Total), data = td_subset) + 
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') + 
  scale_x_discrete(labels = c('Bus_Dev', 'Cont. Learning',
                              'Corp Priorities', 'DoM', 
                              'General', 'IG', 'Misc', 'Other',
                              'Policy & Programs')) +
  ylim(0, quantile(td_subset$Total, 0.95))

by(td_subset$Total, td_subset$Region, summary)
by(td_subset$Total, td_subset$Region, sum)


# 4-  Working with 3 variables: ‘Purpose of trip’ and ‘Total’ and facet over ‘Region’.

ggplot(aes(`Purpose of trip`, Total, color = Region), data = td_subset) + 
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') + 
  scale_x_discrete(labels = c('Bus_Dev', 'Cont. Learning',
                              'Corp Priorities', 'DoM', 
                              'General', 'IG', 'Misc', 'Other',
                              'Policy & Programs')) +
  ylim(0, quantile(td_subset$Total, 0.95))

ggplot(aes(Region, Total, color = `Purpose of trip`), data = td_subset) + 
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') +
  ylim(0, quantile(td_subset$Total, 0.95))

library(dplyr)


# 5-  Working with 4 variables: ‘Purpose of trip’ and ‘Total’ and facet over ‘Region’ and the 4 seasons in a year. 
# First, make a new variable for the 4 seasons based on the ‘Month’ variable. For instance, Months 1,2 and 3 would be in season 1. 
# Then plot the 4 variables

td_seasons <- td_subset %>%
  mutate(season = 
           ifelse(Month %in% c(1,2,3), "Season 1",
                  ifelse(Month %in% c(4,5,6), "Season 2",
                 ifelse(Month %in% c(7,8,9), "Season 3", "Season 4"))))

ggplot(aes(`Region`, Total, color = `Purpose of trip`), data = td_seasons) + 
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') + 
  facet_wrap(~season)

ggplot(aes(`Purpose of trip`, Total, color = Region), data = td_seasons) + 
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') + 
  facet_wrap(~season) +
  scale_x_discrete(labels = c('BD', 'CL',
                              'CP', 'DoM', 
                              'G', 'IG', 'M', 'O',
                              'P&P')) +
  ylim(0, quantile(td_subset$Total, 0.95))

table(td_subset$Region)





