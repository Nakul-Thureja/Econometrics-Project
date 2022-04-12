#code for adding columns of yield_index and yield_index_rate in our dataset
#note: final.csv contains the modified dataset
library(dplyr)
library(ggplot2) 
library(sqldf)
library(tidyr)

#code for generating the dataset with yield_index and growth rate
data <- read.csv("C:/users/nakul/Desktop/data.csv")
data <- sqldf("select *, (select sum(D1.yield_area_cc_total)/sum(D1.area_cc_total) from data D1 
                 where D2.state = D1.state and D2.district = D1.district and D2.year = D1.year and D2.cropcategory = D1.cropcategory) 
      as yield_index from data D2" )

data <- sqldf("select *, (select ((D2.yield_index - D1.yield_index)/D1.yield_index) from data as D1 where D2.year- 1 = D1.year and D1.state = D2.state and D1.district = D2.district and D1.cropcategory = D2.cropcategory) as rate from data as D2")

write.csv(data,"C:/users/nakul/Desktop/final.csv")
