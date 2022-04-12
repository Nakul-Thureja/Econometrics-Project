library(ggplot2) 
library(sqldf)

data <- read.csv("C:/Users/nakul/Desktop/final.csv")

production <- data$productiontonnes*1000
area <- data$areahectares*2.47105
data$yield_kg <- production/area

data$yield_area <- data$yield_area*1000
data$yield_area_cc_total <- data$yield_area_cc_total*1000
data$area_cc_total <- data$area_cc_total*2.47105

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Mean,Median,Mode,Standard Deviation for dependent variable Sepsis 
dataSepsis <- subset(data,Sepsis != 'NA')
dataSepsis <- sqldf("select * from dataSepsis group by sdyid ") 
mean(dataSepsis$Sepsis)
median(dataSepsis$Sepsis)
mode(dataSepsis$Sepsis)
sd(dataSepsis$Sepsis)

#Mean,Median,Mode,Standard Deviation for dependent variable LBW 
dataLBW <- subset(data,LBW != 'NA')
dataLBW <- sqldf("select * from dataLBW group by sdyid") 
mean(dataLBW$LBW)
median(dataLBW$LBW)
mode(dataLBW$LBW)
sd(dataLBW$LBW)

#Mean,Median,Mode,Standard Deviation for dependent variable Pneumonia 
dataPneumonia <- subset(data,Pneumonia != 'NA')
dataPneumonia <- sqldf("select * from dataPneumonia group by sdyid") 
mean(dataPneumonia$Pneumonia)
median(dataPneumonia$Pneumonia)
mode(dataPneumonia$Pneumonia)
sd(dataPneumonia$Pneumonia)

#Mean,Median,Mode,Standard Deviation for dependent variable Diarrhea 
dataDiarrhea <- subset(data,Diarrhea != 'NA')
dataDiarrhea <- sqldf("select * from dataDiarrhea group by sdyid") 
mean(dataDiarrhea$Diarrhea)
median(dataDiarrhea$Diarrhea)
mode(dataDiarrhea$Diarrhea)
sd(dataDiarrhea$Diarrhea)

#Mean,Median,Mode,Standard Deviation for dependent variable Fever 
dataFever <- subset(data,Fever != 'NA')
dataFever <- sqldf("select * from dataFever group by sdyid") 
mean(dataFever$Fever)
median(dataFever$Fever)
mode(dataFever$Fever)
sd(dataFever$Fever)

#Mean,Median,Mode,Standard Deviation for dependent variable Measles 
dataMeasles <- subset(data,Measles != 'NA')
dataMeasles <- sqldf("select * from dataMeasles group by sdyid") 
mean(dataMeasles$Measles)
median(dataMeasles$Measles)
mode(dataMeasles$Measles)
sd(dataMeasles$Measles)

#Removing outliers from Data
dataSepsis <- subset(data,Sepsis != 'NA')
dataSepsis <- subset(dataSepsis,Sepsis <= 31.5)
dataLBW <- subset(data,LBW != 'NA')
dataLBW <- subset(dataLBW,LBW <= 60)
dataPneumonia <- subset(data,Pneumonia != 'NA')
dataPneumonia <- subset(dataPneumonia,Pneumonia <= 43)
dataDiarrhea <- subset(data,Diarrhea != 'NA')
dataDiarrhea <- subset(dataDiarrhea,Diarrhea <= 24)
dataFever <- subset(data,Fever != 'NA')
dataFever <- subset(dataFever,Fever <= 34)
dataMeasles <- subset(data,Measles != 'NA')
dataMeasles <- subset(dataMeasles,Measles <= 8.8)

#code for season-wise density plot (histogram) for dependent variable Sepsis

hk<- subset(dataSepsis, season == "Kharif")
hr <-subset(dataSepsis, season == "Rabi")
hs<- subset(dataSepsis, season == "Summer")
hw <-subset(dataSepsis, season == "Whole Year")


hk$s <- 'kharif'
hr$s <- 'rabi'
hs$s <- 'summer'
hw$s <- 'whole year'

newSepsis <- rbind(hk,hr,hs,hw)

ggplot(newSepsis, aes(x=Sepsis, fill = season))+geom_density(alpha = 0.2)

#code for season-wise density plot (histogram) for dependent variable LBW
hk<- subset(dataLBW, season == "Kharif")
hr <-subset(dataLBW, season == "Rabi")
hs<- subset(dataLBW, season == "Summer")
hw <-subset(dataLBW, season == "Whole Year")


hk$s <- 'kharif'
hr$s <- 'rabi'
hs$s <- 'summer'
hw$s <- 'whole year'

newLBW <- rbind(hk,hr,hs,hw)

ggplot(newLBW, aes(x=LBW, fill = season))+geom_density(alpha = 0.2)

#code for season-wise density plot (histogram) for dependent variable Pneumonia
hk<- subset(dataPneumonia, season == "Kharif")
hr <-subset(dataPneumonia, season == "Rabi")
hs<- subset(dataPneumonia, season == "Summer")
hw <-subset(dataPneumonia, season == "Whole Year")


hk$s <- 'kharif'
hr$s <- 'rabi'
hs$s <- 'summer'
hw$s <- 'whole year'

newPneumonia <- rbind(hk,hr,hs,hw)

ggplot(newPneumonia, aes(x=Pneumonia, fill = season))+geom_density(alpha = 0.2)

#code for season-wise density plot (histogram) for dependent variable Diarrhea
hk<- subset(dataDiarrhea, season == "Kharif")
hr <-subset(dataDiarrhea, season == "Rabi")
hs<- subset(dataDiarrhea, season == "Summer")
hw <-subset(dataDiarrhea, season == "Whole Year")


hk$s <- 'kharif'
hr$s <- 'rabi'
hs$s <- 'summer'
hw$s <- 'whole year'

newDiarrhea <- rbind(hk,hr,hs,hw)

ggplot(newDiarrhea, aes(x=Diarrhea, fill = season))+geom_density(alpha = 0.2)

#code for season-wise density plot (histogram) for dependent variable Fever
hk<- subset(dataFever, season == "Kharif")
hr <-subset(dataFever, season == "Rabi")
hs<- subset(dataFever, season == "Summer")
hw <-subset(dataFever, season == "Whole Year")


hk$s <- 'kharif'
hr$s <- 'rabi'
hs$s <- 'summer'
hw$s <- 'whole year'

newFever <- rbind(hk,hr,hs,hw)

ggplot(newFever, aes(x=Fever, fill = season))+geom_density(alpha = 0.2)

#code for season-wise density plot (histogram) for dependent variable Measles
hk<- subset(dataMeasles, season == "Kharif")
hr <-subset(dataMeasles, season == "Rabi")
hs<- subset(dataMeasles, season == "Summer")
hw <-subset(dataMeasles, season == "Whole Year")


hk$s <- 'kharif'
hr$s <- 'rabi'
hs$s <- 'summer'
hw$s <- 'whole year'

newMeasles <- rbind(hk,hr,hs,hw)

ggplot(newMeasles, aes(x=Measles, fill = season))+geom_density(alpha = 0.2)

#code for year-wise density plot (histogram) for dependent variable Sepsis
h2011 <- subset(dataSepsis, year == "2011")
h2012 <- subset(dataSepsis, year == "2012")
h2013 <- subset(dataSepsis, year == "2013")
h2014 <- subset(dataSepsis, year == "2014")
h2015 <- subset(dataSepsis, year == "2015")
h2016 <- subset(dataSepsis, year == "2016")


h2011$yr <- '2011'
h2012$yr <- '2012'
h2013$yr <- '2013'
h2014$yr <- '2014'
h2015$yr <- '2015'
h2016$yr <- '2016'


newSepsis <- rbind(h2011,h2012,h2013,h2014,h2015,h2016)

ggplot(newSepsis, aes(x=Sepsis, fill = yr))+geom_density(alpha = 0.2)

#code for year-wise density plot (histogram) for dependent variable LBW
h2011 <- subset(dataLBW, year == "2011")
h2012 <- subset(dataLBW, year == "2012")
h2013 <- subset(dataLBW, year == "2013")
h2014 <- subset(dataLBW, year == "2014")
h2015 <- subset(dataLBW, year == "2015")
h2016 <- subset(dataLBW, year == "2016")


h2011$yr <- '2011'
h2012$yr <- '2012'
h2013$yr <- '2013'
h2014$yr <- '2014'
h2015$yr <- '2015'
h2016$yr <- '2016'


newLBW <- rbind(h2011,h2012,h2013,h2014,h2015,h2016)

ggplot(newLBW, aes(x=LBW, fill = yr))+geom_density(alpha = 0.2)

#code for year-wise density plot (histogram) for dependent variable Pneumonia
h2011 <- subset(dataPneumonia, year == "2011")
h2012 <- subset(dataPneumonia, year == "2012")
h2013 <- subset(dataPneumonia, year == "2013")
h2014 <- subset(dataPneumonia, year == "2014")
h2015 <- subset(dataPneumonia, year == "2015")
h2016 <- subset(dataPneumonia, year == "2016")


h2011$yr <- '2011'
h2012$yr <- '2012'
h2013$yr <- '2013'
h2014$yr <- '2014'
h2015$yr <- '2015'
h2016$yr <- '2016'


newPneumonia <- rbind(h2011,h2012,h2013,h2014,h2015,h2016)

ggplot(newPneumonia, aes(x=Pneumonia, fill = yr))+geom_density(alpha = 0.2)

#code for year-wise density plot (histogram) for dependent variable Diarrhea
h2011 <- subset(dataDiarrhea, year == "2011")
h2012 <- subset(dataDiarrhea, year == "2012")
h2013 <- subset(dataDiarrhea, year == "2013")
h2014 <- subset(dataDiarrhea, year == "2014")
h2015 <- subset(dataDiarrhea, year == "2015")
h2016 <- subset(dataDiarrhea, year == "2016")


h2011$yr <- '2011'
h2012$yr <- '2012'
h2013$yr <- '2013'
h2014$yr <- '2014'
h2015$yr <- '2015'
h2016$yr <- '2016'


newDiarrhea <- rbind(h2011,h2012,h2013,h2014,h2015,h2016)

ggplot(newDiarrhea, aes(x=Diarrhea, fill = yr))+geom_density(alpha = 0.2)

#code for year-wise density plot (histogram) for dependent variable Fever
h2011 <- subset(dataFever, year == "2011")
h2012 <- subset(dataFever, year == "2012")
h2013 <- subset(dataFever, year == "2013")
h2014 <- subset(dataFever, year == "2014")
h2015 <- subset(dataFever, year == "2015")
h2016 <- subset(dataFever, year == "2016")


h2011$yr <- '2011'
h2012$yr <- '2012'
h2013$yr <- '2013'
h2014$yr <- '2014'
h2015$yr <- '2015'
h2016$yr <- '2016'


newFever <- rbind(h2011,h2012,h2013,h2014,h2015,h2016)

ggplot(newFever, aes(x=Fever, fill = yr))+geom_density(alpha = 0.2)

#code for year-wise density plot (histogram) for dependent variable Measles
h2011 <- subset(dataMeasles, year == "2011")
h2012 <- subset(dataMeasles, year == "2012")
h2013 <- subset(dataMeasles, year == "2013")
h2014 <- subset(dataMeasles, year == "2014")
h2015 <- subset(dataMeasles, year == "2015")
h2016 <- subset(dataMeasles, year == "2016")


h2011$yr <- '2011'
h2012$yr <- '2012'
h2013$yr <- '2013'
h2014$yr <- '2014'
h2015$yr <- '2015'
h2016$yr <- '2016'


newMeasles <- rbind(h2011,h2012,h2013,h2014,h2015,h2016)

ggplot(newMeasles, aes(x=Measles, fill = yr))+geom_density(alpha = 0.2)

#code for correlation coefficient of dependent variable Sepsis
#correlation between dependent variable Sepsis and Gdp,Tap and Beds respectively
cor(dataSepsis$Sepsis,dataSepsis$gdp, use = "complete.obs")
cor(dataSepsis$Sepsis,dataSepsis$tap, use = "complete.obs")
cor(dataSepsis$Sepsis,dataSepsis$beds, use = "complete.obs")

#correlation between dependent variable Sepsis and Yield Index for each of the six crop categories
cashcrop <- subset(dataSepsis,cropcategory=="Cash")
pulsescrop <- subset(dataSepsis,cropcategory=="Pulse")
cerealcrop <- subset(dataSepsis,cropcategory=="Cereal")
horticulturecrop <- subset(dataSepsis,cropcategory=="Horticulture")
oilseedscrop <- subset(dataSepsis,cropcategory=="Oilseed")
coarsecrop <- subset(dataSepsis,cropcategory=="Coarse Cereal")

cor(cashcrop$Sepsis,cashcrop$yield_index, use = "complete.obs")
cor(pulsescrop$Sepsis,pulsescrop$yield_index, use = "complete.obs")
cor(cerealcrop$Sepsis,cerealcrop$yield_index, use = "complete.obs")
cor(horticulturecrop$Sepsis,horticulturecrop$yield_index, use = "complete.obs")
cor(oilseedscrop$Sepsis,oilseedscrop$yield_index, use = "complete.obs")
cor(coarsecrop$Sepsis,coarsecrop$yield_index, use = "complete.obs")

#correlation between dependent variable Sepsis and Yield Index growth rate for each of the six crop categories
cashcrop <- subset(dataSepsis,cropcategory=="Cash")
pulsescrop <- subset(dataSepsis,cropcategory=="Pulse")
cerealcrop <- subset(dataSepsis,cropcategory=="Cereal")
horticulturecrop <- subset(dataSepsis,cropcategory=="Horticulture")
oilseedscrop <- subset(dataSepsis,cropcategory=="Oilseed")
coarsecrop <- subset(dataSepsis,cropcategory=="Coarse Cereal")

cor(cashcrop$Sepsis,cashcrop$rate, use = "complete.obs")
cor(pulsescrop$Sepsis,pulsescrop$rate, use = "complete.obs")
cor(cerealcrop$Sepsis,cerealcrop$rate, use = "complete.obs")
cor(horticulturecrop$Sepsis,horticulturecrop$rate, use = "complete.obs")
cor(oilseedscrop$Sepsis,oilseedscrop$rate, use = "complete.obs")
cor(coarsecrop$Sepsis,coarsecrop$rate, use = "complete.obs")

#code for correlation coefficient of dependent variable LBW
#correlation between dependent variable LBW and Gdp,Tap and Beds respectively
dataLBW <- subset(data,LBW != 'NA')
dataLBW <- subset(dataLBW,LBW <= 60)
dataLBW <- sqldf("select * from dataLBW group by sdyid,cropcategory") 

cor(dataLBW$LBW,dataLBW$gdp, use = "complete.obs")
cor(dataLBW$LBW,dataLBW$tap, use = "complete.obs")
cor(dataLBW$LBW,dataLBW$beds, use = "complete.obs")

#correlation between dependent variable LBW and Yield Index for each of the six crop categories
cashcrop <- subset(dataLBW,cropcategory=="Cash")
pulsescrop <- subset(dataLBW,cropcategory=="Pulse")
cerealcrop <- subset(dataLBW,cropcategory=="Cereal")
horticulturecrop <- subset(dataLBW,cropcategory=="Horticulture")
oilseedscrop <- subset(dataLBW,cropcategory=="Oilseed")
coarsecrop <- subset(dataLBW,cropcategory=="Coarse Cereal")

cor(cashcrop$LBW,cashcrop$yield_index, use = "complete.obs")
cor(pulsescrop$LBW,pulsescrop$yield_index, use = "complete.obs")
cor(cerealcrop$LBW,cerealcrop$yield_index, use = "complete.obs")
cor(horticulturecrop$LBW,horticulturecrop$yield_index, use = "complete.obs")
cor(oilseedscrop$LBW,oilseedscrop$yield_index, use = "complete.obs")
cor(coarsecrop$LBW,coarsecrop$yield_index, use = "complete.obs")

#correlation between dependent variable LBW and Yield Index growth rate for each of the six crop categories
cashcrop <- subset(dataLBW,cropcategory=="Cash")
pulsescrop <- subset(dataLBW,cropcategory=="Pulse")
cerealcrop <- subset(dataLBW,cropcategory=="Cereal")
horticulturecrop <- subset(dataLBW,cropcategory=="Horticulture")
oilseedscrop <- subset(dataLBW,cropcategory=="Oilseed")
coarsecrop <- subset(dataLBW,cropcategory=="Coarse Cereal")

cor(cashcrop$LBW,cashcrop$rate, use = "complete.obs")
cor(pulsescrop$LBW,pulsescrop$rate, use = "complete.obs")
cor(cerealcrop$LBW,cerealcrop$rate, use = "complete.obs")
cor(horticulturecrop$LBW,horticulturecrop$rate, use = "complete.obs")
cor(oilseedscrop$LBW,oilseedscrop$rate, use = "complete.obs")
cor(coarsecrop$LBW,coarsecrop$rate, use = "complete.obs")

#code for correlation coefficient of dependent variable Pneumonia
#correlation between dependent variable Pneumonia and Gdp,Tap and Beds respectively
dataPneumonia <- subset(data,Pneumonia != 'NA')
dataPneumonia <- subset(dataPneumonia,Pneumonia <= 43)
dataPneumonia <- sqldf("select * from dataPneumonia group by sdyid,cropcategory") 

cor(dataPneumonia$Pneumonia,dataPneumonia$gdp, use = "complete.obs")
cor(dataPneumonia$Pneumonia,dataPneumonia$tap, use = "complete.obs")
cor(dataPneumonia$Pneumonia,dataPneumonia$beds, use = "complete.obs")

#correlation between dependent variable Pneumonia and Yield Index for each of the six crop categories
cashcrop <- subset(dataPneumonia,cropcategory=="Cash")
pulsescrop <- subset(dataPneumonia,cropcategory=="Pulse")
cerealcrop <- subset(dataPneumonia,cropcategory=="Cereal")
horticulturecrop <- subset(dataPneumonia,cropcategory=="Horticulture")
oilseedscrop <- subset(dataPneumonia,cropcategory=="Oilseed")
coarsecrop <- subset(dataPneumonia,cropcategory=="Coarse Cereal")

cor(cashcrop$Pneumonia,cashcrop$yield_index, use = "complete.obs")
cor(pulsescrop$Pneumonia,pulsescrop$yield_index, use = "complete.obs")
cor(cerealcrop$Pneumonia,cerealcrop$yield_index, use = "complete.obs")
cor(horticulturecrop$Pneumonia,horticulturecrop$yield_index, use = "complete.obs")
cor(oilseedscrop$Pneumonia,oilseedscrop$yield_index, use = "complete.obs")
cor(coarsecrop$Pneumonia,coarsecrop$yield_index, use = "complete.obs")

#correlation between dependent variable Pneumonia and Yield Index growth rate for each of the six crop categories
cashcrop <- subset(dataPneumonia,cropcategory=="Cash")
pulsescrop <- subset(dataPneumonia,cropcategory=="Pulse")
cerealcrop <- subset(dataPneumonia,cropcategory=="Cereal")
horticulturecrop <- subset(dataPneumonia,cropcategory=="Horticulture")
oilseedscrop <- subset(dataPneumonia,cropcategory=="Oilseed")
coarsecrop <- subset(dataPneumonia,cropcategory=="Coarse Cereal")

cor(cashcrop$Pneumonia,cashcrop$rate, use = "complete.obs")
cor(pulsescrop$Pneumonia,pulsescrop$rate, use = "complete.obs")
cor(cerealcrop$Pneumonia,cerealcrop$rate, use = "complete.obs")
cor(horticulturecrop$Pneumonia,horticulturecrop$rate, use = "complete.obs")
cor(oilseedscrop$Pneumonia,oilseedscrop$rate, use = "complete.obs")
cor(coarsecrop$Pneumonia,coarsecrop$rate, use = "complete.obs")

#code for correlation coefficient of dependent variable Diarrhea
#correlation between dependent variable Diarrhea and Gdp,Tap and Beds respectively
dataDiarrhea <- subset(data,Diarrhea != 'NA')
dataDiarrhea <- subset(dataDiarrhea,Diarrhea <= 24)
dataDiarrhea <- sqldf("select * from dataDiarrhea group by sdyid,cropcategory") 

cor(dataDiarrhea$Diarrhea,dataDiarrhea$gdp, use = "complete.obs")
cor(dataDiarrhea$Diarrhea,dataDiarrhea$tap, use = "complete.obs")
cor(dataDiarrhea$Diarrhea,dataDiarrhea$beds, use = "complete.obs")

#correlation between dependent variable Diarrhea and Yield Index for each of the six crop categories
cashcrop <- subset(dataDiarrhea,cropcategory=="Cash")
pulsescrop <- subset(dataDiarrhea,cropcategory=="Pulse")
cerealcrop <- subset(dataDiarrhea,cropcategory=="Cereal")
horticulturecrop <- subset(dataDiarrhea,cropcategory=="Horticulture")
oilseedscrop <- subset(dataDiarrhea,cropcategory=="Oilseed")
coarsecrop <- subset(dataDiarrhea,cropcategory=="Coarse Cereal")

cor(cashcrop$Diarrhea,cashcrop$yield_index, use = "complete.obs")
cor(pulsescrop$Diarrhea,pulsescrop$yield_index, use = "complete.obs")
cor(cerealcrop$Diarrhea,cerealcrop$yield_index, use = "complete.obs")
cor(horticulturecrop$Diarrhea,horticulturecrop$yield_index, use = "complete.obs")
cor(oilseedscrop$Diarrhea,oilseedscrop$yield_index, use = "complete.obs")
cor(coarsecrop$Diarrhea,coarsecrop$yield_index, use = "complete.obs")

#correlation between dependent variable Diarrhea and Yield Index growth rate for each of the six crop categories
cashcrop <- subset(dataDiarrhea,cropcategory=="Cash")
pulsescrop <- subset(dataDiarrhea,cropcategory=="Pulse")
cerealcrop <- subset(dataDiarrhea,cropcategory=="Cereal")
horticulturecrop <- subset(dataDiarrhea,cropcategory=="Horticulture")
oilseedscrop <- subset(dataDiarrhea,cropcategory=="Oilseed")
coarsecrop <- subset(dataDiarrhea,cropcategory=="Coarse Cereal")

cor(cashcrop$Diarrhea,cashcrop$rate, use = "complete.obs")
cor(pulsescrop$Diarrhea,pulsescrop$rate, use = "complete.obs")
cor(cerealcrop$Diarrhea,cerealcrop$rate, use = "complete.obs")
cor(horticulturecrop$Diarrhea,horticulturecrop$rate, use = "complete.obs")
cor(oilseedscrop$Diarrhea,oilseedscrop$rate, use = "complete.obs")
cor(coarsecrop$Diarrhea,coarsecrop$rate, use = "complete.obs")

#code for correlation coefficient of dependent variable Fever
#correlation between dependent variable Fever and Gdp,Tap and Beds respectively
dataFever <- subset(data,Fever != 'NA')
dataFever <- subset(dataFever,Fever <= 34)
dataFever <- sqldf("select * from dataFever group by sdyid,cropcategory") 

cor(dataFever$Fever,dataFever$gdp, use = "complete.obs")
cor(dataFever$Fever,dataFever$tap, use = "complete.obs")
cor(dataFever$Fever,dataFever$beds, use = "complete.obs")

#correlation between dependent variable Fever and Yield Index for each of the six crop categories
cashcrop <- subset(dataFever,cropcategory=="Cash")
pulsescrop <- subset(dataFever,cropcategory=="Pulse")
cerealcrop <- subset(dataFever,cropcategory=="Cereal")
horticulturecrop <- subset(dataFever,cropcategory=="Horticulture")
oilseedscrop <- subset(dataFever,cropcategory=="Oilseed")
coarsecrop <- subset(dataFever,cropcategory=="Coarse Cereal")

cor(cashcrop$Fever,cashcrop$yield_index, use = "complete.obs")
cor(pulsescrop$Fever,pulsescrop$yield_index, use = "complete.obs")
cor(cerealcrop$Fever,cerealcrop$yield_index, use = "complete.obs")
cor(horticulturecrop$Fever,horticulturecrop$yield_index, use = "complete.obs")
cor(oilseedscrop$Fever,oilseedscrop$yield_index, use = "complete.obs")
cor(coarsecrop$Fever,coarsecrop$yield_index, use = "complete.obs")

#correlation between dependent variable Fever and Yield Index growth rate for each of the six crop categories
cashcrop <- subset(dataFever,cropcategory=="Cash")
pulsescrop <- subset(dataFever,cropcategory=="Pulse")
cerealcrop <- subset(dataFever,cropcategory=="Cereal")
horticulturecrop <- subset(dataFever,cropcategory=="Horticulture")
oilseedscrop <- subset(dataFever,cropcategory=="Oilseed")
coarsecrop <- subset(dataFever,cropcategory=="Coarse Cereal")

cor(cashcrop$Fever,cashcrop$rate, use = "complete.obs")
cor(pulsescrop$Fever,pulsescrop$rate, use = "complete.obs")
cor(cerealcrop$Fever,cerealcrop$rate, use = "complete.obs")
cor(horticulturecrop$Fever,horticulturecrop$rate, use = "complete.obs")
cor(oilseedscrop$Fever,oilseedscrop$rate, use = "complete.obs")
cor(coarsecrop$Fever,coarsecrop$rate, use = "complete.obs")

#code for correlation coefficient of dependent variable Measles
#correlation between dependent variable Measles and Gdp,Tap and Beds respectively
dataMeasles <- subset(data,Measles != 'NA')
dataMeasles <- subset(dataMeasles,Measles <= 8.8)
dataMeasles <- sqldf("select * from dataMeasles group by sdyid,cropcategory") 

cor(dataMeasles$Measles,dataMeasles$gdp, use = "complete.obs")
cor(dataMeasles$Measles,dataMeasles$tap, use = "complete.obs")
cor(dataMeasles$Measles,dataMeasles$beds, use = "complete.obs")

#correlation between dependent variable Measles and Yield Index for each of the six crop categories
cashcrop <- subset(dataMeasles,cropcategory=="Cash")
pulsescrop <- subset(dataMeasles,cropcategory=="Pulse")
cerealcrop <- subset(dataMeasles,cropcategory=="Cereal")
horticulturecrop <- subset(dataMeasles,cropcategory=="Horticulture")
oilseedscrop <- subset(dataMeasles,cropcategory=="Oilseed")
coarsecrop <- subset(dataMeasles,cropcategory=="Coarse Cereal")

cor(cashcrop$Measles,cashcrop$yield_index, use = "complete.obs")
cor(pulsescrop$Measles,pulsescrop$yield_index, use = "complete.obs")
cor(cerealcrop$Measles,cerealcrop$yield_index, use = "complete.obs")
cor(horticulturecrop$Measles,horticulturecrop$yield_index, use = "complete.obs")
cor(oilseedscrop$Measles,oilseedscrop$yield_index, use = "complete.obs")
cor(coarsecrop$Measles,coarsecrop$yield_index, use = "complete.obs")

#correlation between dependent variable Measles and Yield Index growth rate for each of the six crop categories
cashcrop <- subset(dataMeasles,cropcategory=="Cash")
pulsescrop <- subset(dataMeasles,cropcategory=="Pulse")
cerealcrop <- subset(dataMeasles,cropcategory=="Cereal")
horticulturecrop <- subset(dataMeasles,cropcategory=="Horticulture")
oilseedscrop <- subset(dataMeasles,cropcategory=="Oilseed")
coarsecrop <- subset(dataMeasles,cropcategory=="Coarse Cereal")

cor(cashcrop$Measles,cashcrop$rate, use = "complete.obs")
cor(pulsescrop$Measles,pulsescrop$rate, use = "complete.obs")
cor(cerealcrop$Measles,cerealcrop$rate, use = "complete.obs")
cor(horticulturecrop$Measles,horticulturecrop$rate, use = "complete.obs")
cor(oilseedscrop$Measles,oilseedscrop$rate, use = "complete.obs")
cor(coarsecrop$Measles,coarsecrop$rate, use = "complete.obs")

#Regression for dependent variable Fever
#Code for Regression Part A 
dataFever <- subset(data,Fever != 'NA')
dataFever <- subset(dataFever,Fever <= 34)
dataFever <- sqldf("select * from dataFever group by sdyid") 
regression1 <- lm(Fever ~ gdp + tap + beds, dataFever)
print(summary(regression1))

#Subsets for CROP CATEGORIES
dataFever <- subset(data,Fever != 'NA')
dataFever <- subset(dataFever,Fever <= 34)
dataFever <- sqldf("select * from dataFever group by sdyid,cropcategory") 
cashcrop <- subset(dataFever,cropcategory=="Cash")
pulsescrop <- subset(dataFever,cropcategory=="Pulse")
cerealcrop <- subset(dataFever,cropcategory=="Cereal")
horticulturecrop <- subset(dataFever,cropcategory=="Horticulture")
oilseedscrop <- subset(dataFever,cropcategory=="Oilseed")
coarsecrop <- subset(dataFever,cropcategory=="Coarse Cereal")

#Code for Regression Part B
regression2 <- lm(Fever ~ gdp + tap + beds + yield_index, cashcrop)
print(summary(regression2))

regression3 <- lm(Fever ~ gdp + tap + beds + yield_index, pulsescrop)
print(summary(regression3))

regression4 <- lm(Fever ~ gdp + tap + beds + yield_index, cerealcrop)
print(summary(regression4))

regression5 <- lm(Fever ~ gdp + tap + beds + yield_index, horticulturecrop)
print(summary(regression5))

regression6 <- lm(Fever ~ gdp + tap + beds + yield_index, oilseedscrop)
print(summary(regression6))

regression7 <- lm(Fever ~ gdp + tap + beds + yield_index, coarsecrop)
print(summary(regression7))

#Code for Regression Part C
dataFeverYield <- dataFever
dataFeverYield <- transform(dataFeverYield, cash= ifelse(dataFeverYield$cropcategory == "Cash",yield_index,0))
dataFeverYield <- transform(dataFeverYield, pulses= ifelse(dataFeverYield$cropcategory == "Pulse",yield_index,0))
dataFeverYield <- transform(dataFeverYield, cereals= ifelse(dataFeverYield$cropcategory == "Cereal",yield_index,0))
dataFeverYield <- transform(dataFeverYield, ccereals= ifelse(dataFeverYield$cropcategory == "Coarse Cereal",yield_index,0))
dataFeverYield <- transform(dataFeverYield, horticulture= ifelse(dataFeverYield$cropcategory == "Horticulture",yield_index,0))
dataFeverYield <- transform(dataFeverYield, oilseed= ifelse(dataFeverYield$cropcategory == "Oilseed",yield_index,0))


regression8 <- lm(Fever ~ gdp + tap + beds + cash + pulses + cereals + 
                    ccereals + horticulture + oilseed, dataFeverYield)
print(summary(regression8))

#Code for Regression Part D
regression10 <- lm(Fever ~ gdp + tap + beds + rate, cashcrop)
print(summary(regression10))

regression11 <- lm(Fever ~ gdp + tap + beds + rate, pulsescrop)
print(summary(regression11))

regression12 <- lm(Fever ~ gdp + tap + beds + rate, cerealcrop)
print(summary(regression12))

regression13 <- lm(Fever ~ gdp + tap + beds + rate, horticulturecrop)
print(summary(regression13))

regression14 <- lm(Fever ~ gdp + tap + beds + rate, oilseedscrop)
print(summary(regression4))

regression15 <- lm(Fever ~ gdp + tap + beds + rate, coarsecrop)
print(summary(regression15))

#Code for Regression Part E
dataFeverRate <- dataFever
dataFeverRate <- transform(dataFeverYield, cash= ifelse(dataFeverYield$cropcategory == "Cash",rate,0))
dataFeverRate <- transform(dataFeverYield, pulses= ifelse(dataFeverYield$cropcategory == "Pulse",rate,0))
dataFeverRate <- transform(dataFeverYield, cereals= ifelse(dataFeverYield$cropcategory == "Cereal",rate,0))
dataFeverRate <- transform(dataFeverYield, ccereals= ifelse(dataFeverYield$cropcategory == "Coarse Cereal",rate,0))
dataFeverRate <- transform(dataFeverYield, horticulture= ifelse(dataFeverYield$cropcategory == "Horticulture",rate,0))
dataFeverRate <- transform(dataFeverYield, oilseed= ifelse(dataFeverYield$cropcategory == "Oilseed",rate,0))


regression16 <- lm(Fever ~ gdp + tap + beds + cash + pulses + cereals + 
                     ccereals + horticulture + oilseed, dataFeverRate)
print(summary(regression16))

#REMOVING 0 VALUES FOR LOG
cashcrop <- subset(dataFever,cropcategory=="Cash")
cashcrop <- subset(cashcrop,gdp!=0)
cashcrop <- subset(cashcrop,tap!=0)
cashcrop <- subset(cashcrop,beds!=0)
cashcrop <- subset(cashcrop,yield_index!=0)

pulsescrop <- subset(dataFever,cropcategory=="Pulse")
pulsescrop <- subset(pulsescrop,gdp!=0)
pulsescrop <- subset(pulsescrop,tap!=0)
pulsescrop <- subset(pulsescrop,beds!=0)
pulsescrop <- subset(pulsescrop,yield_index!=0)

cerealcrop <- subset(dataFever,cropcategory=="Cereal")
cerealcrop <- subset(cerealcrop,gdp!=0)
cerealcrop <- subset(cerealcrop,tap!=0)
cerealcrop <- subset(cerealcrop,beds!=0)
cerealcrop <- subset(cerealcrop,yield_index!=0)

horticulturecrop <- subset(dataFever,cropcategory=="Horticulture")
horticulturecrop <- subset(horticulturecrop,gdp!=0)
horticulturecrop <- subset(horticulturecrop,tap!=0)
horticulturecrop <- subset(horticulturecrop,beds!=0)
horticulturecrop <- subset(horticulturecrop,yield_index!=0)

oilseedscrop <- subset(dataFever,cropcategory=="Oilseed")
oilseedscrop <- subset(oilseedscrop,gdp!=0)
oilseedscrop <- subset(oilseedscrop,tap!=0)
oilseedscrop <- subset(oilseedscrop,beds!=0)
oilseedscrop <- subset(oilseedscrop,yield_index!=0)

coarsecrop <- subset(dataFever,cropcategory=="Coarse Cereal")
coarsecrop <- subset(coarsecrop,gdp!=0)
coarsecrop <- subset(coarsecrop,tap!=0)
coarsecrop <- subset(coarsecrop,beds!=0)
coarsecrop <- subset(coarsecrop,yield_index!=0)

#Code for Regression Part F
regression17 <- lm(Fever ~ log(gdp) + log(tap) + log(beds) + log(yield_index), cashcrop)
print(summary(regression17))

regression18 <- lm(Fever ~ log(gdp) + log(tap) + log(beds) + log(yield_index), cerealcrop)
print(summary(regression18))

regression19 <- lm(Fever ~ log(gdp) + log(tap) + log(beds) + log(yield_index), horticulturecrop)
print(summary(regression19))

regression20 <- lm(Fever ~ log(gdp) + log(tap) + log(beds) + log(yield_index), pulsescrop)
print(summary(regression20))

regression21 <- lm(Fever ~ log(gdp) + log(tap) + log(beds) + log(yield_index), oilseedscrop)
print(summary(regression21))

regression22 <- lm(Fever ~ log(gdp) + log(tap) + log(beds) + log(yield_index), coarsecrop)
print(summary(regression22))

#Code for Regression Part G
dataFeverLogYield <- dataFever
dataFeverLogYield <- subset(dataFeverLogYield,gdp!=0)
dataFeverLogYield <- subset(dataFeverLogYield,tap!=0)
dataFeverLogYield <- subset(dataFeverLogYield,beds!=0)
dataFeverLogYield <- subset(dataFeverLogYield,yield_index!=0)

dataFeverLogYield <- transform(dataFeverLogYield, cash = ifelse(dataFeverLogYield$cropcategory == "Cash",log(yield_index),0))
dataFeverLogYield <- transform(dataFeverLogYield, pulses = ifelse(dataFeverLogYield$cropcategory == "Pulse",log(yield_index),0))
dataFeverLogYield <- transform(dataFeverLogYield, cereals = ifelse(dataFeverLogYield$cropcategory == "Cereal",log(yield_index),0))
dataFeverLogYield <- transform(dataFeverLogYield, ccereals = ifelse(dataFeverLogYield$cropcategory == "Coarse Cereal",log(yield_index),0))
dataFeverLogYield <- transform(dataFeverLogYield, horticulture = ifelse(dataFeverLogYield$cropcategory == "Horticulture",log(yield_index),0))
dataFeverLogYield <- transform(dataFeverLogYield, oilseed = ifelse(dataFeverLogYield$cropcategory == "Oilseed",log(yield_index),0))

regression23 <- lm(Fever ~ log(gdp) + log(tap) + log(beds) + cash + pulses + cereals + 
                     ccereals + horticulture + oilseed, dataFeverLogYield)
print(summary(regression23))

