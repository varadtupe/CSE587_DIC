#Part 3
#Krithika Krishnan
#Varad Tupe
#install.packages("twitteR")


##########################################################################################
#Tweet Collection
##########################################################################################
rm(list = ls())
getwd()
setwd("/Users/varadtupe/Documents/GitHub/CSE587_DIC/lab1/finalLab1/")

#Loading data from already captured tweets .rdata file
load("./data/Part3_2.RData")
library(ggmap)
library(twitteR)
setup_twitter_oauth("xxnT9Yq3j6YvATGT04E0C8ASH", "lVM3BFFTT8i1qjQhX9FM2CKdum6h8iVrCcZfqWntTk02oWm08M","57972043-A4xUZwhzLY9hCgwTRRYN2yytdRcTfayjQwELg8blm","NNivHGb0hEPjNCWwSogIUGSQ3SNPeLGnbL4gbyrxAhbE3")

#############################################
#Fetching Data
#############################################
searchStr = 'flu'
tdFlu = searchTwitter(searchStr, geocode='37.09024,-95.712891,1340mi',  n=8000, retryOnRateLimit=1)
tdFluDF = twListToDF(tdFlu)
mainFlu = rbind(mainFlu,tdFluDF)
#############################################


#############################################
searchStr = 'fluvirus'
tdFlu = searchTwitter(searchStr,  geocode='37.09024,-95.712891,1340mi', n=10000, retryOnRateLimit=1)
tdFluDF = twListToDF(tdFlu)
mainFlu = rbind(mainFlu,tdFluDF)
#############################################


#############################################
searchStr = 'fluseason'
tdFlu = searchTwitter(searchStr,  geocode='37.09024,-95.712891,1340mi', n=10000, retryOnRateLimit=1)
tdFluDF = twListToDF(tdFlu)
mainFlu = rbind(mainFlu,tdFluDF)
#############################################

#############################################
searchStr = 'flushot'
tdFlu = searchTwitter(searchStr, geocode='37.09024,-95.712891,1340mi',  n=10000, retryOnRateLimit=1)
tdFluDF = twListToDF(tdFlu)
mainFlu = rbind(mainFlu,tdFluDF)
#############################################


#############################################
searchStr = 'influenza'
tdFlu = searchTwitter(searchStr, geocode='37.09024,-95.712891,1340mi',  n=10000, retryOnRateLimit=1)
tdFluDF = twListToDF(tdFlu)
mainFlu = rbind(mainFlu,tdFluDF)
#############################################


###########################
#Check data with lat long with respect to USA

filteredData = unique(mainFlu[which(mainFlu$longitude != "NA"),])
filteredData = unique(mainFlu[which(mainFlu$latitude != "NA"),])

filteredData$latitude = as.numeric(filteredData$latitude)
filteredData$longitude = as.numeric(filteredData$longitude)


filteredData$statename = ""
filteredData = filteredData[which(filteredData$longitude > -124.7327),]
filteredData = filteredData[which(filteredData$longitude < -66.96927),]
filteredData = filteredData[which(filteredData$latitude > 23.51704),]
filteredData = filteredData[which(filteredData$latitude < 49.37173),]
filteredData$Seq = seq(1,129)

####################
#State assigner
for(i in 1:129){
  
  if(filteredData$statename[i] == ""){
  lt = as.numeric(filteredData$latitude[i])
  lng = as.numeric(filteredData$longitude[i])
  #print(lt)
  #print(lng)
  pos = c(lng ,lt)
  print("pos")
  print(pos)
  rgc = revgeocode(pos, output = "more")
  print(rgc)

  print(tolower(rgc$administrative_area_level_1))
  #if(rgc$country == "United States"){
  
  print(i)
  print(rgc$administrative_area_level_1)
  filteredData$statename[i] = tolower(rgc$administrative_area_level_1)
  }
  print("After")
  
}


save.image("~/Documents/GitHub/CSE587_DIC/lab1/Part3/Part3_2.RData")





############################################
#Grouping data based on tweets
stCount = data.frame(table(filteredData$statename))
stCount$Freq = as.numeric(stCount$Freq)
colnames(stCount) = c("statename" ,"Freq")
############################################
# convert to level factor
final = NULL
final = merge(stCountFinal, stCount, by="statename",all.x = TRUE)
final$Freq = as.numeric(final$Freq)
final[is.na(final$Freq),"Freq"] = 0
final$Lvl <- as.factor(as.numeric(cut(final$Freq,4,ordered = FALSE)))


levels(final$Lvl) = c("Minimal","Low","Moderate","High","Insufficient data")
final

final[which(final$Freq == as.numeric(0)),"Lvl"] = as.factor("Insufficient data")

#final[which(final$Freq == as.numeric(0)),"Lvl"] = 0
#final[which(is.na(final$Lvl)),] = as.factor(5)

##############################################
#Heat Map

library(maps)
library(mapproj)
library(fiftystater)


## to be added to display alaska & hawaii names 
# geom_text(data=cnames, aes(cnames$long, cnames$lat, label=cnames$state), size=2)


ggplot(final, aes(map_id = final$statename)) + 
  geom_map(aes(fill = final$Lvl),color = "black", map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  scale_fill_manual(values = c( "High" = "red",
                                "Moderate" = "orange",
                                "Low"="yellow",
                                "Minimal"='#99ff33',
                                "Insufficient data" = "white"),
                    #labels=c("High",  "Moderate","Low","Minimal", "Insufficient data"), drop = FALSE)+
                    labels=c("Minimal","Low","Moderate","High","Insufficient data"), drop = FALSE)+
                    
  coord_map("bonne", lat0 = 20)+labs(fill= "",
                                     title="Influenza based tweets collected during Feb 2018",x="",y="")+
  theme(panel.grid=element_blank())



save.image("./data/Part3_2.RData")



############################################