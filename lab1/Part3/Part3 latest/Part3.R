#Part 3
#install.packages("twitteR")


##########################################################################################
#Main
##########################################################################################
rm(list = ls())
getwd()
setwd("/Users/varadtupe/Documents/GitHub/CSE587_DIC/lab1/Part3")
load("~/Documents/GitHub/CSE587_DIC/lab1/Part3/Part3_2.RData")
library(ggmap)
library(twitteR)
setup_twitter_oauth("xxnT9Yq3j6YvATGT04E0C8ASH", "lVM3BFFTT8i1qjQhX9FM2CKdum6h8iVrCcZfqWntTk02oWm08M","57972043-A4xUZwhzLY9hCgwTRRYN2yytdRcTfayjQwELg8blm","NNivHGb0hEPjNCWwSogIUGSQ3SNPeLGnbL4gbyrxAhbE3")


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
#Check data with lat long

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

for(i in 40:129){
  
  if(filteredData$statename[i] == ""){
  lt = as.numeric(filteredData$latitude[i])
  lng = as.numeric(filteredData$longitude[i])
  #print(lt)
  #print(lng)
  pos = c(lng ,lt)
  print("pos")
  print(pos)
  rgc = revgeocode(pos, output = "more")

  print(tolower(rgc$administrative_area_level_1))
  #if(rgc$country == "United States"){
  
  print(i)
  print(rgc$administrative_area_level_1)
  filteredData$statename[i] = tolower(rgc$administrative_area_level_1)
  }
  print("After")
  
}





##################
library(googleway)
gc <- as.numeric(geocode('Baylor University')) 


revgeocode(gc)

apiKey = "AIzaSyAEJlMozuo5KhQR34VCfnR_HhfBDZNdk5I"
i=1
lt = as.numeric(filteredData$latitude[i])
lng = as.numeric(filteredData$longitude[i])
 lt
 lng
 pos = c(lng ,lt)
rgc = revgeocode(pos, output = 'more')

grg = google_reverse_geocode(location = pos, key = "AIzaSyAEJlMozuo5KhQR34VCfnR_HhfBDZNdk5I")


filteredData$statename[i] = tolower(gc$results[[1]]$address_components[[6]]$long_name)
print(filteredData$statename[i])
##################



ggplot(filteredData, aes(map_id = ili_data$STATENAME)) + 
  geom_map(aes(fill = ili_data$factored_activitylevel),color = "black", map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map("bonne", lat0 = 20)+labs(fill="ILI Activity Level",
                                     title="2017-2018 Influenza Season Week 4 ending Jan 27, 2018",x="",y="")+
  theme(panel.grid=element_blank())



mapgilbert <- get_map(location = c(lon = -95.712891 , lat = 37.09024), zoom = 4,
                      maptype = "satellite", scale = 2)

ggmap(mapgilbert) +
  geom_point(data = filteredData, aes(x = as.numeric(filteredData$longitude), y = as.numeric(filteredData$latitude), fill = "red", alpha = 0.8), size = 5, shape = 21) 



  guides(fill=FALSE, alpha=FALSE, size=FALSE)

ggplot(filteredData, aes(fill = )) +
  geom_map(aes(map_id = id), map = positions) +
  expand_limits(positions) + ylim(0, 3)




for(i in 1:nrow(filteredData)){
  
    lt = as.numeric(filteredData$latitude[i])
    lng = as.numeric(filteredData$longitude[i])
    # lt
    # lng
    gc = revgeocode(c(lng,lt), output = "all")
    print(gc)
    filteredData$statename[i] = tolower(gc$results[[1]]$address_components[[6]]$long_name)
    
}
ltlll = c(-81.19937181, 28.59965095)
gc = revgeocode(c(-81.19937181, 28.59965095), output = "all")
stname = tolower(gc$results[[1]]$address_components[[6]]$long_name)
stname
save.image("~/Documents/GitHub/CSE587_DIC/lab1/Part3/Part3_2.RData")

##########################################################################################
##########################################################################################
##########################################################################################


mainDF2 = mainDF

#Binding data
mainDF = rbind(mainDF,tdFluDF)

##################################################
#Get State
geocode(c("Amherst","Boston"),output = 'all')$results[[1]]$address_components[[3]]$long_name
geocode(vst$hectorjhoan$getLocation(),output = 'all')$results[[1]]$address_components[[3]]$long_name

#LoadData
test = read.delim("tdFluDFTabbed.txt", sep = "\t" ,header = TRUE,row.names = NULL)

searchStr = '#flu'
startDt = '2018-02-10'
endDt = '2018-02-11'
tdFlu = searchTwitter(searchStr, geocode='37.09024,-95.712891,1340mi',  n=20000, retryOnRateLimit=1)

tdFluDF = twListToDF(tdFlu)
write.table(tdFluDF, "tdFluDFTabbed.txt", sep = "\t", col.names = T, append = F,row.names = F)

?store_tweets_db(tweets, table_name="tweets")
?twitteR

a = searchTwitter('flu', geocode='40.7361,-73.9901,5mi',  n=5000, retryOnRateLimit=1)
summary(a)
?s
a_df = twListToDF(a)
head(a_df)

vst = lookupUsers('hectorjhoan')
vst$hectorjhoan$getLocation()
geocode(vst$hectorjhoan$getLocation())
tuser <- getUser('maidenizer666')

users <- lookupUsers(c('maidenizer666', 'whitehouse'))

which(a_df$longitude != "NA")



#############################################
searchStr = '#fluvirus'
startDt = '2018-01-15'
endDt = '2018-02-16'
#tdFlu = searchTwitter(searchStr, geocode='37.09024,-95.712891,1340mi',  n=20000, retryOnRateLimit=1,since = startDt,until = endDt)
tdFlu = searchTwitter(searchStr,  n=20000, retryOnRateLimit=1)
tdFluDF = twListToDF(tdFlu)
mainFlu = rbind(mainFlu,tdFluDF)

#write.table(tdFluDF, "tdFluDF.csv", sep = ",", col.names = T, append = T)
#############################################


mainInfluenza = rbind(mainInfluenza,tdFluDF)
mainFlu = mainDF
mainFluShot = tdFluDF
mainFluSeason = tdFluDF
mainFluVirus = tdFluDF


mainFlu = rbind(mainFlu,mainInfluenza)
