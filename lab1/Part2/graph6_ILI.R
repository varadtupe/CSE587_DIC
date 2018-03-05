############################################################################## 
# A Weekly Influenza Surveillance Report Prepared by the Influenza Division
# Influenza-Like Illness (ILI) Activity Level Indicator Determined by Data Reported to ILINet
#
##############################################################################
rm(list = ls())
setwd("/Users/varadtupe/Documents/GitHub/CSE587_DIC/lab1/Part2")
ili_data <- read.csv("./graph6_StateDataforMap_2017-18week4.csv",header = T, sep = ",")

head(ili_data)
levels(factor(ili_data$ACTIVITY.LEVEL))
# [1] "Level 0"  "Level 1"  "Level 10" "Level 3"  "Level 4"  "Level 5"  "Level 6"  "Level 8"  "Level 9" 
#Level 2 Level 7 

levels(factor(ili_data$ACTIVITY.LEVEL.LABEL))
# [1] "High"              "Insufficient Data" "Low"               "Minimal"           "Moderate"         

ili_data$STATENAME <- tolower(ili_data$STATENAME)
levels(factor(ili_data$STATENAME))# 54
##############################################################################
#### heatmap of US
# install.packages("maps")
# # install.packages("ggmap")
# # install.packages("mapdata")
library(ggplot2)
library(maps)



###########
# library(ggplot2)
# install.packages("mapproj")
library(mapproj)
# install.packages("fiftystater")
library(fiftystater)
ff<-fifty_states
cnames <- aggregate(cbind(ff$long, ff$lat) ~ ff$id, data=ff, 
                    FUN=function(x)mean(range(x)))

names(cnames)[1] <- "state"
names(cnames)[2] <- "long"
names(cnames)[3] <- "lat"
a <- data.frame(state="new york city", long=-74.0060, lat=40.7128)
a
b <- data.frame(state="puerto rico", long=-66.5901, lat=18.2208)
a <- rbind(a,b)
a
c <- data.frame(state="virgin islands", long=-64.8963, lat=18.3358)
a <- rbind(a,c)
a
cnames <- rbind(cnames,a)  
cnames

# factoring activity into a high to low order 
ili_data$factored_activitylevel <- factor(ili_data$ACTIVITY.LEVEL,
    levels = c("Level 10", "Level 9","Level 8","Level 7","Level 6","Level 5","Level 4","Level 3","Level 2","Level 1","Level 0" ))



# to display alaska & hawaii names 
geom_text(data=cnames, aes(cnames$long, cnames$lat, label=cnames$state), size=2)

x11()

ggplot(ili_data, aes(map_id = ili_data$STATENAME)) + 
  geom_map(aes(fill = ili_data$factored_activitylevel),color = "black", map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  scale_fill_manual(values = c("Level 10" = "#b32400", "Level 9" = "#e65c00",
                                 "Level 8" = "#ff8533", "Level 7" = "#ff9900", 
                                 "Level 6" = "#ffcc00", 
                                 "Level 5" = "#faec94", "Level 4" = "#c6ff1a",
                                 "Level 3" = "#99ff33", "Level 2" = "#00ff00", 
                                 "Level 1" = "#2eb82e", "Level 0" = "#ffffff"),
                    labels=c("High", "High", "High", "Moderate","Moderate","Low","Low",
                             "Minimal","Minimal","Minimal", "Insufficient data"), drop = FALSE)+
    coord_map("bonne", lat0 = 20)+labs(fill="ILI Activity Level",
                   title="2017-2018 Influenza Season Week 4 ending Jan 27, 2018",x="",y="")+
              theme(panel.grid=element_blank())
