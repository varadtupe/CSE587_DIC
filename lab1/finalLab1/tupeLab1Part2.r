##########################################
# DIC lab 1 part 2 
##########################################
#Krithika Krishnan
#Varad Tupe
##########################################
# 1) Influenza national summary
##########################################

# install.packages("ggplot2")
rm(list = ls())
setwd("/Users/varadtupe/Documents/GitHub/CSE587_DIC/lab1/finalLab1/")
library(ggplot2)
library(reshape2)
#Influenza Positive Tests Reported to CDC by U.S. Clinical Laboratories 2017-2018 Season
part1Data = read.csv('./data/graph1.csv',header = T,sep = ',')
names(part1Data)
part1Data$Week = as.factor(part1Data$Week)
stack1Data = melt(part1Data[,1:3],id.vars = 'Week')
plotS1 <- ggplot(stack1Data) 
plotS1 +  geom_bar(aes(x=Week,y=value,factor=variable,fill=variable),stat="identity",colour="black") + 
  scale_fill_manual(values=c("#FFF450","#007E32")) +
    geom_line(data=part1Data, aes(group=1,x=part1Data$Week,y=550*part1Data$Percent.Positive.A),size=1,col="yellow",lty="dashed") +
  geom_line(data=part1Data, aes(group=1,x=part1Data$Week,y=550*part1Data$Percent.Positive.B),size=1,col="green",lty="dotted") +
  geom_line(data=part1Data, aes(group=1,x=part1Data$Week,y=550*part1Data$X..Positive),size=1,col="black") +
  scale_colour_manual(values=c("yellow","green","black"))+
  theme(axis.text.x = element_text(angle = 75, hjust = 1))+
  scale_x_discrete(breaks=c("201740","201742","201744","201746","201748","201750","201752","201802","201804","201806",
                                                                     "201808","201810","201812","201814","201816","201818","201820"),expand=c(0,0))+
  # scale_y_continuous(breaks = round(seq(2000, 18000, by = 2000),1))+
  scale_y_continuous(name="Number of positive specimens",sec.axis = sec_axis(~.*0.0018, name = "Percent Positive",breaks = seq(0, 30, by = 2)),limits=c(0,18000),breaks = seq(0, 18000, by = 2000))+
ggtitle("Influenza Positive Tests Reported to CDC by U.S. Clinical Laboratories, \n National Summary, 2017-2018 Season")+
  theme(panel.grid.major = element_line(color= NA),
        panel.grid.minor = element_line(color= NA)) +
  guides(col = guide_legend(ncol = 1)) + 
  theme(legend.key = element_blank(), 
        legend.position = "top", legend.title = element_blank(),
        legend.box.just = "left")

# 
# b=data.frame(part1Data$Week="201806",values=NA)
# a <- rbind(a,b)
# ggplot(a,aes(x=year,y=values,group=1))+geom_line()
# scale_x_discrete(limits=levels(as.factor(part1Data$Week)))+
# scale_x_discrete(labels= c("201740","201742","201744","201746","201748","201750","201752","201802","201804","201806",
# "201808","201810","201812","201814","201816","201818","201820"),limits=c("201740","201741","201742","201743","201744","201745","201746","201747","201748","201749","201750","201751","201752","201801","201802","201803","201804","201805","201806",
# "201807","201808","201809","201810","201811","201812","201813","201814","201815","201816","201817","201818","201819","201820"))+


# scale_x_discrete(limits=c("201740","201742","201744","201746","201748","201750","201752","201802","201804","201806",
                          # "201808","201810","201812","201814","201816","201818","201820"))+
  # scale_x_discrete(limits=levels(as.factor(part1Data$Week)))
##########################################

##########################################
# 2) Positive tested 
##########################################
part2Data = read.csv('./data/graph2.csv',header = T,sep = ',')
names(part2Data)
part2Data$Week = as.factor(part2Data$Week)
stack2Data = melt(part2Data[,c("Week","A.Subtyping.not.performed.","A..H1N1.pdm09","A.H3.","A.H3N2v.","B.","BVIC","BYAM")],
                  id.vars = 'Week')
plotS2 <- ggplot(stack2Data) 
plotS2 +  geom_bar(aes(x=Week,y=value,factor=variable,fill=variable), stat="identity",colour="black") + 
  scale_fill_manual(values=c("#FFF450","#F2A406","#F80005","#8F09D9","#008232","#A5D10C","#007E32"),
                    labels = c("A (subtyping not performed)","A (H1N1)pdm09","A (H3N2)","H3N2v.",
                               "B (lineage not performed)","B (Victoria Lineage","B (Yamagata Lineage)"),name = "") +
  ylab("Number of Positive Specimens")+
  scale_x_discrete(breaks=c("201740","201742","201744","201746","201748","201750","201752","201802","201804","201806",
                            "201808","201810","201812","201814","201816","201818","201820"))+
  scale_y_continuous(limits=c(0,4000),breaks = seq(0,4000,500),expand=c(0,0))+
  theme(axis.text.x = element_text(angle = 75, hjust = 1))+ 
  ggtitle("Influenza Positive Tests Reported to CDC by U.S. Public Health \n Laboratories, National Summary, 2017-2018 Season")+
  theme(panel.grid.major = element_line(color= NA),
        panel.grid.minor = element_line(color= NA))
##########################################
# 3) Influenza sub-type pie-charts 

##########################################

part3Data = read.csv('./data/3_piechart.csv',header = T,sep = ',',fill = TRUE)
names(part3Data)
attach(part3Data)
nm = c('Influenza A (H3 N2)','Influenza A (H1 N1)pdm09','Influenza A (subtype unknown)','Influenza B Victoria','Influenza B Yamagata','Influenza B lineage not determined')

val = c(18068,1896,348,228,2292,921)
valchar = as.character(c(18068,1896,348,228,2292,921))

#val = c(18068,1896,348,228,2292,921)
val = c(921,2292,228,348,1896,18068)
valchar = as.character(c(921,2292,228,348,1896,18068))
#valchar = as.character(c(18068,1896,348,228,2292,921))
mainPie = data.frame(nm,val)

ggplot(mainPie, aes(x = "", y = val, fill =valchar)) +
  geom_col(width = 1) +
  scale_fill_manual(values = c("red","orange", "yellow","lightgreen","green","darkgreen"),labels= nm)+
  coord_polar("y", start = 0*pi / 2,direction = -1) +
  ggtitle("Influenza Positive Specimens Reported to \n U.S. Public Health Laboratories Cumulative, 2017-2018 Season")+
  xlab("")+ylab("")

############################################
part3Data$X..of..Sub.type.Total = as.character(part3Data$X..of..Sub.type.Total)
part3Data$X..of..Sub.type.Total = gsub('%', '', part3Data$X..of..Sub.type.Total)
part3Data$X..of..Sub.type.Total = as.numeric(part3Data$X..of..Sub.type.Total)
part3Data$X..of..Sub.type.Total[which(is.na(part3Data$X..of..Sub.type.Total))] <- 0

ggplot(part3Data[part3Data$X.Sub.type == 'H3',], aes(x = "", y = X..of..Sub.type.Total, fill = Genetic_Group)) +
  geom_col(width = 1) +
  #scale_fill_manual(values = c("red", "yellow")) +
  coord_polar("y", start = 0*pi / 2,direction = -1) +
  labs(title = "Influenza A (H3 N2)")+
  xlab("")+ylab("")

ggplot(part3Data[part3Data$X.Sub.type == 'B/Victoria',], aes(x = "", y = X..of..Sub.type.Total, fill = Genetic_Group)) +
  geom_col(width = 1) +
  #scale_fill_manual(values = c("red", "yellow")) +
  coord_polar("y", start = 0*pi / 2,direction = -1) +
  labs(title = 'Influenza A (H1 N1)pdm09')+
  xlab("")+ylab("")

ggplot(part3Data[part3Data$X.Sub.type == 'B/Yamagata',], aes(x = "", y = X..of..Sub.type.Total, fill = Genetic_Group)) +
  geom_col(width = 1) +
  #scale_fill_manual(values = c("red", "yellow")) +
  coord_polar("y", start = 0*pi / 2,direction = -1) +
  labs(title = 'Influenza B Victoria')+
  xlab("")+ylab("")


ggplot(part3Data[part3Data$X.Sub.type == 'H1pdm09',], aes(x = "", y = X..of..Sub.type.Total, fill = Genetic_Group)) +
  geom_col(width = 1) +
  #scale_fill_manual(values = c("red", "yellow")) +
  coord_polar("y", start = 0*pi / 2,direction = -1) +
  labs(title = 'Influenza B Yamagata')+
  xlab("")+ylab("")

############################################
# 6) Influenza-like illness
############################################
# Number of Influenza-Associated Pediatric Deaths by Week of Death data downloaded on Jan-27-2018

ped_old <- read.csv("./data/graph5_PedFluDeath_WeeklyData.csv", header=T, sep=",")


#head(ped_old)
names(ped_old)
ped <- ped_old[-c(1:7),]
names(ped) <- c("season","week_number","current_week_deaths","previous_week_deaths")
head(ped)
attach(ped)
library("ggplot2")

####
previous_week_death=as.numeric(ped$current_week_deaths)
current_week_deaths=as.numeric(ped$current_week_deaths)

names(ped)
stackData = melt(ped[,2:4],id.vars = 'week_number',factorsAsStrings = F)

####



ggplot(data=stackData) + 
  geom_bar(aes(x=stackData$week_number, y=as.numeric(stackData$value),fill=stackData$variable), 
            stat="identity",size=.2,colour="black")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 2))+
  annotate("text", x = c(30,70,110,150), y = c(20,20,20,20), label = c("2014-2015\n Number of Deaths\n Reported = 148", 
      "2015-2016\n Number of Deaths\n Reported = 93", "2016-2017\n Number of Deaths\n Reported = 110", 
      "2017-2018\n Number of Deaths\n Reported = 53") , color="black", size=5 )+
  xlab("Week of death") + ylab("Number of deaths")+ 
  scale_fill_manual(values=c("blue","dark green"),
                      labels = c("Deaths Reported Current Week","Deaths Reported Previous Week")) +
    scale_x_discrete(breaks=c("2014-40","2014-46",
                            "2014-52","2015-05","2015-11","2015-17","2015-23","2015-29","2015-35",
                            "2015-41","2015-47","2016-01","2016-07","2016-13","2016-19",
                            "2016-25","2016-31","2016-37","2016-43","2016-49","2017-03","2017-09",
                            "2017-15","2017-21","2017-27","2017-33","2017-39","2017-45","2017-51"))+
  scale_y_continuous(limits=c(0,30),breaks = seq(0,30,5),expand=c(0,0))+
  labs(fill="")+
  ggtitle("Number of Influenza-Associated Pediatric Deaths \n by Week of Death: 2014-2015 season to present")+
  theme(panel.grid.major = element_line(color= NA),
        panel.grid.minor = element_line(color= NA))

############################################

# 7) Flu heat map of USA 
############################################


############################################################################## 
# A Weekly Influenza Surveillance Report Prepared by the Influenza Division
# Influenza-Like Illness (ILI) Activity Level Indicator Determined by Data Reported to ILINet
##############################################################################
# reading in the data for week 4 2018

ili_data <- read.csv("./data/StateDatabyWeekforMap_2017-18week4.csv",header = T, sep = ",")

#head(ili_data)
levels(factor(ili_data$ACTIVITY.LEVEL))

levels(factor(ili_data$ACTIVITY.LEVEL.LABEL))
ili_data$STATENAME <- tolower(ili_data$STATENAME)
levels(factor(ili_data$STATENAME)) # 54

#### heatmap of US
# install.packages("maps")
# # install.packages("ggmap")
# # install.packages("mapdata")
# library(ggplot2)
library(maps)
library(mapproj)
library(fiftystater)

ff<-fifty_states
cnames <- aggregate(cbind(ff$long, ff$lat) ~ ff$id, data=ff, 
                    FUN=function(x)mean(range(x)))

names(cnames)[1] <- "state"
names(cnames)[2] <- "long"
names(cnames)[3] <- "lat"
a <- data.frame(state="new york city", long=-74.0060, lat=40.7128)
b <- data.frame(state="puerto rico", long=-66.5901, lat=18.2208)
a <- rbind(a,b)
c <- data.frame(state="virgin islands", long=-64.8963, lat=18.3358)
a <- rbind(a,c) 
cnames <- rbind(cnames,a)  
head(cnames)

# factoring activity into a high to low order 
ili_data$factored_activitylevel <- factor(ili_data$ACTIVITY.LEVEL,
                                          levels = c("Level 10", "Level 9","Level 8","Level 7","Level 6","Level 5","Level 4","Level 3","Level 2","Level 1","Level 0" ))


## to be added to display alaska & hawaii names 
# geom_text(data=cnames, aes(cnames$long, cnames$lat, label=cnames$state), size=2)


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
                                     title="2017-18 Influenza Season Week 4 ending Jan 27, 2018",x="",y="")+
  theme(panel.grid=element_blank())

