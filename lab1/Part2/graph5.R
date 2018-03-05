setwd("~/CSE587/lab1/Part2")
# Number of Influenza-Associated Pediatric Deaths by Week of Death data downloaded on Jan-27-2018			

ped_old <- read.csv("./data/graph5_PedFluDeath_WeeklyData.csv", header=T, sep=",")

head(ped_old)
names(ped_old)

ped <- ped_old[-c(1:7),]

names(ped) <- c("season","week_number","current_week_deaths","previous_week_death")
head(ped)
tail(ped)
attach(ped)
library("ggplot2")

ggplot(data=ped) + 
  geom_bar(aes(x=as.character(week_number), y=as.numeric(as.character(previous_week_death))), 
           colour="dark green", stat="identity",
           position=position_dodge(),
           size=.2)+ theme(axis.text.x = element_text(angle = 90, hjust = 2))+
  geom_bar(aes(x = as.character(week_number), y=as.numeric(as.character(current_week_deaths))),
           fill="blue",stat = "identity")+
  xlab("Week of death") + ylab("Number of deaths")+ 
  scale_x_discrete(breaks=c("2014-40","2014-46",
                            "2014-52","2015-05","2015-11","2015-17","2015-23","2015-29","2015-35",
                            "2015-41","2015-47","2016-01","2016-07","2016-13","2016-19",
                            "2016-25","2016-31","2016-37","2016-43","2016-49","2017-03","2017-09",
                            "2017-15","2017-21","2017-27","2017-33","2017-39","2017-45","2017-51"))+
  scale_y_continuous(limits=c(0,30),breaks = seq(0,30,5),expand=c(0,0))+
   ggtitle("Number of Influenza-Associated Pediatric Deaths by Week of Death: 2014-2015 season to present")+
  theme(panel.grid.major = element_line(color= NA),
        panel.grid.minor = element_line(color= NA))
