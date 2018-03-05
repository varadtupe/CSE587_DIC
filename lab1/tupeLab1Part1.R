#Please change the path accordingly
setwd("/Users/varadtupe/Documents/GitHub/CSE587_DIC/lab1/Part1")
############################################################
#Question 1:
############################################################
sales1<-c(12,14,16,29,30,45,19,20,16, 19, 34, 20)
sales2<-rpois(12,34)  # random numbers, Poisson distribution, mean at 34, 12 numbers
par(bg="cornsilk")

plot(sales1, col="blue", type="o", ylim=c(0,100), xlab="Month", ylab="Sales" )
title(main="Sales by Month")

lines(sales2, type="o", pch=22, lty=2, col="red")
grid(nx=NA, ny=NULL)
legend("topright", inset=.05, c("Sales1","Sales2"), fill=c("blue","red"), horiz=TRUE)

############################################################
#Question 2
############################################################
sales<-read.table(file.choose(), header=T)
sales # to verify that data has been read
barplot(as.matrix(sales), main="Sales Data", ylab= "Total",beside=T, col=rainbow(5))

############################################################
#Question 3
############################################################

fn<-boxplot(sales,col=c("orange","green"))$stats

text(1.45, fn[3,2], paste("Median =", fn[3,2]), adj=0, cex=.7)
text(0.45, fn[3,1],paste("Median =", fn[3,1]), adj=0, cex=.7)
grid(nx=NA, ny=NULL)

############################################################
#Question 4
############################################################
fb1<-read.csv("FB.csv")
View(fb1)
par(bg="cornsilk")
aapl1<-read.csv('AAPL.csv')
summary(aapl1)
par(bg="cornsilk")
plot(aapl1$Adj.Close, col="blue", type="o", ylim=c(150,210), xlab="Days", ylab="Price") 
lines(fb1$Adj.Close, type="o", pch=22, lty=2, col="red")
legend("topright", inset=.05, c("Apple","Facebook"), fill=c("blue","red"), horiz=TRUE) 
hist(aapl1$Adj.Close, col=rainbow(8),main = 'Histogram of Apple Stock Adjusted Closing price')

############################################################
#Question 5
############################################################
data()
#Observe the data sets available for explorations.
attach(mtcars)
head(mtcars)
summary(mtcars)
#after analysis remove the data from the memory
detach(mtcars)

attach(uspop)
head(uspop)
plot(uspop)
detach(uspop)

attach(trees)
summary(trees)
plot(trees)
detach(trees)
############################################################
#Question 6
############################################################
library("ggmap")
library("maptools")
library(maps)
visited <- c("SFO", "Chennai", "London", "Melbourne", "Johannesbury, SA")
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(visit.x,visit.y, col="red", pch=36)
 
#Part 2
library("ggmap")
library("maptools")
library(maps)
visited <- c("SFO", "New York", "Buffalo", "Dallas, TX")
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
map("state", fill=TRUE, col=rainbow(50), bg="lightblue", mar=c(0,0,0,0))
points(visit.x,visit.y, col="yellow", pch=36)

############################################################
#Question 7
############################################################
library(lattice)
splom(mtcars[c(1,3,4,5,6)], main="MTCARS Data")
splom(mtcars[c(1,3,4,6)], main="MTCARS Data")
splom(mtcars[c(1,3,4,6)], col=rainbow(10),main="MTCARS Data")
splom(rock[c(1,2,3,4)], main="ROCK Data")

############################################################
#Question 8
############################################################
data1 = trees
euroData
data()
data1$volcat<-cut(data1$Volume,c(0,20,40,60,80,100))

install.packages("ggplot2")
library(ggplot2)
ggplot(data1,aes(x=Height))+geom_histogram(binwidth=5) +  ggtitle('Histogram of Tree Heights')
ggplot(subset(data1),aes(x=Girth,colour=volcat))+geom_density() +  ggtitle('Density of Girth of Trees')

