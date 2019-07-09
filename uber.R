setwd("G:/Upgrad/Course2/UberAssignment")

#loading libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)

#read the uber data file and check the structure
uber<-read.csv("Uber Request Data.csv",stringsAsFactors = F)
str(uber)

##############################################################
                    #Data Cleaning#
##############################################################

#identifying duplicate request id's as request id's must be unique
sum(duplicated(uber$Request.id)) # No duplicate requests

# checking for missing values in dataset
sapply(uber,function(x) length(which(is.na(x)))) # The NA values are valid:
#"Driver.id" Column:Because of non availability & cancellation of cars the driver id are NAs
#"Drop.timestamp" Column:There is no droping points becuase of cancelled trips hence occuring NA's

# checking for blanks
sapply(uber,function(x) length(which(x=="")))  # No blanks

# Check for Uppercase or Lowercase in individual columns 
summary(uber)

#Converting "Request.timestamp","Drop.timestamp" to standard formats
uber$Request.timestamp= lubridate::parse_date_time(uber$Request.timestamp, c("dmY_HMS", "dmY_HM"))
uber$Drop.timestamp<-lubridate::parse_date_time(uber$Drop.timestamp,c("dmY_HMS", "dmY_HM"))

#Deriving new metrics"RequestHours","timeslots"hence becomes easy to validate
uber$RequestHours<-as.numeric(format(uber$Request.timestamp,"%H"))

uber <- mutate(uber, timeslots = ifelse(RequestHours %in% 02:04, "EarlyMorning(2-4)",
                                        ifelse(RequestHours %in% 05:11, "Morning(5-11)",
                                               ifelse(RequestHours %in% 12:15, "Afternoon(12-15)",
                                                      ifelse(RequestHours %in% 16:20,"Evening(16-20)",
                                                             ifelse(RequestHours %in% 21:00,"night(21-00)","LateNight(01-02"))))))



##############################################################################
                    #Data Analysis#
#############################################################################
#Q1)a)Create plots to visualise the frequency of requests that get cancelled or show 'no cars
#available';
#Q1b)identify the most problematic types of requests (city to airport / airport to city etc.)and the time slots (early mornings, late evenings etc.) using plots 


#Ans1a)
#Uni-variate Analysis 
#Plot1:Frequency of requests cancelled vs 'no cars available'
#Choosing bar graph:It gives clear visualisation of status vs freq count of Requests.Geom_text() displays count on each bar which makes easy to visualisation

ggplot(uber,aes(x=Status,fill=Status))+geom_bar(position = "dodge")+geom_text(stat='count',aes(label=..count..),vjust=0)+ggtitle("Frequency of Requests")+xlab("Ride Status")+ylab("Count of Requests")+labs(fill="Status")

#Ans1b)
####Segmented uni-variate analysis####
#Choosing barplot-It gives clear visualisation of timeslots vs freq requests and adding aesthetic fill parameter gives different colouring scales for Airport & city Pickups.
#Also adding grid.arrange to see the 2 plots in single graph for easy visualisation.Using stat_bin explains the details of these summaries in more detail.

uber_cancelled<-subset(uber,uber$Status=="Cancelled")
plot2<-ggplot(uber_cancelled,aes(x=timeslots,fill=Pickup.point))+geom_bar(position = "dodge")+geom_text(stat='count',aes(label=..count..),vjust=0)+ggtitle("Cancelled Requests")+xlab("Time slots")+ylab("Count of Requests")+labs(fill="Status")
Non_Available_Requests <- subset(uber,uber$Status == "No Cars Available")
plot3<-ggplot(Non_Available_Requests,aes(x=timeslots,fill = Pickup.point))+geom_bar(position = "dodge")+geom_text(stat='count',aes(label=..count..),vjust=0)+ggtitle("Non-Available Requests")+xlab("Time slots")+ylab("Count of Requests")+labs(fill="Status")
grid.arrange(plot2,plot3,nrow=1,ncol=2) 

#Explanation
#Problematic Requests:
#During Mornings i.e., 5AM-11AM the cancellation of rides are very high in city.Drivers are not willing to take Airport trips.
#During Evenings i.e., 16-20PM the frequency of cabs is very less in Airports.

#Q2Find out the gap between supply and demand and show the same using plots.
#Find the time slots when the highest gap exists
#Find the types of requests (city-airport or airport-city)
#for which the gap is the most severe in the identified time slots

#Ans2)
#Choosing barplot-It gives clear visualisation of timeslots vs freq requests and adding aesthetic fill parameter 
#gives different colouring scales for Airport & city Pickups.Using stat_bin explains the details of these summaries in more detail.
#grid.arrange to see the 2 plots in single graph for easy visualisation.

uber$uber_supply<- ifelse(uber$Status=="Trip Completed","Trip completed","NotCompleted") # derived new column to analyse supply demand gap
str(uber$uber_supply)
ubersupply<-subset(uber,uber$Status=="Trip Completed")
plot4<-ggplot(ubersupply,aes(x=timeslots,fill=Pickup.point))+geom_bar(position = "dodge")+facet_wrap(~ubersupply$Status)+ggtitle("Supply")+xlab("Time slots")+ylab("Count of Requests")+labs(fill="Status")+geom_text(stat='count',aes(label=..count..),vjust=0)
plot5<-ggplot(uber,aes(x=timeslots,fill=Pickup.point))+geom_bar(position = "dodge")+ggtitle("Demand")+xlab("Time slots")+ylab("Count of Requests")+labs(fill="Status")+geom_text(stat='count',aes(label=..count..),vjust=0)
grid.arrange(plot4,plot5,nrow=1,ncol=2)

#Here demand supply gap=Demand-Supply.Lets visualise in our plots in Airport and in city
ggplot(uber,aes(x=timeslots,fill=factor(uber_supply)))+stat_count(width = 0.5)+geom_bar(position="stack")+
geom_text(stat='count',aes(label=..count..),position=position_stack(vjust=0.5))+facet_wrap(~Pickup.point)+ggtitle("Supply-Demand Gap")+ylab("Frequency of count")+labs(fill="Uber_Supply")

#Ans:
#In Airports during Evening timings i.e.,16PM -20PM the gap is high as graph says that demand supply gap is 818 units
#In city during Mornings i.e., 5Am-11Am the gap is high and graph says that demand supply gap is 750 units

#Q3)What do you think is the reason for this issue for the supply-demand gap? 
#Write the answer in less than 100 words.You may accompany the write-up with plot(s).

#Ans:Assuming Reasons:
#   1)During Mornings demand is high in city as most of the employees will go to offices.
#Decrease supply in city:
##   1)Got Struck at airports in Early morning slots and may be drivers waiting for next trips.
#    2)Lot of demand hence got struck in traffic
#    3)Lazy drivers to take pickup
# During Evenings in Airports demand is high because most of the flights would land at that time.
#Decrease supply in airports:
#   1)There is a sufficient demand in city so drivers are completing trips in city to avoid travelling to long distance ,toll free charges & waiting time for next rides

#Q4) Recommend some ways to resolve the supply-demand gap.
#Ans) By adding boosters we can decrease the supply-demand gap.
#     Introducing surge price in areas where demand is high, we can decrease the Demand-Supply gap
#     Providing toll fee charges to drivers on airport pick ups


