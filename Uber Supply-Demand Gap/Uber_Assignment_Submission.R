#Import the required libraries



#For graph plotting
# Install 
install.packages("ggthemes") 

# Load
library(ggthemes) 
library(ggplot2)

#For date and time function
library(lubridate) 
library(dplyr)            # For grouping operations #
library(tidyr)            # For grouping operations #


#Read the Uber Data file
uber_data <- read.csv("Uber Request Data.csv",header= TRUE)

######################################## Data Cleaning ##########################################################


#Remove the unnecessary columns Request id,Driver id and Drop timestamp,they dont serve any purspose
#in the analysis.The droptimestamp can be used to calculate the wait times or Idle times.In the given
#data set ,it cannot be used as there is no continuity in the pick up and drop  for a particular driver id
#Hence, we remove the Driver id and Drop.timestamp columns

#Delete Uncessary Columns
uber_data<- uber_data[,c(-1,-3,-6)]

#Fix missing values if any

#check if there are any NA's in Request timestamp.Since there are no NA's we dont need to deal
length(which(is.na(uber_data$Request.timestamp)))== 0

#Standardise format for date and time

#Convert the date time to requied format using parse_date_time.
#There are four kinds of formats available in the excel sheet,if there are any missing vlaues in the seconds field
uber_data$Request.timestamp<-parse_date_time(uber_data$Request.timestamp,c('%d/%m/%Y %H:%M:%S',"%d-%m-%y %H:%M","%d-%m-%y %H:%M%S","%d-%m-%y %H:%M"),tz="Asia/Calcutta" ,truncated=6)

#################################### Segmented Univariate Analysis ##########################################


#1.Get the times in separate column
uber_data$Request_Time<-format(uber_data$Request.timestamp, "%H:%M:%S")


#2.Get the day of the week based on the Date
uber_data$day_of_week <- weekdays(uber_data$Request.timestamp)

#3.Get the hour of the day form the time
uber_data$hour_of_the_day<-as.numeric(format(uber_data$Request.timestamp,"%H"))

#4.Segment the data into time of the day with 4 slots as early morning ,afternoon ,late evening and night.
uber_data$Time_of_Day <- sapply( (uber_data$hour_of_the_day), function(x) {
  if(x >= 4 && x <=10 ) {
    return ("early morning")
  }else if(x >=17 && x <= 22 )
  {
    return("late evening")
  }
  else if(x >=23 || x <= 4 )
   { return ("night")}
  else
    return("afternoon")
})

#5.Write the updated csv to a file if analysis to be done by Tableau
#write.csv(uber_data,"C:/Users/comp/Documents/PGDM/Uber Assignment/uber_data_cleaned.csv")

####################### Demand analysis ########################################################################################


####################### PLOT-1 :Hourly Distribution of Pick up Requests To and From the City#######################

#Plot showing frequency of requests throughout the day and type of requests at various timeslots
#Type of graph :Histogram ,as we are building a frequency table.
#Colors are used to differentiate differnt pick up points to know where the max request comes from
#The bin width reprsent an hour each

plot_by_requests_per_hour<- ggplot(uber_data, aes(x=hour_of_the_day,fill=Pickup.point))+
                            geom_histogram(stat = "count",binwidth = 2) +
                            stat_count(aes(y=..count..,label=..count..),geom="text",vjust=1,size=3)+
                            ggtitle("Hourly Distribution of Pick up Requests To and From the City")+
                            labs(x= "Time in Hour of day",y="Number of Requests",fill="Pick up Point")+
                            theme_excel()
print(plot_by_requests_per_hour)

#Summary :
#Based on the plot max demand for cabs from City and Airpot is given below
# -------------------------------
# |Time Slot |Pickup | Drop     |
# |---------------------------- |
# |5am -9am  |city   | Airport  |
# |5pm -9pm  |Airport| City     |
#  -----------------------------

#######################PLOT-2:Hourly Distribution of Requests Based on Time of the Day #######################
#Type of graph :Histogram ,as we are building a frequency table.
#Colors are used to differentiate differnt type of trip status 
#The bins represent the time of day
#They are grouped by the pickup point to analyse the spread of request depending on this factor.

plot_by_trip_status_per_time_of_day_wise <-ggplot(uber_data,aes(x = Time_of_Day, fill=Status)) + 
                                           geom_histogram(stat="count") +facet_grid(Pickup.point ~ .)+stat_count(aes(y=..count..,label=..count..),geom="text",hjust=0.5,vjust=1.0,size=3)+
                                           ggtitle("Hourly Distribution of Number of Requests across the day") +
                                           labs(x= "Time Of Day",y="Number of Requests",color="Trip Status") +
                                           theme_excel()

print(plot_by_trip_status_per_time_of_day_wise)

####################### Supply analysis ########################################################################################

#The next two plots helping in knowing the % of supply for the hours of the day

#######################PLOT-3:Hourly Distribution of Trip Status with pick up from the Airport#######################

#Group by the hour of the day and status of the Trip from City to Airport and find out the % of success
#Type of graph :Line plot as we use it to see the trend analysis 
#the points show the success percentage at each hour
#clearly identifies the demand v/s supply with the help of success ratio

#Group the data by hour and find the total number of requests and those that get completed

uber_data_city = uber_data[uber_data$Pickup.point=="City",]
uber_data_grouped <- uber_data_city %>% group_by(hour_of_the_day,Status)%>%
summarise (Number_of_Requests = n()) %>%
mutate(freq = Number_of_Requests / sum(Number_of_Requests))

#plotting the grouped data
uber_data_gap_from_city <- aggregate(freq~hour_of_the_day,uber_data_grouped[which(uber_data_grouped$Status == 'Trip Completed'),],sum)
plot_by_trip_status_and_destination <-ggplot(uber_data_gap_from_city,aes(x = hour_of_the_day,y=freq,label=sprintf("%0.2f", round(freq, digits = 2)))) + 
                                      geom_line() +geom_point()+
                                      geom_text(size = 3, hjust = 0.5,vjust=1.5) +
                                      scale_x_continuous("Hour of day",breaks=c(0:24)) +
                                      ggtitle("% success for Trips from City to Airport throught the day") +
                                      labs(x= "Hour Of Day",y="% of Success") 


print(plot_by_trip_status_and_destination)

#Summary
#Based on the graph , the max supply and Demand gaps from City to Airport:
# -------------------------------
# |Time Slot |Success %         |
# |---------------------------- |
# |4am -10pm |<= 40%            |
# -------------------------------

#There is demand supply gap in this timeslot and needs to be analyzed further.

  
  
  ####################### PLOT-4:Hourly Distribution of Trip Status with pick up from the Airport #######################

#Group by the hour of the day and status of the Trip from Airport to City and find out the % of success
#Type of graph :Line plot as we use it to see the trend analysis 
#the points show the success percentage at each hour
#clearly identifies the demand v/s supply with the help of success ratio

uber_data_airport = uber_data[uber_data$Pickup.point=="Airport",]
uber_data_grouped <- uber_data_airport %>% group_by(hour_of_the_day,Status)%>%
summarise (Number_of_Requests = n()) %>%
mutate(freq = Number_of_Requests / sum(Number_of_Requests))

uber_data_gap_from_city <- aggregate(freq~hour_of_the_day,uber_data_grouped[which(uber_data_grouped$Status == 'Trip Completed'),],sum)
plot_by_trip_status_and_pickup_Airport <-ggplot(uber_data_gap_from_city,aes(x = hour_of_the_day,y=freq,label=sprintf("%0.2f", round(freq, digits = 2)))) + 
                                      geom_line() +geom_point()+
                                      geom_text(size = 3, hjust = 0.5,vjust=1.5) +
                                      scale_x_continuous("Hour of day",breaks=c(0:24)) +
                                      ggtitle("% success for Trips from Airport to City throught the day") +
                                      labs(x= "Hour Of Day",y="% of Success") 

print(plot_by_trip_status_and_pickup_Airport)

#Summary
#Based on the graph , the max supply and Demand gaps from Airport to City:
# -------------------------------
# |Time Slot |Success %         |
# |---------------------------- |
# |5pm -9pm |<= 30%             |
#--------------------------------

#The demand and low supply in the timeslot .Needs further analysis on what is cause of the low success rate.
####################### Gap analysis ########################################################################################

#The next two plots helping in knowing the reason for gaps in demand and supply
#We analyze differnt issues that decrease the success rate creating gaps in supply when demand is high

  
  
#######################PLOT-5:Hourly Distribution of Trip Status with pick up from the City###########################

#Build a line plot with hour of the day on x axis and count of requests in Y axis
#Type of graph :Line plot as we use it to see the trend analysis 
#the line graph show the peak cancellation and no cars requests time slots throughout the day


plot_by_trip_status_per_hour_from_city    <-ggplot(uber_data[uber_data$Pickup.point == "City",],aes(x = hour_of_the_day, color = Status))+
                                            geom_line(stat = 'count')+
                                            scale_x_continuous("Hour of day",breaks=c(0:24)) +
                                            ggtitle("Hourly Distribution of Trip Status with pick up from the City") +
                                            labs(x= "Time in Hour of day",y="Number of Requests",color="Trip Status") +
                                            theme_solarized_2(light = FALSE)+
                                            scale_colour_solarized("blue")
print(plot_by_trip_status_per_hour_from_city)

#Summary:

#Based on the graph , the most problemtic time slot for City to Airport:
# -------------------------------
# |Time Slot |Pickup | Drop     |
# |---------------------------- |
# |4am -10am  |city   | Airport |
#  -----------------------------



#######################PLOT-6:Hourly Distribution of Trip Status with pick up from the Airport#######################

#Type of graph :Line plot as we use it to see the trend analysis 
#the line graph show the peak cancellation and no cars requests time slots throughout the day
#the line graph show the peak cancellation and no cars requests time slots throughout the day

plot_by_trip_status_per_hour_from_airport <-ggplot(uber_data[uber_data$Pickup.point == "Airport",],aes(x = hour_of_the_day, color = Status)) + 
                                            geom_line(stat = 'count') +
                                            scale_x_continuous("Hour of day",breaks=c(0:24)) +
                                            ggtitle("Hourly Distribution of Trip Status with pick up from the Airport") +
                                            labs(x= "Time in Hour of day",y="Number of Requests",color="Trip Status") +
                                            theme_solarized_2(light = FALSE) +
                                            scale_colour_solarized("blue")

print(plot_by_trip_status_per_hour_from_airport)

#Summary:

#Based on the graph , the most problemtic time slot for Airport to City:
# -------------------------------
# |Time Slot |Pickup | Drop     |
# |---------------------------- |
# |5pm -11pm |Airport  | City   |
#  -----------------------------
####################### Hypothesis  ##############################################

#We test across all the days of the week to confirm if the pattern remains the same

#######################PLOT-7:Hourly Distribution of Requests across the week #######################

plot_by_trip_status_per_hour_day_wise <-ggplot(uber_data,aes(x = hour_of_the_day, linetype = day_of_week,color=Pickup.point)) + 
                                            geom_line(stat = 'count') +
                                            scale_x_continuous("Hour of day",breaks=c(0:24)) +
                                            ggtitle("Hourly Distribution of Trip Status with pick up from the Airport") +
                                            labs(x= "Time in Hour of day",y="Number of Requests",color="Pickup Point") +
                                            theme_solarized_2(light = FALSE) +
                                            scale_colour_solarized("blue")
print(plot_by_trip_status_per_hour_day_wise)

#Summary:
#The graph shows that the pattern remains same across all weekdays.

