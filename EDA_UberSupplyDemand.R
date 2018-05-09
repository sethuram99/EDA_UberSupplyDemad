library(lubridate) 
library(dplyr)

RequestData <- read.csv("Uber Request Data.csv", stringsAsFactors = F)


head(RequestData)
str(RequestData)


#Parsing the date to same format
mdy <- dmy_hms(RequestData$Request.timestamp, tz="Asia/Calcutta") 
dmy <- dmy_hm(RequestData$Request.timestamp, tz="Asia/Calcutta") 
mdy[is.na(mdy)] <- dmy[is.na(mdy)] # some dates are ambiguous, here we give 
RequestData$Request.timestamp <- mdy        # mdy precedence over dmy

#Parsing the date to same format
mdy <- dmy_hms(RequestData$Drop.timestamp, tz="Asia/Calcutta") 
dmy <- dmy_hm(RequestData$Drop.timestamp, tz="Asia/Calcutta") 
mdy[is.na(mdy)] <- dmy[is.na(mdy)] # some dates are ambiguous, here we give 
RequestData$Drop.timestamp <- mdy        # mdy precedence over dmy



#Plot to check trips per status
ggplot(RequestData, aes(x= factor(Status))) + geom_bar()


RequestData$Request.timestamp <- format(RequestData$Request.timestamp,format='%Y-%m-%d %H:%M')
RequestData$Drop.timestamp <- format(RequestData$Drop.timestamp,format='%Y-%m-%d %H:%M')

RD_groups <- group_by(RequestData, Status)
RD_groups_summary <- summarise(RD_groups, totalcount = n())
head(RD_groups_summary)

#Separating the Date and Time for Request and Drop for further analysis
RequestDataTime <- separate(RequestData, Request.timestamp, into=c("RequestDate", "RequestTime"), sep = " ") 
hm(RequestDataTime$RequestTime)

RequestDataTime <- separate(RequestDataTime, Drop.timestamp, into=c("DropDate", "DropTime"), sep = " ") 
hm(RequestDataTime$DropTime)


RequestTime_Airport <- RequestData %>% filter(Status == "Trip Completed" & Pickup.point == "Airport") 

mean(difftime(Average_RequestTime_Airport$Drop.timestamp, Average_RequestTime_Airport$Request.timestamp, tz = "GMT" ,units = "mins"))

#Average trip duration from Airpot to City is 52.52576 mins

RequestTime_City <- RequestData %>% filter(Status == "Trip Completed" & Pickup.point == "City") 

mean(difftime(RequestTime_City$Drop.timestamp, RequestTime_City$Request.timestamp, tz = "GMT" ,units = "mins"))

#Average trip duration from City to Airport is 52.85957 mins

#write.csv(Average_RequestTime_Airport, file = "AVGRTAP.csv")


#Number of requests per hour to find the peak hours
RequestsPerHour <- RequestData %>% 
  mutate(hour = strftime(Request.timestamp, format="%H:%M")) %>% 
  group_by(hour) %>% 
  summarize(Count = n()) %>% 
  # add the date back in
  mutate(x_date = ymd_hm(paste("2018-02-01", hour))) %>%
  ungroup()

ggplot(RequestsPerHour, aes(x = x_date, y = Count, group = 1)) + 
  geom_line() + scale_x_datetime(date_breaks = "1 hour",
                               date_labels = "%H:%M") 

#From the above chart we observed that 05:00-10:00 and 17:00-23:00 are the peak hours



#Find out the demand supply during 05:00-10:00
RequestsStatusPerHour_Morning <- RequestData %>% 
  mutate(request_date = strftime(Request.timestamp, format="%H:%M")) %>% 
  group_by(request_date, Status) %>% 
  summarize(Count = n()) %>%  
  # add the date back in
  mutate(requestdate = ymd_hm(paste("2018-02-01", request_date))) %>%
  ungroup() %>% filter(requestdate > ymd_hms("2018-02-01 05:00:00") & requestdate < ymd_hms("2018-02-01 10:00:00")) 

ggplot(RequestsStatusPerHour_Morning, aes(x = requestdate, y = Count, color = Status, group = 1)) + 
  geom_line() + scale_x_datetime(date_breaks = "1 hour",
                                 date_labels = "%H:%M") + scale_colour_manual(values=c("red", "blue", "green"))

#We have more number of request for which we have no cars available

#Find out the source where we are getting more requests
RequestsSource_Morning <- RequestData %>% 
  mutate(request_date = strftime(Request.timestamp, format="%H:%M")) %>% 
  group_by(request_date, Pickup.point) %>% 
  summarize(Count = n()) %>%  
  # add the date back in
  mutate(requestdate = ymd_hm(paste("2018-02-01", request_date))) %>%
  ungroup() %>% filter(requestdate > ymd_hms("2018-02-01 05:00:00") & requestdate < ymd_hms("2018-02-01 10:00:00")) %>%
  spread(key = Pickup.point, value = Count, is.na <- 0)

ggplot(RequestsSource_Morning, aes(x = requestdate, y = Count, group = 1)) + 
  geom_line(aes(y = City), colour="blue") + 
  geom_line(aes(y = Airport), colour = "orange") + 
  scale_x_datetime(date_breaks = "1 hour",
                   date_labels = "%H:%M")

#We have got more requests from City during this duration


#Finding the peak hours where cabs were not available
NotAvailable_At_City <- RequestData %>% filter(Status != "Trip Completed" & Pickup.point == "Airport")
NotAvailable_At_City_Group <- NotAvailable_At_City %>% 
  mutate(request_date = strftime(Request.timestamp, format="%H:%M")) %>% 
  group_by(request_date, Status) %>% 
  summarize(Count = n()) %>%  
  # add the date back in
  mutate(requestdate = ymd_hm(paste("2018-02-01", request_date))) %>%
  filter(requestdate > ymd_hms("2018-02-01 05:00:00") & requestdate < ymd_hms("2018-02-01 10:00:00")) %>% 
  ungroup() %>% spread(key = Status, value = Count, is.na <- 0)

colnames(NotAvailable_At_City_Group)[colnames(NotAvailable_At_City_Group) == "No Cars Available"] <- "NoCars"

ggplot(NotAvailable_At_City_Group, aes(x = requestdate)) + 
  geom_line(aes(y = Cancelled), colour="blue") + 
  geom_line(aes(y = NoCars), colour = "orange") + 
  ylab(label="Number of requests") + 
  xlab("Hour")+ scale_x_datetime(date_breaks = "1 hour",
                                 date_labels = "%H:%M") 

#We have more number of request which belongs to status "No Cars Available" around 07:30 and "Cancelled" at 08:45

#Find out the number of cars which are in trip during this period
ActiveTrips <- RequestData %>% filter(Status == "Trip Completed")
ActiveTrips_Group_Morning <- ActiveTrips %>% 
  mutate(request_date = strftime(Request.timestamp, format="%H:%M")) %>% 
  group_by(request_date, Pickup.point) %>% 
  summarize(Count = n()) %>%  
  # add the date back in
  mutate(requestdate = ymd_hm(paste("2018-02-01", request_date))) %>%
  filter(requestdate > ymd_hms("2018-02-01 05:00:00") & requestdate < ymd_hms("2018-02-01 10:00:00")) %>% 
  ungroup() %>% spread(key = Pickup.point, value = Count, is.na <- 0)

ggplot(ActiveTrips_Group_Morning, aes(x = requestdate)) + 
  geom_line(aes(y = Airport), colour="blue") + 
  geom_line(aes(y = City), colour = "orange") + 
  ylab(label="Number of requests") + 
  xlab("Hour")+ scale_x_datetime(date_breaks = "1 hour",
                                 date_labels = "%H:%M") 


#We have equal distribution of requests in Airport and City



#Find out the demand supply during after 17:00
RequestsStatusPerHour <- RequestData %>% 
  mutate(request_date = strftime(Request.timestamp, format="%H:%M")) %>% 
  group_by(request_date, Status) %>% 
  summarize(Count = n()) %>%  
  # add the date back in
  mutate(requestdate = ymd_hm(paste("2018-02-01", request_date))) %>%
  ungroup() %>% filter(requestdate > ymd_hms("2018-02-01 17:00:00")) 

ggplot(RequestsStatusPerHour, aes(x = requestdate, y = Count, color = Status, group = 1)) + 
  geom_line() + scale_x_datetime(date_breaks = "1 hour",
                                 date_labels = "%H:%M") + scale_colour_manual(values=c("red", "blue", "green"))
#We have more number of request for which we have no cars available

#Find out the source where we are getting more requests
RequestsSource <- RequestData %>% 
  mutate(request_date = strftime(Request.timestamp, format="%H:%M")) %>% 
  group_by(request_date, Pickup.point) %>% 
  summarize(Count = n()) %>%  
  # add the date back in
  mutate(requestdate = ymd_hm(paste("2018-02-01", request_date))) %>%
  ungroup() %>% filter(requestdate > ymd_hms("2018-02-01 17:00:00")) %>%
  spread(key = Pickup.point, value = Count, is.na <- 0)

ggplot(RequestsSource, aes(x = requestdate, y = Count, group = 1)) + 
  geom_line(aes(y = City), colour="blue") + 
  geom_line(aes(y = Airport), colour = "orange") + 
  scale_x_datetime(date_breaks = "1 hour",
                                 date_labels = "%H:%M")

#We have got more requests from Airport during this duration


#Finding the peak hours where cabs were not available
NotAvailable_At_Airport <- RequestData %>% filter(Status != "Trip Completed" & Pickup.point == "Airport")
NotAvailable_At_Airport_Group <- NotAvailable_At_Airport %>% 
  mutate(request_date = strftime(Request.timestamp, format="%H:%M")) %>% 
  group_by(request_date, Status) %>% 
  summarize(Count = n()) %>%  
  # add the date back in
  mutate(requestdate = ymd_hm(paste("2018-02-01", request_date))) %>%
  filter(requestdate > ymd_hms("2018-02-01 17:00:00")) %>% 
  ungroup() %>% spread(key = Status, value = Count, is.na <- 0)

setnames(NotAvailable_At_Airport_Group, "No Cars Available", "NoCars")
colnames(NotAvailable_At_Airport_Group)[colnames(NotAvailable_At_Airport_Group) == "No Cars Available"] <- "NoCars"

ggplot(NotAvailable_At_Airport_Group, aes(x = requestdate)) + 
  geom_line(aes(y = Cancelled), colour="blue") + 
  geom_line(aes(y = NoCars), colour = "orange") + 
  ylab(label="Number of requests") + 
  xlab("Hour")+ scale_x_datetime(date_breaks = "1 hour",
                                 date_labels = "%H:%M") 

#We have more number of request which belongs to status "No Cars Available"

#Find out the number of cars which are in trip during this period
ActiveTrips <- RequestData %>% filter(Status == "Trip Completed")
ActiveTrips_Group <- ActiveTrips %>% 
  mutate(request_date = strftime(Request.timestamp, format="%H:%M")) %>% 
  group_by(request_date, Pickup.point) %>% 
  summarize(Count = n()) %>%  
  # add the date back in
  mutate(requestdate = ymd_hm(paste("2018-02-01", request_date))) %>%
  filter(requestdate > ymd_hms("2018-02-01 17:00:00")) %>% 
  ungroup() %>% spread(key = Pickup.point, value = Count, is.na <- 0)

ggplot(ActiveTrips_Group, aes(x = requestdate)) + 
  geom_line(aes(y = Airport), colour="blue") + 
  geom_line(aes(y = City), colour = "orange") + 
  ylab(label="Number of requests") + 
  xlab("Hour")+ scale_x_datetime(date_breaks = "1 hour",
                                 date_labels = "%H:%M") 


#Number of Requests Per Hour
ActiveTrips_Group_Hour <- ActiveTrips %>% 
  mutate(request_date = hour(Request.timestamp)) %>% 
  group_by(request_date) %>% 
  summarize(Count = n())
