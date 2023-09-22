head(heartrate_seconds_merged)
glimpse(heartrate_seconds_merged)
skim_without_charts(hourlyCalories_merged)
install.packages("skimr")
library(skimr)
str(hourlyCalories_merged)
head(minuteCaloriesWide_merged)

D1 <- read.csv("heartrate_seconds_merged.csv")
D2 <- read.csv("hourlyCalories_merged.csv")
Combined <- rbind(D1,D2)
write.csv(Combined, file = "combined_datasets.csv", row.names = FALSE)
view(heartrate_seconds_merged)
glimpse(dailyActivity_merged)

activity <-read_csv("dailyActivity_merged.csv")
sleep <-read_csv("sleepDay_merged.csv")
weight <-read_csv("weightLogInfo_merged.csv")

activity <-activity %>%
  mutate_at(vars(Id), as.character) %>%
  mutate_at(vars(ActivityDate), as.Date, format = "%m/%d/%y") %>%
  rename("Day"="ActivityDate") 

activity <- activity %>%
  mutate_at(vars(Id), as.character) %>%
  mutate_at(vars(ActivityDate), as.Date, format ="%M%D%Y") %>%
  rename("Day"="ActivityDate")


sleep <-sleep %>%
  mutate_at(vars(Id), as.character) %>%
  mutate_at(vars(SleepDay), as.Date, format = "%m/%d/%y") %>%
  rename("Day"="SleepDay")

sleep = sleep %>%
  mutate_at(vars(Id), as.character) %>%
  mutate_at(vars(SleepDay), as.Date, format = "%M%D%Y") %>%
  rename("Day"="SleepDay")

weight <-weight %>%
  mutate_at(vars(Id,LogId), as.character) %>%
  mutate_at(vars(Date),as.Date, format = "%m/%d/%y") %>%
  rename("Day"="Date")

weight <- weight %>%
  mutate_at(vars(Id,LogId), as.character) %>%
  mutate_at(vars(Date), as.Date, format = "%d%m%y") %>%
  rename("Day"="Date")


combined_data <-sleep %>%
  right_join(activity, by=c("Id","Day")) %>%
  left_join(weight, by=c("Id", "Day")) %>%
  mutate(Weekday = weekdays(as.Date(Day, "m/%d/%Y")))

combined_data <- 
  sleep %>% 
  right_join(activity, by=c("Id","Day")) %>%
  left_join(weight, by=c("Id","Day")) %>%
  mutate(Weekday = weekdays(as.Date(Day, "%d%m%y")))
             
             
  help("weekdays")           
             
             Weekday <-factor(combined_data$Weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

combined_data %>%
  select(TotalMinutesAsleep, TotalSteps, TotalDistance, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories, WeightKg, Fat, BMI, IsManualReport) %>%
  summary()
combined_data %>%
  select(TotalMinutesAsleep,TotalSteps,TotalDistance,VeryActiveMinutes,FairlyActiveMinutes,LightlyActiveMinutes,SedentaryMinutes,Calories,WeightKg,Fat,BMI,IsManualReport) %>%
  summary()


ggplot(data=combined_data, aes(x=Weekday, y=TotalSteps)) + 
  geom_bar(stat="identity", fill="green")+
  labs(title="Steps by Day", y="Total Steps") 

ggplot(data=combined_data, aes(x=Weekday, y=TotalSteps)) +
  geom_bar(stat = "identity", fill="red") +
  labs(title = "total steps by day", y="Total steps")



ggplot(data=combined_data, aes(x=Weekday, y=FairlyActiveMinutes)) + 
  geom_bar(stat="identity", fill="blue")+
  labs(title="Fairly Active Minutes by Day", y="Minutes") 

ggplot(data=combined_data, aes(x=Weekday, y=FairlyActiveMinutes)) +
  geom_bar(stat"identity", fill="turquoise") +
  labs(title = "Fairy Active Minutes", y= "Minutes")


ggplot(data=combined_data, aes(x=Weekday, y=ModeratelyActiveMinutes)) + 
  geom_bar(stat="identity", fill="yellow")+
  labs(title="Moderately Active Minutes by Day", y="Minutes") 
ggplot(data = combined_data, aes(x=Weekday, y=ModeratelyActiveMinutes)) +
  geom_bar(stat = "identity", fill="violet") +
  labs(title = "moderately active minutes by day", y="Minutes")


ggplot(data=combined_data, aes(x=Weekday, y=LoggedActivitiesDistance)) + 
  geom_bar(stat="identity", fill="#ba8089")+
  labs(title="Logged Activity Distance by Day", y="Logged Activity Distance") 

ggplot(data=combined_data,aes(x=Weekday, y=LoggedActivitiesDistance)) +
  geom_bar(stat = "identity", fill="orange") +
  labs(title = "logged activities distance by day", y= "logged activity distance")

# Distribution of sleep time 
ggplot(combined_data, aes(TotalMinutesAsleep)) +
  geom_histogram(bins=10, na.rm=TRUE,color = "lightblue",fill="green" )+
  labs(title="Distribution of Total Time Asleep", x="Total Time Asleep (minutes)") 

ggplot(data=combined_data,aes(TotalMinutesAsleep))
geom_histogram(bins = 10, na.rm = TRUE, color="darkblue", fill="white")
labs(title="distribution of total time asleep", x= "total time asleep(minutes)")


# Total minutes Asleep vs Calories
ggplot(combined_data) +
  geom_point(mapping = aes(x=TotalMinutesAsleep/60, y=DailyCalories), na.rm=TRUE, color="#fa8072") +
  labs(title="Calories vs Time Slept", x="Time Asleep (Hours)", y="DailyCalories") 
ggplot(data=combined_data) +
  geom_point(aes(x=TotalMinutesAsleep/60, y=DailyCalories), na.rm=TRUE,color="lightgreen" ) +
  labs(title = "calories vs time slept", x= "time asleep in hours", y="daily calories")


head(combined_data)
view(combined_data)

install.packages("openxlsx")
library(openxlsx)

# Export data to excel file and CSV file
write.xlsx(combined_data, file="Fitbit_Fitness_Data.xlsx")
write.csv(combined_data, file = "Fitbit.csv")
