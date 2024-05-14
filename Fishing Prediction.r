fishing <- read.csv("fishing.csv")
fishing <- na.omit(fishing)


money_per_hour <- fishing$Value[1:152]/fishing$Time[1:152]

fishing['MoneyPerHour'] <- money_per_hour

#Verify That Data on Money Per Hour is Normal
qqnorm(fishing$MoneyPerHour)
qqline(fishing$MoneyPerHour, col ="orange")

#Rank - Regression
plot(MoneyPerHour~Rank, data = fishing)
rank.lm <- lm(MoneyPerHour~Rank, data=fishing)
summary(rank.lm)

#Time - Regression
plot(MoneyPerHour~Time, data = fishing)
time.lm <- lm(MoneyPerHour~Time, data=fishing)
summary(time.lm)

#Search - t-test
t.test(MoneyPerHour~Search, data = fishing)

#General - t-test
t.test(MoneyPerHour~General, data = fishing)

#Boat Age - t-test
t.test(MoneyPerHour~Boat_Age, data = fishing)

#Port - t-test
t.test(MoneyPerHour~Port, data = fishing)

#Experienced - t-test
t.test(MoneyPerHour~Experienced, data = fishing)

#Value - regression
plot(MoneyPerHour~Value, data = fishing)
value.lm <- lm(MoneyPerHour~Value, data=fishing)
summary(value.lm)

#Sea - 5 discrete levels, cannot be analysed

#Depth - t-test
t.test(MoneyPerHour~Depth, data = fishing)

#Catch - regression
plot(MoneyPerHour~Catch, data = fishing)
catch.lm <- lm(MoneyPerHour~Catch, data=fishing)
summary(catch.lm)






#3 Best variables

#Verify That Data on Money Per Hour is Normal
qqnorm(fishing$MoneyPerHour, main = "Normal Q-Q Plot of Money Obtained Per Hour")
qqline(fishing$MoneyPerHour, col ="orange")


#Search - t-test
t.test(MoneyPerHour~Search, data = fishing, var.equal=FALSE)

#General - t-test
t.test(MoneyPerHour~General, data = fishing, var.equal=FALSE)

#Time - Regression
plot(MoneyPerHour~Time, data = fishing)
time.lm <- lm(MoneyPerHour~Time, data=fishing)
summary(time.lm)


# Time - Regression
# Create the scatterplot
plot(MoneyPerHour ~ Time, data = fishing, 
     xlab = "Time (Hours)",
     ylab = "Money Per Hour (Thousands)",
     main = "Scatterplot of Money Per Hour vs. Time")  

# Fit the linear regression model
time.lm <- lm(MoneyPerHour ~ Time, data = fishing)

# Add a trendline (regression line)
abline(time.lm, col = "red")  

# Display the regression summary
summary(time.lm)
