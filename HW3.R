#Jamie Boud
#HW3

#In Class Code Activity
install.packages(c("ggplot2", "dplyr"))
library(ggplot2)
library(dplyr)
install.packages("lubridate")
library(lubridate)


datCO2 = read.csv("/cloud/project/annual-co-emissions-by-region.csv")

# check column names
colnames(datCO2)

# change the 4 column name
colnames(datCO2)[4] <- "CO2"
# check names again
colnames(datCO2)

# convert the entity names to factor and store a variable with levels for
# easy reference
datCO2$Entity <- as.factor(datCO2$Entity)

plot(datCO2$Year, datCO2$CO2)

US = datCO2 %>%
  filter(Entity == "United States")

ME = datCO2 %>%
  filter(Entity == "Mexico")

#Plotting using base R----

plot(US$Year, US$CO2,
     type="b",
     pch=19,
     xlab= "Year",
     ylab= "Fossil Fuel emissions(billions of tons of CO2",
     yaxt="n")
axis(2, seq(0, 6000000000, by=2000000000), seq(0,6, by=2), las=2)


#GG Plot ----

ggplot(US, aes(x=Year, y=CO2))+ geom_point()+
  geom_line()+
  labs(x="Year", y="US fossil fuel CO2 emissions (tons CO2)")+
  theme_classic()

NorthA = datCO2 %>%
  filter(Entity == "United States" |
           Entity == "Mexico" |
           Entity == "Canada")

ggplot(NorthA,
       aes(x=Year, y=CO2, color=Entity))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("red", "royalblue", "darkgoldenrod3"))

#In Class Prompt Activity ----

tempAnom = read.csv("/cloud/project/climate-change.csv")

#Activity 1, Plot with base R and ggplot----

#base R ----
tempAnom$date = ymd(tempAnom$Day)

NH = tempAnom %>%
  filter(Entity == "Northern Hemisphere")
SH = tempAnom %>%
  filter(Entity == "Southern Hemisphere")

plot(tempAnom$date,tempAnom$temperature_anomaly) %>%

plot(NH$date,NH$temperature_anomaly,
  pch = 20,
  ylab = "Temperature Anomaly",
  xlab = "Date",
  col = adjustcolor("red", alpha.f = 0.2))
points(SH$date,SH$temperature_anomaly,
       pch = 20,
       ylab = "Temperature Anomaly",
       xlab = "Date",
       col = adjustcolor("blue", alpha.f = 0.2))
legend("topleft",
       c("Northern Hemisphere", "Southern Hemisphere"),col=c("red", "blue"),
       pch=19, bty= "n")

#gg plot----

world = tempAnom[tempAnom$Entity == "Northern Hemisphere" | tempAnom$Entity == "Southern Hemisphere", ]

ggplot(data = world, # data for plot
       aes(x = date, y= temperature_anomaly, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  labs(x="Year", y="Temperature Anomaly")+ # make axis labels
  theme_classic()

#Plot the total all time emissions for the United States, Mexico, and Canada----

NorthA <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(data = NorthA, # data for plot
       aes(x = Year, y=Annual.CO2.emissions..zero.filled., color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ # make axis labels
  theme_classic()

#HW3 Start----
