#Importing Libraries
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)

setwd('C:/Users/abhis/OneDrive/Desktop/Work and Internships/Projects/Eastspring Investments')

#Importing Datasets
original.assets.dt<- read.csv('assets.csv')
original.ism.dt<- read.csv('ism.csv')

#View(original.assets.dt)
#?View(original.ism.dt)

#Overview of Datasets
summary(original.assets.dt)
summary(original.ism.dt)

#==========================================================================================
#------------------------------------- Data Cleaning -------------------------------------
#==========================================================================================

#Creating a copy of Original Datasets
clean.assets.dt<- copy(original.assets.dt)
clean.ism.dt<- copy(original.ism.dt)

#Formating Dates
clean.assets.dt$Date<- as.Date(clean.assets.dt$Date, format='%d/%m/%Y')
clean.ism.dt$ISM.Release.Date<- as.Date(clean.ism.dt$ISM.Release.Date, format='%d/%m/%Y')

clean.assets.dt$Day<- day(clean.assets.dt$Date)
clean.assets.dt$Month<- month(clean.assets.dt$Date)
clean.assets.dt$Year<- year(clean.assets.dt$Date)

clean.ism.dt$Day<- day(clean.ism.dt$ISM.Release.Date)
clean.ism.dt$Month<- month(clean.ism.dt$ISM.Release.Date)
clean.ism.dt$Year<- year(clean.ism.dt$ISM.Release.Date)

#Removing NA values
clean.assets.dt<- na.omit(clean.assets.dt)
clean.ism.dt<- na.omit(clean.ism.dt)

#------------------------------------- Assets Data -------------------------------------
#Since ISM Data begins only from 1998, we can drop the columns in Assets that are older than 1998
clean.assets.dt<- clean.assets.dt[clean.assets.dt$Year>=1998,]

# Calculate the monthly mean asset values
monthly.mean.assets.dt <- clean.assets.dt %>% group_by(Year, Month) %>% summarize(MonthlyMeanAsset = mean(US.Equity.Index))

monthly.mean.assets.dt$Date <- as.Date(paste(01, monthly.mean.assets.dt$Month, monthly.mean.assets.dt$Year), "%d %m %Y")

drop <- c('Year', 'Month')

monthly.mean.assets.dt<- monthly.mean.assets.dt[, !(names(monthly.mean.assets.dt) %in% drop)]

monthly.mean.assets.dt$assets.percentage.change<- ((monthly.mean.assets.dt$MonthlyMeanAsset - lag(monthly.mean.assets.dt$MonthlyMeanAsset, order_by = monthly.mean.assets.dt$Date))/monthly.mean.assets.dt$MonthlyMeanAsset)*100

View(monthly.mean.assets.dt)

#------------------------------------- ISM Data -------------------------------------
#Month-to-month change
clean.ism.dt$ISM.Monthly.Change<- clean.ism.dt$ISM.Actual.Reported - lag(clean.ism.dt$ISM.Actual.Reported, order_by = clean.ism.dt$ISM.Release.Date)

#Direction of change
clean.ism.dt <- clean.ism.dt %>%
  mutate(direction = ifelse(ISM.Monthly.Change > 0, 1, -1))

#Forecast Vs Actual
clean.ism.dt$ISM.Forecast.Difference<- -(clean.ism.dt$ISM.Median.Forecast - clean.ism.dt$ISM.Actual.Reported)

clean.ism.dt$Date <- as.Date(paste(01, clean.ism.dt$Month, clean.ism.dt$Year), "%d %m %Y")

drop <- c('Day', 'Month', 'Year', 'ISM.Release.Date')

clean.ism.dt<- clean.ism.dt[, !(names(clean.ism.dt) %in% drop)]

View(clean.assets.dt)
View(clean.ism.dt)


#Overview of Datasets
summary(clean.assets.dt)
summary(clean.ism.dt)


#------------------------------------- Merging Datasets ------------------------------
merged.dt<- merge(x= monthly.mean.assets.dt, y= clean.ism.dt, by= 'Date')
merged.dt<- na.omit(merged.dt)

View(merged.dt)

#==========================================================================================
#------------------------------------ Data Exploration ------------------------------------
#==========================================================================================

#creating a correlation matrix using Pearson Correlation
correlation.dt <- copy(merged.dt)
drop <- c('direction', 'Date', 'ISM.Median.Forecast', 'ISM.Actual.Reported', 'MonthlyMeanAsset')

correlation.dt<- correlation.dt[, !(names(correlation.dt) %in% drop)]
corr_matrix <- cor(correlation.dt, method = "pearson")
print(corr_matrix)

melted_corr <- melt(corr_matrix)

#Plotting the correlation matrix into a heatmap
ggplot(melted_corr, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Pearson Correlation Matrix") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  geom_text(aes(label = round(value, 2)), size = 2, color = "black")

# Create a plot using ggplot2
start_date <- as.Date("2010-01-01")
end_date <- as.Date("2022-04-01")

ggplot(merged.dt, aes(x = Date)) +
  geom_line(aes(y = assets.percentage.change - 15, color = "Asset Value"), size = 0.5) +
  geom_line(aes(y = ISM.Monthly.Change + 15, color = "Monthly Change in ISM"), size = 0.5) +
  geom_line(aes(y = ISM.Forecast.Difference, color = "Forecast Difference in ISM"), size = 0.5) +
  labs(title = "", x = "Date", y = "Percentage Change") +
  scale_color_manual(values = c("Asset Value" = "black", "Monthly Change in ISM" = "red", "Forecast Difference in ISM" = "blue")) +
  theme_minimal() +
  theme(legend.position = "bottom",        
        legend.box = "horizontal",         
        legend.text = element_text(size = 8),  
        plot.title = element_text(size = 14),  
        axis.title.y = element_text(size = 12),  
        axis.text.y = element_blank(),      
        axis.ticks.y = element_blank()) +   
  coord_cartesian(xlim = c(start_date, end_date)) 

#Using a Regression Model to Validate Jack's Results
monthly.change.model <- lm(merged.dt$assets.percentage.change ~ merged.dt$ISM.Monthly.Change, data = merged.dt)

# Print the summary of the regression
summary(monthly.change.model)

#Using a Regression Model to Validate Jills's Results
forecast.difference.model <- lm(merged.dt$assets.percentage.change ~ merged.dt$ISM.Forecast.Difference, data = merged.dt)

# Print the summary of the regression
summary(forecast.difference.model)

#==========================================================================================
#----------------------------------- Linear Regression -----------------------------------
#==========================================================================================

#creating a correlation matrix using Pearson Correlation
correlation.dt <- copy(merged.dt)
drop <- c('Date')

correlation.dt<- correlation.dt[, !(names(correlation.dt) %in% drop)]
corr_matrix <- cor(correlation.dt, method = "pearson")
print(corr_matrix)

melted_corr <- melt(corr_matrix)

#Plotting the correlation matrix into a heatmap
ggplot(melted_corr, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Pearson Correlation Matrix") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  geom_text(aes(label = round(value, 2)), size = 2, color = "black")

# Fit a linear model
proposed.model <- lm(merged.dt$assets.percentage.change ~ merged.dt$ISM.Actual.Reported + merged.dt$ISM.Median.Forecast)

# Print the summary of the regression
summary(proposed.model)
