library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(lubridate)

# Fetching the data 
chi.data <- read.csv(url("https://data.cityofchicago.org/api/views/ijzp-q8t2/rows.csv?accessType=DOWNLOAD"))


# Seeing the structure of the data
str(chi.data)

# Changing the date column from a factor to a date
chi.data <- chi.data %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Arranging by date 
chi.data <- chi.data %>%
  arrange(desc(Date))


# Making a checkpoint in case the dataset gets destroyed
chi.data.chkpt1 <- chi.data

# Saving the dataframe object for easier loading next time without having to reformat everything.
# Load with 'load("chidata.Rda")'
save(chi.data, file = "chidata.Rda")
load(file = "chidata.Rda")

str(chi.data)

# Making some groupings and calculating total counts by year for visualization
chi.summ <- chi.data %>% 
  group_by(Year, Date, Primary.Type, Location.Description, Arrest, Domestic) %>%
  summarize(total = n_distinct(ID))


str(chi.summ)
summary(chi.summ)

# Let's see how many different type of crime are reported 
unique(chi.summ$Primary.Type)

# Looking at how many of these reports are domestic disputes
prop.table(table(chi.summ$Domestic))
prop.table(table(chi.summ$Arrest))

# Breaking down each report type by whether or not it is a domestic dispute
table(chi.summ$Domestic, chi.summ$Primary.Type)

# Loading in the scales package for modifying the axes
library(scales)

# Number of crimes reported by year
chi.summ %>%
  group_by(Year) %>%
  summarize(total = sum(total)) %>%
  ggplot(aes(x = Year, y = total)) + geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0, 500000)) +
  scale_x_continuous(breaks = c(seq(2001,2018))) +
  ylab("Total Crimes Reported") +
  xlab("Year")


# Number of crimes reported by whether or not an arrest was made 
chi.summ %>%
  group_by(Year, Arrest) %>%
  summarize(total = sum(total)) %>%
  ggplot(aes(x = Year, y = total)) + geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0, 500000)) +
  scale_x_continuous(breaks = c(seq(2001,2018))) +
  facet_grid(.~Arrest) +
  ylab("Total Crimes Reported") +
  xlab("Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Top 10 crime types over the entire timeframe
chi.summ %>%
  group_by(Primary.Type) %>%
  summarize(total = sum(total)) %>%
  arrange(desc(total)) %>%
  slice(1:10) 

# Number of crimes reported by primary crime type
# Need to come back to this, not a good way to visualize 
chi.summ %>%
  group_by(Year, Primary.Type) %>%
  summarize(total = sum(total)) %>%
  arrange(desc(total)) %>%
  ggplot(aes(x = Year, y = total, color = Primary.Type)) + geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0, 200000)) +
  scale_x_continuous(breaks = c(seq(2001,2018))) +
  # facet_grid(.~Arrest) +
  ylab("Total Crimes Reported") +
  xlab("Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(.~Primary.Type)



# Number of crimes reported per month colored by year
chi.summ %>%
  group_by(month, year) %>%
  summarize(total = sum(total)) %>%
  ggplot(aes(x = month, y = total, color = factor(year))) + geom_line() + 
  scale_x_continuous(breaks = seq(1,12), labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(labels = comma) +
  xlab("Month") +
  ylab("Crimes Reported")


# Another way to visualize crimes reported by month, facted by year
chi.data %>%
  group_by(Year,month) %>%
  filter(Year > 2007) %>%
  summarize(total = n_distinct(ID)) %>%
  ggplot(aes(x = month, y = total)) + geom_line() + 
  facet_grid(.~Year) +
  scale_x_continuous(breaks = seq(1,12), labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(labels = comma, limits = c(0,45000)) + 
  ylab("Total Crimes Reported") + 
  xlab("Month")


# Total number of reported crimes by type, colored by whether or not there was an arrest
chi.data %>%
  group_by(Primary.Type, Arrest) %>%
  summarize(total = n()) %>%
  ggplot(aes(x = reorder(Primary.Type, total), y = total, fill = Arrest)) + 
  geom_col() + 
  coord_flip() + 
  ylab("Crime Reported") +
  xlab("Total Reports") +
  scale_y_continuous(labels = comma, breaks = seq(0, 1250000, 200000))



# Percentage of arrest by type
chi.data %>%
  group_by(Primary.Type) %>%
  summarize(total = n(), total_arrest = sum(Arrest == "true"), prop = sum(Arrest == "true")/ n()) %>%
  # mutate(freq = sum(Arrest == "true") / sum(n())) %>%
  # arrange(Arrest,freq) %>%
  # mutate(Arrest = factor(Arrest, levels = c("true","false"))) %>%
  ggplot(aes(x = reorder(Primary.Type, prop),y = prop)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  ylab("Percentage of reports that lead to an arrest") +
  xlab("Crime Reported") +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.1)) 

# Domestic Violence, prostitution, narcotics, public indecency, gambling and liquor law violations
# all have nearly 100% arrest rate
table(chi.data$Primary.Type)

# There is only 1 "domestic violence" crime reported in the entire database. This is likely because 
# there is a separate field for specifying whether or not it was a domestic crime. 
# 
# Public indecency has a low report rate (164 reports)
# 
# Prostitution and gambling have high report rates and are still near 100% arrest rate. Might be 
# a result of sting operations. 
# 
# Why narcotics and liquor law violation have such high arrest rates might be due to zero tolerance
# policies.


# Let's plot the coordinate data on a map.
devtools::install_github("dkahle/ggmap")
library(ggmap)

# Adding the API key - You must register a Google Cloud Platform account and create an API
# token, then allow Google Maps access via the token
register_google(key = "INSERT GOOGLE API KEY HERE")

# Adding year and month to the full data set
chi.ym <- chi.data %>%
  mutate(year = lubridate::year(Date),
         month = lubridate::month(Date),
         day = lubridate::day(Date))

# Let's make a subset of the data that only includes violent crimes
unique(chi.ym$Primary.Type)
chi.violent <- chi.ym %>% filter(Primary.Type %in% c('ROBBERY', 'BATTERY', 'ASSAULT', 'CRIM SEXUAL ASSAULT', 'HOMICIDE'))
chi.homicide <- chi.ym %>% filter(Primary.Type %in% c('HOMICIDE'))


gg <- ggmap(get_map("Chicago", zoom = 11))
gg + stat_density2d(data=chi.violent %>% filter(Year == 2001), aes(x=Longitude, y=Latitude, fill=..level.., alpha=..level..),
                    geom="polygon", size=0.01, bins=5, n = 80) + ggtitle('2001') + 
  # scale_alpha_continuous(limits = c(0,80),breaks = c(0,20,40,60,80)) +
  scale_fill_continuous(limits = c(0,100))


year_vector <- 2001:2018

# Loop for creating points for each crime
for (i in year_vector) {
  gg <- ggmap(get_map("Chicago", zoom = 11))
  plot(gg +
         geom_jitter(data = chi.violent%>% filter(year == i), aes(x = Longitude, y = Latitude),
                     size = 0.01, alpha = 0.2) + 
         ggtitle(i) + 
         # scale_alpha(range = c(0,80)) +
         # scale_alpha_continuous(limits = c(0,80), breaks = c(0,20,40,60,80)) +
         # scale_fill_continuous(limits = c(0,80)) +
         # scale_alpha_continuous(limits = c(0,80)) +
         theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank()))
  ggsave(filename = paste(i,"_points.png", sep = ""), device = "png", path = "/Users/casey.moroney")
  
}





# Loop for creating a heatmap for each year 
for (i in year_vector) {
  gg <- ggmap(get_map("Chicago", zoom = 11))
  plot(gg +
         stat_density2d(data = chi.violent%>% filter(year == i), aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                        geom="polygon", size = 0.01, bins=5, n = 80) + 
         ggtitle(i) + 
         # scale_alpha(range = c(0,80)) +
         # scale_alpha_continuous(limits = c(0,80), breaks = c(0,20,40,60,80)) +
         scale_fill_continuous(limits = c(0,80)) +
         scale_alpha_continuous(limits = c(0,80)) +
         theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank())
       
  ) 
  print(i)
  ggsave(filename = paste(i,".png", sep = ""), device = "png", path = "/Users/casey.moroney")
  
}

# Loop for creating violent crime point plot and heatmap side by side
install.packages("gridExtra")
library(gridExtra)
gg <- ggmap(get_map("Chicago", zoom = 11))
for (i in year_vector) {
  
  plot1 <- gg + geom_jitter(data = chi.violent%>% filter(year == i), aes(x = Longitude, y = Latitude),
                            size = 0.01, alpha = 0.1) + 
    ggtitle(i) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  plot2 <- gg + stat_density2d(data = chi.violent%>% filter(year == i), aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                               geom="polygon", size = 0.01, bins=5, n = 80, show.legend = FALSE) + 
    ggtitle(" ") + 
    scale_fill_continuous(limits = c(0,80)) +
    scale_alpha_continuous(limits = c(0,80)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  print(i)
  ggsave(filename = paste("combo",i,".png", sep = ""), device = "png", path = "/Users/casey.moroney", arrangeGrob(plot1, plot2, nrow = 1, ncol = 2), height = 3.5, width = 5.5)
  
}



# Loop for creating homicide point plot and heatmap side by side
install.packages("gridExtra")
library(gridExtra)
gg <- ggmap(get_map("Chicago", zoom = 11))
for (i in year_vector) {
  
  plot1 <- gg + geom_jitter(data = chi.homicide %>% filter(year == i), aes(x = Longitude, y = Latitude),
                            size = 0.01, alpha = 0.5) + 
    ggtitle(i) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  plot2 <- gg + stat_density2d(data = chi.homicide %>% filter(year == i), aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                               geom="polygon", size = 0.01, bins=5, n = 80, show.legend = FALSE) + 
    ggtitle(" ") + 
    scale_fill_continuous(limits = c(0,80)) +
    scale_alpha_continuous(limits = c(0,80)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  print(i)
  ggsave(filename = paste("homicide",i,".png", sep = ""), device = "png", path = "/Users/casey.moroney", arrangeGrob(plot1, plot2, nrow = 1, ncol = 2))
  
}
