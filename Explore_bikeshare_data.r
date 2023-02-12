
library(ggplot2)
library(lubridate)

#Read in the data sets, and set any blank cell to NA
ny = read.csv('new_york_city.csv', na.strings=c("","NA"))
wash = read.csv('washington.csv', na.strings=c("", "NA"))
chi = read.csv('chicago.csv', na.strings=c("","NA"))

head(ny)
nrow(ny)

head(wash)
nrow(wash)

head(chi)
nrow(chi)

wash$Gender<-NA
wash$Birth.Year<-NA
wash$City<-'Washington'
ny$City<-'New York'
chi$City<-'Chicago'

head(wash)
head(chi)
head(ny)

#Concat the data sets
bikeshare<-rbind(chi,wash)
bikeshare<-rbind(bikeshare,ny)
head(bikeshare)
nrow(bikeshare)

#Convert trip duration from seconds to minutes for easier
#reading and comporehension of scale
bikeshare$Trip.Duration<-(bikeshare$Trip.Duration/60)
ny$Trip.Duration<-(ny$Trip.Duration/60)
chi$Trip.Duration<-(chi$Trip.Duration/60)
wash$Trip.Duration<-(wash$Trip.Duration/60)

#Confirming trip duration has been converted properly
head(bikeshare)
head(ny)
head(chi)
head(wash)

ggplot(aes (x = Trip.Duration), data = subset(bikeshare, !is.na(User.Type))) +
    geom_histogram(binwidth = 1, fill='darkgreen') +
    scale_x_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
    facet_grid(City~User.Type) +
    xlab('Trip duration (minutes)') +
    ylab('Number of Rides') 

by(bikeshare$Trip.Duration, bikeshare$User.Type, summary)

by(bikeshare$Trip.Duration, bikeshare$City, summary)

ggplot(aes (x = month(bikeshare$Start.Time)), data = bikeshare) +
    geom_histogram(binwidth=1, color='lightgreen', fill='darkgreen') + 
    scale_x_continuous(breaks = 1:6) +
    xlab('Month of Year') +
    ylab('Number of Rides')

ggplot(aes (x = month(bikeshare$Start.Time)), data = bikeshare) +
    geom_histogram(binwidth=1, color='lightgreen', fill='darkgreen') + 
    facet_wrap(~City) +
    scale_x_continuous(breaks = 1:6) +
    xlab('Month of Year') +
    ylab('Number of Rides')

ggplot(aes(x = hour(bikeshare$Start.Time), y = bikeshare$Trip.Duration), data=bikeshare) +
    geom_point(alpha = 0.2, 
               position=position_jitter(w=0.5), 
               color='darkgreen') +
    ylim(0,500)+
    scale_x_continuous(breaks=0:23)+
    geom_line(stat='summary',fun.y=mean)+
    geom_line(stat='summary',
              fun.y=quantile,
              fun.args=list(probs=0.9),
              linetype=2)+
    xlab('Hour of Day')+
    ylab('Ride Duration (minutes)')+
    ggtitle('All Cities')

ggplot(aes(x = hour(wash$Start.Time), y = wash$Trip.Duration), data=wash) +
    geom_point(alpha = 0.2, 
               position=position_jitter(w=0.5), 
               color='darkgreen') +
    ylim(0,500)+
    scale_x_continuous(breaks=0:23)+
    geom_line(stat='summary',fun.y=mean)+
    geom_line(stat='summary',
              fun.y=quantile,
              fun.args=list(probs=0.9),
              linetype=2)+
    xlab('Hour of Day')+
    ylab('Ride Duration (minutes)')+
    ggtitle('Washington, DC')

ggplot(aes(x = hour(ny$Start.Time), y = ny$Trip.Duration), data=ny) +
    geom_point(alpha = 0.2, 
               position=position_jitter(w=0.5), 
               color='darkgreen') +
    ylim(0,200)+
    scale_x_continuous(breaks=0:23)+
    geom_line(stat='summary',fun.y=mean)+
    geom_line(stat='summary',
              fun.y=quantile,
              fun.args=list(probs=0.9),
              linetype=2)+
    xlab('Hour of Day')+
    ylab('Ride Duration (minutes)')+
    ggtitle('New York City')

ggplot(aes(x = hour(chi$Start.Time), y = chi$Trip.Duration), data=chi) +
    geom_point(alpha = 0.2, 
               position=position_jitter(w=0.5), 
               color='darkgreen') +
    ylim(0,200)+
    scale_x_continuous(breaks=0:23)+
    geom_line(stat='summary',fun.y=mean)+
    geom_line(stat='summary',
              fun.y=quantile,
              fun.args=list(probs=0.9),
              linetype=2)+
    xlab('Hour of Day')+
    ylab('Ride Duration (minutes)')+
    ggtitle('Chicago')

system('python -m nbconvert Explore_bikeshare_data.ipynb')
