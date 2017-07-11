# Useful library

library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)

# Get data on the computer

if(!file.exists('data.zip')){
        download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip', destfile = 'data.zip')
}

if(!file.exists('activity.csv')){
        unzip('data.zip')
}


# Get data into R

data <-fread('activity.csv', na.strings = 'NA', sep = ',')


## Prepartion of a raw dataset called 'data'

data <- as.data.frame(data)
data[ , 'date'] <- as.Date(data[ , 'date'], '%Y-%m-%d')


## Prepartion of a clean dataset called 'clean_data'


# Localise date and interval with missing steps values

result_1 <- data[is.na(data[ , 'steps']), ]
result_1 <- select(result_1, c(date, interval))


# Compute the mean of each interval

result_2 <- data[complete.cases(data),]
result_2 <- aggregate(result_2[ , 'steps'], by = list ( interval = result_2[ , 'interval'] ), FUN = mean )
result_2 <- rename( result_2, steps = 'x' )


# Merge the not completed rows with their mean

result <- merge(result_1, result_2, by = 'interval', all = TRUE)
result <- select(result, c(steps, date, interval))
rm(result_1, result_2)


# Add the artificial completed rows to the original data (complete cases only)

result <- rbind(result, data[complete.cases(data),])
result <- result[order(result[,'date']), ]


#cache the result as clean_data

clean_data <- result
rm(result)


######################


# Compute the number of steps by day 

result <- data[complete.cases(data),]
result <- aggregate(result[ , 'steps'], by = list ( date = result[ , 'date'] ), FUN = sum )
result <- rename( result, steps = 'x' )


# Plot barplot

plot <- ggplot(data = result) + aes(x = date, y = steps)
plot <- plot + ggtitle('Total steps by day') + theme(plot.title = element_text(hjust =  0.5))
plot <- plot + geom_bar(stat = 'identity') + scale_x_date(date_minor_breaks = '1 day')
print(plot)

####################

# Compute the number of steps by day 

result <- data[complete.cases(data),]
result <- aggregate(result[ , 'steps'], by = list ( date = result[ , 'date'] ), FUN = sum )
result <- rename(result, steps = 'x' )
result <- result[rep(row.names(result), result[ , 'steps'] ), ]


# Plot histogram

plot <- ggplot(data = result) + aes(date)
plot <- plot + ggtitle('Total steps by day histogram') + theme(plot.title = element_text(hjust =  0.5))
plot <- plot + geom_histogram(binwidth = 1, colour='blue')
print(plot)

###############

# Compute the centrals tendencies of steps by day 

result <- data[complete.cases(data),]

result_1 <- aggregate(result[ , 'steps'], by = list ( date = result[ , 'date'] ), FUN = mean )
result_1 <- rename( result_1, mean = 'x' )

result_2 <- aggregate(result[ , 'steps'], by = list ( date = result[ , 'date'] ), FUN = median )
result_2 <- rename( result_2, median = 'x' )

result <- merge(result_1, result_2, by = 'date')
result <- melt(result, id.vars = 'date')

rm(result_1, result_2)

# Plot barplot

plot <- ggplot(data = result) + aes(x = date, y = value)
plot <- plot + ggtitle('Steps by day') + theme(plot.title = element_text(hjust =  0.5))
plot <- plot + geom_line(aes(col = variable))
plot <- plot + scale_x_date(date_minor_breaks = '1 day')
print(plot)

#################

# Compute the average steps by 5 minutes intervals

result <- data[complete.cases(data),]
result <- aggregate(result[ , 'steps'], by = list ( interval = result[ , 'interval'] ), FUN = mean )
result <- rename( result, mean = 'x' )


# Plot barplot

plot <- ggplot(data = result) + aes(x = interval, y = mean)
plot <- plot + ggtitle('Average steps by 5 minutes intervals') + theme(plot.title = element_text(hjust =  0.5))
plot <- plot + geom_line()
print(plot)


##################


# Compute the total steps by 5 minutes intervals

result <- data[complete.cases(data),]
result <- aggregate(result[ , 'steps'], by = list ( interval = result[ , 'interval'] ), FUN = sum )
result <- rename( result, steps = 'x' )

max <- max(result[ ,'steps'])
result <- result[ result[ ,'steps'] == max, ]

rm(max)


# Output the result

cat('The interval witch contains the maximum steps is', result[[1]], 'with', result[[2]], 'steps')

##############


#Compute the number of NA

result <- data[is.na(data[ , 'steps']), ]
result <- nrow(result)
cat('There is', result, 'observations with missing values')


#########################################


# Output the result

cat('Here the raw dataset')
head(data)

cat('Here the processed dataset')
head(clean_data)


######################## Question 4


# Compute the number of steps by day from the raw dataset

result_1 <- data[complete.cases(data),]
result_1 <- aggregate(result_1[ , 'steps'], by = list ( date = result_1[ , 'date'] ), FUN = sum )
result_1 <- rename(result_1, steps = 'x' )
result_1 <- result_1[rep(row.names(result_1), result_1[ , 'steps'] ), ]

source <- rep('raw data', nrow(result_1) )
result_1 <- cbind(result_1, source)


# Compute the number of steps by day from the clean dataset

result_2 <- clean_data[complete.cases(clean_data),]
result_2 <- aggregate(result_2[ , 'steps'], by = list ( date = result_2[ , 'date'] ), FUN = sum )
result_2 <- rename(result_2, steps = 'x' )
result_2 <- result_2[rep(row.names(result_2), result_2[ , 'steps'] ), ]

source <- rep('clean data', nrow(result_2) )
result_2 <- cbind(result_2, source)


# Prepare all the data for the plotting

result <- rbind(result_1, result_2)
rm(source, result_1, result_2)

plot1_result <- result


# Plot 1

plot_1 <- ggplot(data = plot1_result) + aes(date, color = source)
plot_1 <- plot_1 + ggtitle('Total steps by day densities') + theme(plot.title = element_text(hjust =  0.5))
plot_1 <- plot_1 + geom_histogram(fill = 'white', binwidth = 1, alpha = 0.5)


###


# Compute the centrals tendencies of steps by day from raw dataset

result <- data[complete.cases(data),]

result_1 <- aggregate(result[ , 'steps'], by = list ( date = result[ , 'date'] ), FUN = mean )
result_1 <- rename( result_1, mean = 'x' )

result_2 <- aggregate(result[ , 'steps'], by = list ( date = result[ , 'date'] ), FUN = median )
result_2 <- rename( result_2, median = 'x' )

result <- merge(result_1, result_2, by = 'date')
result <- melt(result, id.vars = 'date')
rm(result_1, result_2)

plot2_result <- result



# Plot 2

plot_2 <- ggplot(data = plot2_result) + aes(x = date, y = value)
plot_2 <- plot_2 + ggtitle('Steps by day from raw dataset') + theme(plot.title = element_text(hjust =  0.5))
plot_2 <- plot_2 + geom_line(aes(col = variable))
plot_2 <- plot_2 + scale_x_date(date_minor_breaks = '1 day')


###


# Compute the centrals tendencies of steps by day from clean dataset

result <- clean_data[complete.cases(clean_data),]

result_1 <- aggregate(result[ , 'steps'], by = list ( date = result[ , 'date'] ), FUN = mean )
result_1 <- rename( result_1, mean = 'x' )

result_2 <- aggregate(result[ , 'steps'], by = list ( date = result[ , 'date'] ), FUN = median )
result_2 <- rename( result_2, median = 'x' )

result <- merge(result_1, result_2, by = 'date')
result <- melt(result, id.vars = 'date')
rm(result_1, result_2)

plot3_result <- result


# Plot 3

plot_3 <- ggplot(data = plot3_result) + aes(x = date, y = value)
plot_3 <- plot_3 + ggtitle('Steps by day from clean dataset') + theme(plot.title = element_text(hjust =  0.5))
plot_3 <- plot_3 + geom_line(aes(col = variable))
plot_3 <- plot_3 + scale_x_date(date_minor_breaks = '1 day')


###


grid.arrange(plot_1, plot_2, plot_3)


######################## 


# Prepare the dataset

day <- weekdays(clean_data[ , 'date'])
result <- cbind(clean_data, day)
result[ , 'day'] <- gsub('lundi|mardi|mercredi|jeudi|vendredi', 'weekday', result[ , 'day'])
result[ , 'day'] <- gsub('samedi|dimanche', 'weekend day', result[ , 'day'])


# Compute the average steps by 5 minutes intervals

result <- aggregate(result[ , 'steps'], by = list ( interval = result[ , 'interval'], day = result[ , 'day'] ), FUN = mean )
result <- rename( result, mean = 'x' )


# Plot barplot

plot <- ggplot(data = result) + aes(x = interval, y = mean) + facet_grid(.~day)
plot <- plot + ggtitle('Average steps by 5 minutes intervals') + theme(plot.title = element_text(hjust =  0.5))
plot <- plot + geom_line()
print(plot)
