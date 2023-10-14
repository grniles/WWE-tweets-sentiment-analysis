# Load libraries
library(readr)
library(dplyr)
library(plyr)
library(ggformula)
library(ggplot2)
library(xts)

# Load csvs
raw1 = read_csv("raw_for_r.csv")
head(raw1)
sd1 = read_csv("sd_for_r.csv")
head(sd1)
raw2 = read_csv("raw2_for_r.csv")
raw2
sd2 = read_csv("sd2_for_r.csv")
sd2

# T-test on week 1
raw1sent <- raw1$Sentiment
sd1sent <- sd1$Sentiment
t.test(raw1sent, sd1sent)

# T-test on week 2
raw2sent <- raw2$Sentiment
sd2sent <- sd2$Sentiment
t.test(raw2sent, sd2sent)

# Aggregate Raw week 1 
aggRaw1 <- aggregate(.~raw1$Time, data=raw1, mean)
aggRaw1$name <- "Raw"

# Plot Raw week 1 aggregate
raw1Col <- gf_col(aggRaw1$Sentiment ~ aggRaw1$Time, color = "red")
raw1Col

# Aggregate Smackdown week 1
aggSd1 <- aggregate(.~sd1$Time, data=sd1, mean)
aggSd1$name <- "SD"

# Plot Smackdown week 1
sd1Col <- gf_col(aggSd1$Sentiment ~ aggSd1$Time, color = "dark blue")
sd1Col

# Aggregate Raw week 2
aggRaw2 <- aggregate(.~raw2$Time, data=raw2, mean)
aggRaw2$name <- "Raw"

# Plot Raw week 2 aggregate
raw2Col <- gf_col(aggRaw2$Sentiment ~ aggRaw2$Time, color = "red")
raw2Col

# Aggregate Smackdown week 2
aggSd2 <- aggregate(.~sd2$Time, data=sd2, mean)
aggSd2$name <- "SD"

# Plot smackdown week 2 aggregate
sd2Col <- gf_col(aggSd2$Sentiment ~ aggSd2$Time, color = "dark blue")
sd2Col

# Bind raw and smackdown aggregates and plot the mean sentiment of each
week1 <- rbind.fill(aggRaw1, aggSd1)
week1plot <- ggplot(week1, aes(Time, Sentiment, fill = name)) + geom_bar(stat = "identity") +
  ggtitle("Mean Sentiment Raw vs Smackdown Week 1")
week1plot

# Bind raw and smackdown aggregates for week 2 and plot the mean sentiment of each
week2 <- rbind.fill(aggRaw2, aggSd2)
week2plot <- ggplot(week2, aes(Time, Sentiment, fill = name)) + geom_bar(stat = "identity") +
  ggtitle("Mean Sentiment Raw vs Smackdown Week 2")
week2plot

# plot Raw week 1 tweets/min
ggplot(raw1, aes(Time, fill=Sentiment)) + 
  geom_histogram(bins=200, color = "red", fill="red") +
  ggtitle("Number of Tweets per Minute Raw 12/3/18")

# plot Smackdown week 1 tweets/min
ggplot(sd1, aes(Time, fill=Sentiment)) + 
  geom_histogram(bins=180, color = "blue", fill="blue") +
  ggtitle("Number of Tweets per Minute Smackdown 12/4/18")

# plot Raw week 2 tweets/min
ggplot(raw2, aes(Time, fill=Sentiment)) + 
  geom_histogram(bins=140, color = "red", fill="red") +
  ggtitle("Number of Tweets per Minute Raw 12/10/18")

# Plot smackdown week 2 tweets/min
ggplot(sd2, aes(Time, fill=Sentiment)) +
  geom_histogram(bins=130, color = "blue", fill="blue") +
  ggtitle("Number of Tweets per Minute Smackdown 12/11/18")
