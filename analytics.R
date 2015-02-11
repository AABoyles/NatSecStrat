library(dplyr)
library(ggplot2)

ywc <- read.csv("outputs/yearwordcount.csv", stringsAsFactors = FALSE)

# Presidential Word Counts:
temp <- ywc %>%
  group_by(Year) %>%
  summarize(yearcount = sum(Count)) %>%
  merge(read.csv("outputs/pdfurls.csv"), by="Year")
ggplot(temp, aes(Year, yearcount)) +
  geom_bar(stat="identity", aes(fill=President)) +
  labs(y="Word Count")

# Presidential Distinct Word Counts:
temp <- ywc %>%
  group_by(Year) %>%
  summarize(yearcount = n()) %>%
  merge(read.csv("outputs/pdfurls.csv"), by="Year")
ggplot(temp, aes(Year, yearcount)) + 
  geom_bar(stat="identity", aes(fill=President)) +
  labs(y="Distinct Words")

# Word Frequency Distribution
temp <- ywc %>%
  group_by(Word) %>%
  summarize(Wordcount = sum(Count)) %>%
  arrange(desc(Wordcount), Word)
temp <- cbind(temp, Rank=1:nrow(temp))
ggplot(temp, aes(x=Rank, y=Wordcount)) +
  geom_line() +
  labs(x="Word Rank", y="Appearance Count")

# Log-Log Word Frequency Distribution
ggplot(temp, aes(x=log(Rank), y=log(Wordcount))) +
  geom_point() +
  stat_smooth(method="lm") +
  labs(x="log(Word Rank)", y="log(Appearance Count)")

# Cumulative Word Frequency Distribution
temp <- cbind(temp, Cum=cumsum(temp$Wordcount))
ggplot(temp, aes(x=Rank, y=Cum)) +
  geom_line(color="Red") +
  geom_abline(slope = sum(temp$Wordcount)/nrow(temp)) +
  labs(x="Word", y="Cumulative Appearance Count")
