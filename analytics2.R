library(dplyr)
library(ggplot2)
library(tidyr)

ywc <- read.csv("outputs/yearwordcount.csv", stringsAsFactors = FALSE)

# Presidential Word Counts:
temp <- ywc %>%
  filter(Word == "irresponsible" | Word == "transgressors" | Word == "punish") %>%
  merge(read.csv("outputs/pdfurls.csv"), by="Year")
ggplot(temp, aes(Year, Count, color=Word)) +
  geom_line() +
  geom_point(size=2) +
  labs(y="Word Appearances")

# Finding Interesting Words:
temp <- ywc %>%
  group_by(Word) %>%
  summarize(Wordcount = sum(Count)) %>%
  arrange(desc(Wordcount), Word) %>%
  head(100)

# Continents
temp <- ywc %>%
  filter(Word == "asia" | Word == "africa" | Word == "australia" | Word == "europe" | Word == "america" | Word == "antarctica") %>%
  merge(read.csv("outputs/pdfurls.csv"), by="Year")
ggplot(temp, aes(Year, Count, color=Word)) +
  geom_line() +
  geom_point(size=2) +
  labs(y="Word Appearances")

# Lengths
lengths <- ywc %>%
  group_by(Year) %>%
  summarize(yearcount = sum(Count)) %>%
  merge(read.csv("outputs/pdfurls.csv"), by="Year")

# Continents, controlling for lengths
temp <- ywc %>%
  filter(Word == "asia" | Word == "africa" | Word == "australia" | Word == "europe" | Word == "america" | Word == "antarctica") %>%
  merge(lengths, by="Year") %>%
  mutate(Freq = Count/yearcount)
ggplot(temp, aes(Year, Freq, color=Word)) +
  geom_line() +
  geom_point(size=2) +
  labs(y="Word Frequencies")

# War and Peace
temp <- ywc %>%
  filter(Word == "war" | Word == "peace") %>%
  merge(lengths, by="Year") %>%
  mutate(Freq = Count/yearcount)
ggplot(temp, aes(Year, Freq, color=Word)) +
  geom_line() +
  geom_point(size=2) +
  labs(y="Word Frequencies")

# (War and Peace Scatterplot)
temp <- ywc %>%
  filter(Word == "war" | Word == "peace") %>%
  merge(lengths, by="Year") %>% 
  mutate(Freq = Count/yearcount) %>%
  select(-Count) %>%
  spread(Word, Freq)
ggplot(temp, aes(peace, war)) +
  geom_point() +
  stat_smooth(method = "lm")
summary(lm(temp$peace ~ temp$war))

# Terrorism vs. Freedom
temp <- ywc %>%
  filter(Word == "terrorism" | Word == "freedom") %>%
  merge(lengths, by="Year") %>%
  mutate(Freq = Count/yearcount)
ggplot(temp, aes(Year, Freq, color=Word)) +
  geom_line() +
  geom_point(size=2) +
  labs(y="Word Frequencies")

