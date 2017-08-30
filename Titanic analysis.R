# Load packages
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggthemes)

# Import data
train <- read.csv("train.csv", stringsAsFactors=F)
test <- read.csv("test.csv", stringsAsFactors=F)

full_data <- bind_rows(train, test)

# Examine data
glimpse(full_data)

# Create dataframe called titanic
titanic <- full_data[c(1:891), ]

# Examine first 6 records of titanic
head(titanic)

# Look at some summary data
summary(titanic) # Age has a min of 0.42

# Plot age to take a look
ggplot(titanic, aes(Age)) +
  geom_bar()
# Several values under 1, which are children under the age of 1

# Correlation matrix that doesn't work
qplot(x=Var1, y=Var2, data=melt(cor(titanic)), fill=value, geom="tile") + 
  scale_fill_gradient2(limits=c(-1, 1))

# Density plot
ggplot(titanic, aes(Age, fill=Sex)) +
  geom_density(alpha=0.3) +
  facet_wrap(~ Survived)

# FEATURE ENGINEERING
# Grab title from passenger names
full_data$Title <- gsub('(.*, )|(\\..*)', '', full_data$Name)

# Show title counts by sex
table(full_data$Sex, full_data$Title)

# Combine titles with very low cell counts to a "rare" title
rare_title <- c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer",
                "Lady", "Major", "Rev", "Sir", "the Countess")

# Reassign mlle, ms, and mme accordingly
full_data$Title[full_data$Title == "Mlle"] <- "Miss"
full_data$Title[full_data$Title == "Ms"] <- "Miss"
full_data$Title[full_data$Title == "Mme"] <- "Mrs"
full_data$Title[full_data$Title %in% rare_title] <- "Rare Title"

# Show title counts by sex again
table(full_data$Sex, full_data$Title)

# Grab surnames
full_data$Surname <- sapply(full_data$Name,
                            function(x) strsplit(x, split = '[, .]')[[1]][1])

# Calculate total unique surnames
nlevels(factor(full_data$Surname)) # 866

# Create a family size variable including the passenger themselves
full_data$Fsize <- full_data$SibSp + full_data$Parch + 1

# Create a family variable
full_data$Family <- paste(full_data$Surname, full_data$Fsize, sep="_")

# Plot a relationship between family size and survival with ggplot2
ggplot(full_data[1:891, ], aes(x=Fsize, fill=factor(Survived))) + 
  geom_bar(stat="count", position="dodge") +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x="Family Size") +
  theme_few()

# Discretize family size
full_data$FsizeD[full_data$Fsize == 1] <- "singleton"
full_data$FsizeD[full_data$Fsize < 5 & full_data$Fsize > 1] <- "small"
full_data$FsizeD[full_data$Fsize > 4] <- "large"

# Show family size by survival using a mosaic plot
mosaicplot(table(full_data$FsizeD, full_data$Survived),
           main="Family Size by Survival",
           shade=TRUE)

# Look at values in the passenger cabin
full_data %>% 
  select(Cabin) %>% 
  group_by(Cabin) # a lot of missing values

# Create a Deck variable, which is the first character of the Cabin
full_data$Deck <- factor(sapply(full_data$Cabin,
                                function (x) strsplit(x, NULL)[[1]][1]))

# Possibly more could be done, like looking into cabins with multiple rooms

# Analyze by pivoting features
# Pclass
titanic %>% 
  select(Pclass, Survived) %>% 
  group_by(Pclass) %>% 
  summarize(avg = mean(Survived))

# Sex
titanic %>% 
  select(Sex, Survived) %>% 
  group_by(Sex) %>% 
  summarize(avg = mean(Survived))

# SibSp
titanic %>% 
  select(SibSp, Survived) %>% 
  group_by(SibSp) %>% 
  summarize(avg = mean(Survived))

# Parch
titanic %>% 
  select(Parch, Survived) %>% 
  group_by(Parch) %>% 
  summarize(avg = mean(Survived))


# MISSINGNESS