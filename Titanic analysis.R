# Load packages
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggthemes) # theme_few()
library(scales) # dollar_format() function
library(mice) # imputation
library(randomForest) # classification algorithm

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
# qplot(x=Var1, y=Var2, data=melt(cor(titanic)), fill=value, geom="tile") + 
#   scale_fill_gradient2(limits=c(-1, 1))

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
# Passengers 62 and 830 are missing Embarkment
full_data[c(62, 830), "Embarked"]

# We will infer their values for embarkment based on present data that we can imagine may be
# relevant: passenger class and fare. We see that they paid $ 80 and $ NA respectively and
# their classes are 1 and NA . So from where did they embark?
full_data[c(62, 830), ]

# Get rid of missing passenger IDs
embark_fare <- full_data %>% 
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, and median fare
ggplot(embark_fare, aes(x=Embarked, y=Fare, fill=factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), color="red", linetype="dashed", lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# From the visualization I see that the median fare for a first class passenger departing
# from Charbourg (‘C’) coincides nicely with the $80 paid by our embarkment-deficient
# passengers.
# Since their fare was $80 for 1st class, they most likely embarked from "C"
full_data$Embarked[c(62, 830)] <- "C"

# Passenger on row 1044 has an NA Fare value
full_data[1044, ]

# This is a third class passenger who departed from Southampton (‘S’). Let’s visualize
# Fares among all others sharing their class and embarkment (n = 494).
ggplot(full_data[full_data$Pclass == "3" & full_data$Embarked=="S", ], 
       aes(x = Fare)) +
  geom_density(fill="#99d6ff", alpha=0.4) +
  geom_vline(aes(xintercept=median(Fare, na.rm=T)), color="red", linetype="dashed",
             lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

# From this visualization, it seems quite reasonable to replace the NA Fare value with
# the median for their class and embarkment which is $8.05.
full_data$Fare[1044] <- median(full_data[full_data$Pclass == "3" & full_data$Embarked=="S", ]$Fare,
                               na.rm=T)

# There are also a lot of missing age values
# Show number of missing age values
sum(is.na(full_data$Age)) # 263

# We could definitely use rpart (recursive partitioning for regression) to predict missing ages,
# but I’m going to use the mice package for this task just for something different.
# Make variables into factors
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Surname",
                 "Family", "FsizeD")

full_data[factor_vars] <- lapply(full_data[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables
mice_mod <- mice(full_data[, !names(full_data) %in% c("PassengerId", "Name", "Ticket",
                                                      "Cabin", "Family", "Surname", "Survived")],
                 method="rf")

# Save the complete output
mice_output <- complete(mice_mod)

# Let’s compare the results we get with the original distribution of passenger ages to ensure
# that nothing has gone completely awry.
# Plot age distributions
par(mfrow=c(1,2))
hist(full_data$Age, freq=F, main="Age: Original Data", col="lightgreen", ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main="Age: MICE Output", col="blue", ylim=c(0,0.04))

# Things look good, so replace age vector in the original data with the output from the mice model
full_data$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(full_data$Age))

# FEATURE ENGINEERING ROUND 2
# Now that we know everyone’s age, we can create a couple of new age-dependent variables:
# Child and Mother. A child will simply be someone under 18 years of age and a mother is
# a passenger who is 1) female, 2) is over 18, 3) has more than 0 children (no kidding!),
# and 4) does not have the title ‘Miss’.
# First we'll look at the relationship between age and survival
ggplot(full_data[1:891, ], aes(Age, fill=factor(Survived))) + 
  geom_histogram() +
  # Including sex since we know (a priori) it's a significant predictor
  facet_grid(. ~ Sex) +
  theme_few()

# Create the column Child and indicate whether child or adult
full_data$Child[full_data$Age < 18] <- "Child"
full_data$Child[full_data$Age >= 18] <- "Adult"

# Show counts
table(full_data$Child, full_data$Survived)
# It looks like being a child doesn't hurt but doesn't necessarily save you either

# Add Mother variable
full_data$Mother <- "Not Mother"
full_data$Mother[full_data$Sex == "female" & full_data$Parch > 0 & full_data$Age > 18
                 & full_data$Title != "Miss"] <- "Mother"

# Show counts
table(full_data$Mother, full_data$Survived)

# Finish by factorizing the two new factor variables
full_data$Child <- factor(full_data$Child)
full_data$Mother <- factor(full_data$Mother)

# Double check to make sure there is no more missing data
md.pattern(titanic)
