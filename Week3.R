# Load the library readr to read the csv file into R
library(readr)

# Reading the csv file
epa<-read_csv("data/US EPA data 2017.csv")


# Rename the columns by removing the spaces between variable names
names(epa)<-make.names(names(epa))

# Check the number of rows and columns
nrow(epa)
ncol(epa)

# Show the structure of the epa file
str(epa)

# see the top and bottom rows of the data. 
# By default returns only top 6 and bottom 6 rows
head(epa[,c(6:7,10)])
tail(epa[,c(6:7,10)])

#Checking n's
#Create table using one of the columns
table(epa$Parameter.Name)
table(epa$Date.of.Last.Change)

table(epa$Latitude)


# Load the library dplyr
library(dplyr)

# Filter the epa data for using the latitude of 30.497478
# Observation count, State code, Sample Duration, Arithmetic Mean
filter(epa, Latitude == "30.497478") %>%
  select(State.Name, County.Name,Date.of.Last.Change,
         Observation.Percent, Sample.Duration )


# Filter the data on State.Code 1
filter(epa, State.Code == "01") %>% 
  select(County.Code, Observation.Count, Observation.Percent,
         Sample.Duration) %>% 
  as.data.frame

# Select the number of states defined in the dataset
# Returns 54 states when there are only 50 states in USA
select(epa, State.Name) %>% unique %>%  nrow

# Display unique State names
#District of Columbia is a Federal district and not a state and Puerto Rico
#is and unofficial state.
unique(epa$State.Name)

#Validation
summary(epa$Observation.Count)

# Looking at deciles of the data
quantile(epa$Observation.Count, seq(0, 1, 0.1))

# Ranking the states and counties by Observation.Count
ranking <- group_by(epa, State.Name, County.Name) %>%
     summarize(epa = mean(epa$Observation.Count)) %>%
     as.data.frame %>%
     arrange(desc(epa))
ranking
# view head ranking
head(ranking)

# View  bottom 10 tail ranking
tail(ranking,10)

# number of observations for Colbert county in Alabama state
filter(epa, State.Name == "Alabama" & County.Name == "Colbert") %>% nrow

#Convert date variable into a Date class to see ozone variations in a year
epa<-mutate(epa, Date.of.Last.Change = as.Date(Date.of.Last.Change))

#split the data by month to look at the average hourly levels
filter(epa, State.Name == "Alabama" & County.Name == "Colbert") %>%
         mutate(month = factor(months(Date.of.Last.Change), levels = month.name)) %>%
         group_by(month) %>%
         summarize(epa = mean(Observation.Count))

#Filter data for Fairhope county in Alabama
filter(epa, State.Name == "Alabama" & County.Name == "Fairhope") %>% nrow

filter(epa, State.Name == "Alabama" & County.Name == "Fairhope") %>%
          mutate(month = factor(months(Date.of.Last.Change), levels = month.name)) %>%
          group_by(month) %>%
          summarize(epa = mean(Observation.Count))


# Challenge the solution
#Create a new dataset ozone2 using resampled indices.
set.seed(10234)
N <- nrow(ozone)
idx <- sample(N, N, replace = TRUE)
epa2 <- epa[idx, ]

# Reconstruct rankings of countries based on resampled data
ranking2 <- group_by(ozone2, State.Name, County.Name) %>%
         summarize(ozone = mean(Observation.Count)) %>%
         as.data.frame %>%
         arrange(desc(ozone))

# Compare rankings based on resampled data
cbind(head(ranking, 10),
         head(ranking2, 10))

cbind(tail(ranking, 10),
          tail(ranking2, 10))




