#Loading library tidyverse
library(tidyverse)

# Tidy data
# Representing the same underlying data in multiple ways
table1
table2
table3

# Spread across two tibbles
table4a  # cases
table4b  # population

# Compute rate per 10,000
table1 %>% mutate(rate = cases / population * 10000)

# Compute cases per year
table1 %>% count(year, wt = cases)

# Visualise changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

#Gathering
table4a

# Gather those columns into a new pair of variables
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

# Drop the gathered columns to get new key and value columns
table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")

# Combine the tidied versions of table4a and table4b into a single tibble, 
# using dplyr::left_join()
tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)

# Spreading
table2

# Using spread()
table2 %>%
  spread(key = type, value = count)

# Separating and uniting
# Separate
# separate() pulls apart one column into multiple columns, 
# by splitting wherever a separator character appears
table3

# separate() takes the name of the column to separate, 
# and the names of the columns to separate into
# Split into two variables
table3 %>% 
  separate(rate, into = c("cases", "population"))

# Make cases and population as character columns
table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)

#USe seperate()  to separate the last two digits of each year
table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)

# Unite
# Unite() to combine multiple columns into a single column 
table5 %>% 
  unite(new, century, year)

# Sep argument
table5 %>% 
  unite(new, century, year, sep = "")

# Missing values
# Changing the representation of a dataset brings up an important subtlety of missing values
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

# Make the implicit missing value explicit by putting years in the columns
stocks %>% 
  spread(year, return)

# Set na.rm = TRUE in gather() to turn explicit missing values implicit
stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`:`2016`, na.rm = TRUE)

# Using complete() to mak missing values explicit in tidy data
stocks %>% 
  complete(year, qtr)

# Usin tribble to handle missing values
treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

# Fill the missing values with fill()
treatment %>% 
  fill(person)

# Case Study
who

# Focusing on the values that are present using na.rm to 
who1 <- who %>% 
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)
who1

# Get structure of values in the key column
who1 %>% 
  count(key)

# replace the characters “newrel” with “new_rel
who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

# Split the codes at each underscore
who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")
who3

# Drop the new column because it’s constant in this dataset
who3 %>% 
  count(new)

# Drop iso2 and iso3 since they’re redundant
who4 <- who3 %>% 
  select(-new, -iso2, -iso3)

# Separate sexage into sex and age by splitting after the first character
who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)
who5

# The who dataset is now tidy!
who %>%
  gather(key, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

# Religion Income

### Missing Values
# Load the required libraries
library(tidyverse)
library(readr)
library(knitr)
library(Hmisc)
install.packages("missForest")
library(missForest)

#Load iris data
data("iris")

# Set missing values ( 10% )
iris.mis <- prodNA(iris, noNA = 0.1)
kable(summary(iris.mis))

# Print missing values
print(paste0("Missing Values:", sum(is.na(iris.mis)==TRUE)))


# Impute with mean value
iris.mis$imputed_age <- with(iris.mis, impute(Sepal.Length, mean))

# Impute using argImpute
impute_arg <- aregImpute(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width +
                           Species, data = iris.mis, n.impute = 5)

# Impute_arg
impute_arg$imputed$Sepal.Length # check

religion = read_csv("data/religion_income.csv")

## Gather data from wide to long table
kable(religion)

long <- religion %>% 
  gather(income, freq, 2:11)
kable(long[1:10,])

