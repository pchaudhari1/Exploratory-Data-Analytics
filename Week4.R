# Create 3 variables
x <- 1:5
y <- 6:10
z <- 11:15

# Creating a matrix using cbind where x, y and z are columns
cbind(x, y, z)

# Create a matrix using rbind where x, y and z are rows
rbind(x, y, z)

# Creating a matrix with numeric and character columns makes everything a character
cbind(c(1, 2, 3, 4, 5), c("a", "b", "c", "d", "e"))


# Create a matrix of integers 1:10, with 5 rows and 2 columns
matrix(data = 1:10, nrow = 5, ncol = 2)


# With 2 rows and 5 columns
matrix(data = 1:10, nrow = 2, ncol = 5)

# With 2 rows and 5 columns, but fill by row instead of columns
matrix(data = 1:10, nrow = 2, ncol = 5, byrow = TRUE)

# Create a survey data dataframe
survey <- data.frame("index" = c(1, 2, 3, 4, 5),
                     "sex" = c("m", "m", "m", "f", "f"),
                     "age" = c(99, 46, 23, 54, 23))
survey
View(survey)

# Show the structure of the survey dataframe
str(survey)

# Creating a dataframe of survey data WITHOUT factors
survey <- data.frame("index" = c(1, 2, 3, 4, 5),
                     "sex" = c("m", "m", "m", "f", "f"),
                     "age" = c(99, 46, 23, 54, 23),
                     stringsAsFactors = FALSE)

# Print the result (it looks the same as before)
survey

# Look at the structure: no more factors!
str(survey)

# head() shows the first 6 rows
head(ChickWeight)

# tail() shows he bottom 6 rows
tail(ChickWeight)

# View() opens the entire dataframe in a new window
View(ChickWeight)

# Print summary statistics of ToothGrowth to the console
summary(ToothGrowth)

# Show the structure of ToothGrowth
str(ToothGrowth)


# names of columns in the ToothGrowth dataframe
names(ToothGrowth)

# Return the len column of ToothGrowth
ToothGrowth$len

# mean of the len column of ToothGrowth
mean(ToothGrowth$len)

# table of the supp column of ToothGrowth.
table(ToothGrowth$supp)

# Len and supp columns of ToothGrowth
head(ToothGrowth[c("len", "supp")])

# Create a new dataframe called survey
survey <- data.frame("index" = c(1, 2, 3, 4, 5),
                     "age" = c(24, 25, 42, 56, 22))

#Print survey
survey


# Add a new column called sex to survey
survey$sex <- c("m", "m", "f", "f", "m")

# survey with new sex column
survey  


# Change name of 1st column of df to "a"
names(df)[1] <- "a"

# Change name of 2nd column of df to "b"
names(df)[2] <- "b"

# Change the name of the first column of survey to "participant.number"
names(survey)[1] <- "participant.number"
survey


# Change the column name from age to age.years
names(survey)[names(survey) == "age"] <- "years"
survey

# Return rows 1-6 and column 1 of ToothGrowth
ToothGrowth[1:6, 1]

# Return rows 1-3 and columns 1 and 3 of ToothGrowth
ToothGrowth[1:3, c(1,3)]

# Return 1st row (and all columns) of ToothGrowth
ToothGrowth[1, ]

# give 2nd column (and all rows) of ToothGrowth
ToothGrowth[, 2]

# Create a new df with only the rows of ToothGrowth where supp equals VC
ToothGrowth.VC <- ToothGrowth[ToothGrowth$supp == "VC", ]

# Create a new df with only the rows of ToothGrowth where supp equals OJ and dose < 1
ToothGrowth.OJ.a <- ToothGrowth[ToothGrowth$supp == "OJ" &
                                  ToothGrowth$dose < 1, ]

# Get rows of ToothGrowth where len < 20 AND supp == "OJ" AND dose >= 1
subset(x = ToothGrowth, subset = len < 20 &
         supp == "OJ" & dose >= 1)

# Get rows of ToothGrowth where len > 30 AND supp == "VC", but only return the len and dose columns
subset(x = ToothGrowth,
       subset = len > 30 & supp == "VC",
       select = c(len, dose))

# What is the mean tooth length of Guinea pigs given OJ?
# Step 1: Create a subsettted dataframe called oj
oj <- subset(x = ToothGrowth = supp == "OJ")

# Step 2: Calculate the mean of the len column from the new subsetted dataset
mean(oj$len)

mean(ToothGrowth$len[ToothGrowth$supp == "OJ"])

health <- data.frame("age" = c(32, 24, 43, 19, 43),
                     "height" = c(1.75, 1.65, 1.50, 1.92, 1.80),
                     "weight" = c(70, 65, 62, 79, 85))
# Print health
health

# Calculate bmi
health$weight / health$height ^ 2

# Save typing by using with()
with(health, height / weight ^ 2)

# Long code
health$weight + health$height / health$age + 2 * health$height

# Short code that does the same thing
with(health, weight + height / age + 2 * height)

#=================================================================#
#Vectors

library(tidyverse)
# Determine type 
typeof(letters)

typeof(1:10)

# Determine length of vector
x <- list("a", "b", 1:10)
length(x)

# Checking number divisible by 3
1:10 %% 3 == 0

c(TRUE, TRUE, FALSE, NA)

# Checking type
typeof(1)

typeof(1L)

1.5L

# Square root
x <- sqrt(2) ^ 2
x

x - 2

c(-1, 0, 1) / 0

# Create a variable x
x <- "This is a reasonably long string."

# Install pryr package
install.packages("pryr")
pryr::object_size(x)

y <- rep(x, 1000)
y
pryr::object_size(y)

# Missing values
NA          
NA_integer_   
NA_real_   
NA_character_ 


# Create a vector
x <- sample(20, 100, replace = TRUE)
y <- x > 10

# how many are greater than 10?
sum(y)  

# what proportion are greater than 10?
mean(y) 

# If loop
if (length(x)) {
  # do something
}

# Checking data type
typeof(c(TRUE, 1L))

typeof(c(1L, 1.5))

typeof(c(1.5, "a"))

# Scalars and recycling rules
sample(10) + 100

runif(10) > 0.5

1:10 + 1:2

1:10 + 1:3

#Creating a tibble
tibble(x = 1:4, y = 1:2)

tibble(x = 1:4, y = rep(1:2, 2))

tibble(x = 1:4, y = rep(1:2, each = 2))

#Naming vectors
c(x = 1, y = 2, z = 4)

set_names(1:3, c("a", "b", "c"))

# Subsetting
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]

x[c(1, 1, 5, 5, 5, 2)]

#NEgative values drop specifed positions
x[c(-1, -3, -5)]

#USing negatve and positive values 
x[c(1, -1)]

x[0]


x <- c(10, 3, NA, 5, 8, 1, NA)

# All non-missing values of x
x[!is.na(x)]

# All even (or missing!) values of x
x[x %% 2 == 0]

#subsetting with character vector
x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

#Recursive vectors
#crete a list
x <- list(1, 2, 3)
x

#structure of x
str(x)

x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

y <- list("a", 1L, 1.5, TRUE)
str(y)

z <- list(list(1, 2), list(3, 4))
str(z)

#Visualising lists
x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

#Sub-setting
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

#extracts a sub-list
str(a[1:2])

# extract a single component from list
str(a[4])

str(a[[1]])

str(a[[4]])

#Using $ to extract named elements from list
a$a

a[["a"]]

#Attributes
x <- 1:10
attr(x, "greeting")
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)

as.Date

methods("as.Date")

getS3method("as.Date", "default")

getS3method("as.Date", "numeric")

#Factors
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)

attributes(x)

# Date and date-times
x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
typeof(x)
attributes(x)

attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x

#POSIXIt
y <- as.POSIXlt(x)
typeof(y)
attributes(y)

#Tibbles
tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)
attributes(tb)

#Create a datatframe
df <- data.frame(x = 1:5, y = 5:1)
typeof(df)
attributes(df)
