# Get the current working directory
getwd()
# Set the working directory
setwd(dir = "/Users/one/Desktop/ANLY 506-51/Code Portfolio")

# Lists all the objects in the workspace
ls()

# Printing something not present in the current workspace
study3.df

# Creating some random objects
study1.df <- data.frame(id = 1:5, 
                        sex = c("m", "m", "f", "f", "m"), 
                        score = c(51, 20, 67, 52, 42))

score.by.sex <- aggregate(score ~ sex, 
                          FUN = mean, 
                          data = study1.df)

study1.htest <- t.test(score ~ sex, 
                       data = study1.df)

# Saving the above objects as a new .RData file in the data folder of current working directory
save(study1.df, score.by.sex, study1.htest,
     file = "data/study1.RData")

# Saving workspace to complete_image.RData in the data folder of the working directory
save.image(file = "data/projectimage.RData")

# Load the object in study1.RData into my workspace
load(file = "data/study1.RData")

# Loading objects in projectimage.RData into my workspace
load(file = "data/projectimage.RData")

# Remove a huge file like huge.df from workspace
rm(huge.df)

# Remove all the objects from workspace
rm(list = ls())

# Writing the pirates dataframe to a tab-delimited
#  text file named pirates.txt in the working directory
write.table(x = pirates,
            file = "pirates.txt",  # Save the file as pirates.txt
            sep = "\t")            # Make the columns tab-delimited

# Write the pirates dataframe to a tab-delimited
#  text file named pirates.txt to the desktop
write.table(x = pirates,
            file = "Users/nphillips/Desktop/pirates.txt",  # Save the file as pirates.txt to my desktop
            sep = "\t")    

# Read a tab-delimited text file named mydata.txt 
#  from the data folder in the working directory into
#  R and store as a new object called mydata
mydata <- read.table(file = 'data/mydata.txt',    # file is in a data folder in my working directory
                     sep = '\t',                  # file is tab--delimited
                     header = TRUE,               # the first row of the data is a header row
                     stringsAsFactors = FALSE)    # do NOT convert strings to factors!!

# Read a text file from the web
fromweb <- read.table(file = 'http://goo.gl/jTNf6P',
                      sep = '\t',
                      header = TRUE)

# Print the result of fromweb
fromweb

# To read a non-text file into R
install.packages("foreign")

# To read Excel files
install.packages("xlsx")

# Test your R Might!
a <- data.frame("sex" = c("m", "f", "m"),
                "age" = c(19, 43, 25),
                "favorite.movie" = c("Moon", "The Goonies", "Spice World"))
b <- mean(a$age)
b
c <- table(a$sex)
c

