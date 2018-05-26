getwd() #take a look at the working directory, if it's not correct, use setwd() to define the correct wd

#import the dataset with read.csv(), remember this function works only for csv files, 
#to import an xlsx file you should first install "xlsx" package and then use read.xlsx() function to import it
#Basic import: fin <- read.csv("Future-500.csv") 
fin <- read.csv("Future-500.csv", na.strings=c("")) 
#take a look at the structure of the data to get a glimpse of it
head(fin,20)
tail(fin, 10)
str(fin)
summary(fin)

#convert ID and Inception from integer to factor
fin$ID <- factor(fin$ID)
summary(fin)
str(fin)

fin$Inception <- factor(fin$Inception)
summary(fin)
str(fin)

#Factor Variable Trap (FVT)
#Conversion from characters to numeric
a <- c("12","11", "10", "14", "15")
a
typeof(a)
b <- as.numeric(a)
b
typeof(b)

#Conversion from factors to numeric
z <- factor(c("12","12","14","15","16"))
z
typeof(z)
y <- as.numeric(z)
y  #y comes out as the factor numbers of z
typeof(y)
#so what R does is that it assigns numeric numbers like 1 2 3 to each category 
#that's why we see these y values when we convert from factor to numeric

#------------ Correct way:
x <- as.numeric(as.character(z))
x

#FVT Example:
head(fin)
str(fin)
#the goal is to convert revenue and expenses from factors to numeric
#must get rid of $ and commas but instead of introducing new functions to convert these variables
#convert profit variable from numeric to factor(artificially) and analyze FVT effect 

#fin$Profit <- factor(fin$Profit) Dangerous

head(fin)
str(fin)
summary(fin)

#fin$Profit <- as.numeric(fin$Profit) Dangerous
str(fin)
head(fin)

#use sub() and gsub() functions to make Revenue, Expense and Growth non-factor variables
?sub;?gsub #check out what they are used for
fin$Expenses <- gsub(" Dollars", "", fin$Expenses)
fin$Expenses <- gsub(",", "", fin$Expenses)
head(fin)
str(fin) #Note that now Expenses variable has become a chr

fin$Revenue <- gsub("\\$","", fin$Revenue) #use backslash to escape from special character $
fin$Revenue <- gsub(",","", fin$Revenue)
head(fin)
str(fin) #Note that now Revenue variable has become a chr
#Note: look up online for the escape sequence for the special characters

fin$Growth <- gsub("%","", fin$Growth)
head(fin)
str(fin)

#gsub function has already converted the variable from factor to character, 
#all we need to do now is to convert them to numeric
fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)
head(fin)
str(fin)

summary(fin)

#Explore what an NA is..

?NA #seems like it is a logical constant like TRUE and FALSE

TRUE #corresponds to 1
FALSE #corresponds to 0
NA #corresponds to a missing value

NA == TRUE
NA == FALSE
NA == 15
NA == NA

#Locating missing data
#Updated import to : fin <- read.csv("Future-500.csv", na.strings=c("")) 
head(fin, 24)
complete.cases(fin) #checks if there are any missing values in each row 
fin[!complete.cases(fin),] #grab the rows with missing values
#some NA's are placed inside of brackets -> notice that they all belong to a factor variable