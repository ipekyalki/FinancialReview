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
str(fin)

#FILTERING

#Filtering by using which() for non-missing data
head(fin)
fin[fin$Revenue == 9746272,] #returns NA values as well
which(fin$Revenue == 9746272) #which ignores NAs
fin[which(fin$Revenue == 9746272),] #picks up the entire line

head(fin)
fin[fin$Employees == 45,]
fin[which(fin$Employee == 45),] 

#Filtering by using is.na() for non-missing data
head(fin, 24)
#----fin$Expenses == NA
#----fin[fin$Expenses == NA,]

is.na() #checks if something is NA

a <- c(1,24,543,NA,76,45,NA)
is.na(a)

is.na(fin$Expenses)
fin[is.na(fin$Expenses),]
fin[is.na(fin$State),]

#Removing records with missing data!!!!
fin_backup <- fin #create a backup of what we have
fin <- fin_backup

fin[!complete.cases(fin),]
fin[is.na(fin$Industry),]
fin[!is.na(fin$Industry),] #check the opposite if this filtered data really doesn't contain NA's in Industry
fin <- fin[!is.na(fin$Industry),] #now, overwrite the previous filtered column to the original column which has NAs
fin

#Resetting the dataframe index
rownames(fin) <- 1:nrow(fin)
tail(fin)

rownames(fin) <- NULL #faster way to reindex your dataset
tail(fin)


#Replacing missing data: Factual Analysis
fin[!complete.cases(fin),]

fin[is.na(fin$State),] #checking missing rows in State column
fin[is.na(fin$State) & fin$City == "New York",]
fin[is.na(fin$State) & fin$City == "New York","State"] <- "NY"
#check if the State has changed:
fin[c(11,377),]

fin[is.na(fin$State) & fin$City == "San Francisco",]
fin[is.na(fin$State) & fin$City == "San Francisco","State"] <- "CA"
fin[c(82,265),]

fin[!complete.cases(fin),]

#Replacing missing data: Median Imputation Method (1)
fin[!complete.cases(fin),]

median(fin[, "Employees"], na.rm=TRUE) #calculates the median of Employees & ignores NA
med_empl_retail <- median(fin[fin$Industry=="Retail", "Employees"], na.rm=TRUE) #calculates the median of Employees which are in Retail Industry & na.rm=TRUE
#Could be used for mean(fin[fin$Industry=="Retail", "Employees"], na.rm=TRUE)
med_empl_retail

fin[is.na(fin$Employees) & fin$Industry == "Retail",]
fin[is.na(fin$Employees) & fin$Industry == "Retail", "Employees"] <- med_empl_retail
#check:
fin[3,]

fin[!complete.cases(fin),]

median(fin[, "Employees"], na.rm=TRUE)
med_empl_financial <- median(fin[fin$Industry=="Financial Services", "Employees"], na.rm=TRUE)
med_empl_financial

fin[is.na(fin$Employees)&fin$Industry=="Financial Services", ]
fin[is.na(fin$Employees)&fin$Industry=="Financial Services", "Employees"] <- med_empl_financial
#check:
fin[330,]

#Replacing missing data: Median Imputation Method (2)
fin[!complete.cases(fin),]
med_empl_constr <- median(fin[fin$Industry=="Construction", "Growth"], na.rm=TRUE)
med_empl_constr

fin[is.na(fin$Growth) & fin$Industry=="Construction",]
fin[is.na(fin$Growth) & fin$Industry=="Construction","Growth"] <- med_empl_constr
#check:
fin[8,]

#Replacing missing data: Median Imputation Method (3)
fin[!complete.cases(fin),]
median(fin[,"Revenue"], na.rm=TRUE)
med_rev_constr <- median(fin[fin$Industry =="Construction","Revenue"], na.rm=TRUE)

fin[is.na(fin$Revenue) & fin$Industry=="Construction","Revenue"] <- med_rev_constr
#check:
fin[8,]

fin[!complete.cases(fin),]
median(fin[,"Expenses"], na.rm=TRUE)
med_exp_constr <- median(fin[fin$Industry =="Construction","Expenses"], na.rm=TRUE)
med_exp_constr

fin[is.na(fin$Expenses) & fin$Industry=="Construction" & is.na(fin$Profit),"Expenses"] <- med_exp_constr
#check:
fin[8,]
fin[!complete.cases(fin),]

#Replacing missing data: deriving values
#Revenue - Expenses = Profit
fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit),"Revenue"] - fin[is.na(fin$Profit),"Expenses"]
fin[c(8,42),]

fin[!complete.cases(fin),]
fin[is.na(fin$Expenses),"Expenses"] <-  fin[is.na(fin$Expenses),"Revenue"] - fin[is.na(fin$Expenses),"Profit"]
fin[15,]

fin[!complete.cases(fin),]
