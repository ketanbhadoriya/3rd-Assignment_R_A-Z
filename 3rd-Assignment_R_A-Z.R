#Start of the Script

#Loading the Data

fin <- read.csv("Future-500.csv",
                na.strings = c("")) #fin -> Finance Data

#Exploring the Data
head(fin)

tail(fin)

str(fin)

colnames(fin)

summary(fin)

#Chaning from non-factor to factor

fin$ID <- factor(fin$ID)

fin$Inception <- factor(fin$Inception)

str(fin)

#Removing the Unnecesscary Characters

fin$Expenses <- gsub(" Dollar","",fin$Expenses)
fin$Expenses <- gsub(",","",fin$Expenses)

head(fin$Expenses)

fin$Expenses <- gsub("s","",fin$Expenses)
head(fin$Expenses)

fin$Revenue <- gsub("\\$","",fin$Revenue)
fin$Revenue <- gsub(",","",fin$Revenue)
str(fin)

fin$Growth <- gsub("%","",fin$Growth)
str(fin)

#Converting to Numeric
fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)

str(fin)

summary(fin)

###Dealing with Missing Data

#We updated import statement to : fin <- read.csv("Future-500.csv",
                                  #na.strings = c(""))
fin[!complete.cases(fin),]

#Filtering using which() for non-missing data
#fin[fin$Revenue == 9746272,] -> Gives NA

fin[which(fin$Revenue == 9746272),]

#Filtering using is.na() for missing data

head(fin,24)

fin[is.na(fin$Expenses),]

#Removing Records with missing data

#Creating Backup of Data
fin_backup <- fin
fin <- fin_backup

fin[!complete.cases(fin),]

fin <- fin[!is.na(fin$Industry),]

#Resetting the DatFrame Index

rownames(fin) <- 1:nrow(fin)
nrow(fin)
head(fin,20)

#Replacing Missing Data : Factual Analysis Method

fin[!complete.cases(fin),]

#Subset of rows with NAs in State Columns

nrow(fin)
fin[is.na(fin$State),]
fin[c(11,82,265,377),] <- fin %>%
                 filter(is.na(State)) %>%
                 mutate(State = c("NY","CA","CA","NY"))
 nrow(fin)
 
 fin[is.na(fin$State),]

 #Another way of doing the same is : 
 #fin[is.na(fin$State) & fin$City == "New York","State"] <- "NY"
 
fin[!complete.cases(fin),]

#Relacing Missing Data : Median Imputation Method

retail_val <- median(fin[fin$Industry =="Retail","Employees"],na.rm = TRUE)
fs_val <- median(fin[fin$Industry =="Financial Services","Employees"],na.rm = TRUE)

fin[is.na(fin$Employees) & fin$Industry == "Retail","Employees"] <- retail_val
fin[is.na(fin$Employees) & fin$Industry == "Financial Services","Employees"] <- fs_val

fin[is.na(fin$Employees),]

fin$Employess <- NULL

fin[!complete.cases(fin),]

#Median Imputation Method Part 2
fin[is.na(fin$Growth),]
med_growth_val <- median(fin[fin$Industry =="Construction","Growth"],na.rm = TRUE)

fin[is.na(fin$Growth) &fin$Industry=="Construction","Growth"] <- med_growth_val

fin[is.na(fin$Growth),]

fin[!complete.cases(fin),]

med_revenue_val <- median(fin[fin$Industry =="Construction","Revenue"],na.rm = TRUE)
fin[is.na(fin$Revenue) &fin$Industry=="Construction","Revenue"] <- med_revenue_val

fin[!complete.cases(fin),]

med_expenses_val <- median(fin[fin$Industry =="Construction","Expenses"],na.rm = TRUE)
fin[is.na(fin$Expenses) & fin$Industry=="Construction","Expenses"] <- med_expenses_val

fin[!complete.cases(fin),]

#Replacing Missing Data : Deriving Values Method
fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit),"Revenue"] - fin[is.na(fin$Profit),"Expenses"]

fin[is.na(fin$Profit),]

fin[!complete.cases(fin),]

fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses),"Revenue"] - fin[is.na(fin$Expenses),"Profit"]

fin[is.na(fin$Expenses),]

fin[!complete.cases(fin),]

#Visualizing the Cleaned Data
library(ggplot2)
head(fin)
ggplot(data=fin,aes(x=Revenue,y=Expenses,color=Industry,size=Profit))+
  geom_point()

ggplot(data=fin,aes(x=Revenue,y=Expenses,color=Industry))+
  geom_point()+
  geom_smooth(fill = NA, size=1.2)

ggplot(data=fin,aes(x=Industry,y=Growth,color=Industry)) +
geom_boxplot(size=1.2,aplha=0.5,outlier.colour = NA)+
geom_jitter()
