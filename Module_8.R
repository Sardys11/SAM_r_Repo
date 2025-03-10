


#install.packages("pryr")
# require(pryr)
# require(ISLR)
# require(boot)
#install.packages("plyr")
library(data.table)
library(plyr)
library(pryr)


#--STEP 1--Read file from computer via prompt

Student_assignment_6<- read.table("Data/Assignment 6 Dataset.txt", header = TRUE, sep=",")

Student <-Student_assignment_6
Student

#Using ddply() to transform the dataset and compute the Grade Average.

StudentAverage <- ddply(Student,"Sex",transform,Grade.Average=mean(Grade)) # mean(Grade) grouped by Sex, 

StudentAverage

####### why this?? 
sex <- Student$Sex  # Sex is categorical!!!!!

sex
mean(sex) # This doesn't work.Here is why: In mean.default(sex) : argument is not numeric or logical: returning NA

#mean(Sex) will not work unless Sex is encoded numerically. Instead, table(Student$Sex) or summary(Student$Sex) would be better to inspect the category distribution.
#It could be use like this:
# Compute mean grade grouped by Sex

StudentAveragebySex <- ddply(Student_assignment_6, "Sex", summarise, Grade.Average = mean(Grade))

StudentAveragebySex # another way to look at the data

###################################################

#--STEP 2  Convert the data set to a dataframe for names whos' name contains the letter i, then create a new data set with those names,
#------------Write those names to a file separated by comma’s (CSV)
students <-StudentAverage

i_students <- subset(students, grepl("i", students$Name, ignore.case = TRUE))

i_students

write.table(i_students, "Data/Students_Gendered_Mean") # this command makes the file looks like a csv, but the default separator is a space. By default, write.table() includes row names, which means an extra column with row numbers will be added

write.csv(i_students, "Data/Filtered_Students.csv", row.names = FALSE) # this command actually creates a csv file. write.csv() automatically sets sep="," (comma separator). And row.names = FALSE ensures that no extra index column is added.

###################################################

#--STEP 3 Write the filtered data set and convert it to CSV file

# The hint mentions file.choose(), which allows users to interactively select a file location.
# Instead of hardcoding the file path like in Step #2, I might need to modify the write.csv() function to prompt the "user" for a save location:
# Let's try this:

write.csv(i_students, file = file.choose(), row.names = FALSE)
i_students <- subset(students, grepl("i", students$Name, ignore.case = TRUE))

head(i_students)

