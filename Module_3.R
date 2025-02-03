# Module 3
# Student: Sardys Avile-Martinez
# Assignment # 3
### Professor notes:
#       "The data set below is based on the presidential election during 2016, where it outlined the name of the candidate, 
          # the source of the poll (ABC vs, CBS). Discuss your result in your blog. Important note, 
          # I made up this data, so this data does not reflect what really happened in the election."


# Let's create a Data frame and then analyze it 

# Creating the data frame
poll_data <- data.frame(
  Name = c("Jeb", "Donald", "Ted", "Marco", "Carly", "Hillary", "Bernie"),
  ABC_Poll = c(4, 62, 51, 21, 2, 14, 15),
  CBS_Poll = c(12, 75, 43, 19, 1, 21, 19)
)

# Print the data frame
print(poll_data)

# Getting the ABC results
poll_data$ABC_Poll

# Getting the CBS results
poll_data$CBS_Poll

# Let's see Jeb's results
poll_data[1, ]

# Let's see the results excluding the names
poll_data[ ,2:3]

# Summary Statistics
summary(poll_data)

# Data visualization
barplot(
  height = rbind(poll_data$ABC_Poll, poll_data$CBS_Poll),
  beside = TRUE,
  names.arg = poll_data$Name,
  col = c("blue", "red"),
  legend.text = c("ABC Poll", "CBS Poll"),
  main = "2016 Presidential Poll Results (Fictional)",
  xlab = "Candidates",
  ylab = "Polling Percentage"
)

# Let's analyze the poll discrepancies ABC vs CBS

poll_data$Difference <- abs(poll_data$ABC_Poll - poll_data$CBS_Poll)
print(poll_data)


