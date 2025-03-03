# Module 7 - R Object: S3 vs. S4 assignment
# Student: Sardys Avile-Martinez
#Cleaner Code for the S3 and S4 code,  plesae use Module_7.R to load the data and clean the data as well.


# S3 Example
# Define S3 constructor
wildfire_s3 <- function(df, row_index) {
  row <- df[row_index, ]
  obj <- list(
    Incident_ID = as.character(row$Incident_ID),
    Location = as.character(row$Location),
    Area_Burned = row$Area_Burned_Acres_,
    Homes_Destroyed = row$Homes_Destroyed,
    Cause = as.character(row$Cause)
  )
  class(obj) <- "wildfire_s3"
  return(obj)
}

# Create an S3 object
fire1 <- wildfire_s3(df, 1)
summary(fire1)  # Calls the custom S3 summary method
show(fire1) # This show is to compare with S4

######################################################


#S4 Example
# Define S4 class
setClass("WildfireS4", slots = list(
  Incident_ID = "character",
  Location = "character",
  Area_Burned = "numeric",
  Homes_Destroyed = "numeric",
  Cause = "character"
))

# Define S4 constructor
wildfire_s4 <- function(df, row_index) {
  row <- df[row_index, ]
  new("WildfireS4",
      Incident_ID = as.character(row$Incident_ID),
      Location = as.character(row$Location),
      Area_Burned = row$Area_Burned_Acres_,
      Homes_Destroyed = row$Homes_Destroyed,
      Cause = as.character(row$Cause))
}

# Create an S4 object
fire2 <- wildfire_s4(df, 1)
summary(fire2)  # using summary so to compare with S3. This gives more details than S3
show(fire2)  # Calls the S4 show() method

