# Accessing Twitter data from MongoDB


library(mongolite)

options(mongodb = list(
  "host" = "140.226.57.147",
  "username" = "kimchon",
  "password" = "CkMj1527"
))
databaseName <- "twitterdb"
collectionName <- "twitter_search"

# saveData <- function(data) {
#   # Connect to the database
#   db <- mongo(collection = collectionName,
#               url = sprintf(
#                 "mongodb://%s:%s@%s/%s",
#                 options()$mongodb$username,
#                 options()$mongodb$password,
#                 options()$mongodb$host,
#                 databaseName))
#   # Insert the data into the mongo collection as a data.frame
#   data <- as.data.frame(t(data))
#   db$insert(data)
# }

loadData <- function() {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Read all the entries
  data <- db$find()
  data
}


data <- loadData()
