# Variable names and types
var_names <- c("educ", "cigprice", "white", "age", "income",
               "cigs", "law", "lincome", "agesq", "lcigpric")
col_classes <- c("numeric", "numeric", "NULL", "integer", "integer",
                 "integer", "integer", "NULL", "NULL", "NULL")

# Read in the data
db <- read.table("data/SMOKE.raw", header = FALSE, strip.white = TRUE,
                 col.names = var_names, colClasses = col_classes)

# Save the vote dataset in a csv file.
write.csv(db, "data/smoke.csv", row.names = FALSE)

