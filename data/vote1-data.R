# Variable names and types
var_names <- c("state", "district", "democA", "voteA", "expendA",
               "expendB", "prtystrA", "lexpendA", "lexpendB", "shareA")
col_classes <- c("factor", "integer", "numeric", "numeric", "numeric",
                 "numeric", "numeric", "NULL", "NULL", "NULL")

# Read in the data
vote1 <- read.table("data/Vote1.raw", header = FALSE, strip.white = TRUE,
                    col.names = var_names, colClasses = col_classes)

# From the information in the original data set, create variables
# for the vote count of democrat candidates, the expenditures of
# democrat and republican candidates, and the party strength of
# the democrat candidate.
vote <- within(subset(vote1, select = c(state, district)), {
  str_dem <- ifelse(vote1$democA == 1, vote1$prtystrA, 100 - vote1$prtystrA)
  exp_rep <- ifelse(vote1$democA == 1, vote1$expendB, vote1$expendA)
  exp_dem <- ifelse(vote1$democA == 1, vote1$expendA, vote1$expendB)
  vote_dem <- ifelse(vote1$democA == 1, vote1$voteA, 100 - vote1$voteA)
})

# Save the vote dataset in a csv file.
write.csv2(vote, "data/vote.csv", row.names = FALSE)


