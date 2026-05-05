## Build the cvi_example dataset shipped with the package.
##
## This script is run once to (re)generate data/cvi_example.rda. It is not
## part of the installed package — `data-raw/` is excluded via .Rbuildignore.
##
## To rebuild: source("data-raw/cvi_example.R")

# Simulated content validity ratings for a 10-item depression screening
# instrument, rated by 6 expert clinicians on a 4-point relevance scale:
#   1 = not relevant
#   2 = somewhat relevant
#   3 = quite relevant
#   4 = highly relevant
#
# Items reflect typical DSM-style depression symptom dimensions. The pattern
# of ratings is realistic: a few items show universal agreement, most show
# strong but imperfect agreement, and a couple of items would be flagged for
# revision based on standard CVI cutoffs.

cvi_example <- matrix(
  c(
    # item1  item2  item3  item4  item5  item6  item7  item8  item9  item10
    4,     3,     3,     2,     3,     4,     3,     4,     2,     4,     # expert1
    4,     4,     3,     3,     2,     4,     3,     4,     3,     4,     # expert2
    4,     4,     4,     3,     3,     4,     2,     4,     2,     3,     # expert3
    4,     4,     3,     4,     3,     3,     3,     4,     3,     4,     # expert4
    4,     3,     4,     3,     4,     4,     3,     4,     3,     4,     # expert5
    4,     4,     3,     3,     2,     4,     4,     3,     2,     4      # expert6
  ),
  nrow = 6,
  byrow = TRUE,
  dimnames = list(
    paste0("expert", 1:6),
    paste0("item", 1:10)
  )
)

# Save into data/ as a compressed .rda
usethis::use_data(cvi_example, overwrite = TRUE)
