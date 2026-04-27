library(tidyverse)
library(ape)
library(BAMMtools)

# Reading in our files
matches <- read_csv("Matched_Tips.csv")
ml_tree <- read.tree("ML_pruned.tree")

# Finding our optimal lambda smoothing parameter for the ML tree
l <- 10^(-1:6)
cv_ml <- numeric(length(l))
for(i in 1:length(l)){
  print(i)
  cv_ml[i] <- sum(attr(chronopl(ml_tree, lambda = l[i], CV = TRUE), "D2"))
}
plot(l, cv_ml)

cal_ml  <- makeChronosCalib(ml_tree, node = "root", age.min = 459, age.max = 459)
ML.tree <- chronos(ml_tree, lambda = l[which.min(cv_ml)],
                   model = "correlated", calibration = cal_ml)
is.ultrametric(ML.tree)
write.tree(ML.tree, file = "MLTree.tree")
plot(ML.tree)
# Create named numeric vector of GS values
# Names must exactly match tip labels in the tree
gs_vector <- setNames(matches$gs_value, matches$tip_names_in_data_matrix)

# Quick sanity check — confirm names match tree tips
all(names(gs_vector) %in% ML.tree$tip.label)       # should return TRUE

# Set BAMM priors
setBAMMpriors(phy = ML.tree,     traits = gs_vector, outfile = "ml_priors.txt")

# Export trait file in BAMM format
# Two columns, tab-separated, no header: tip label and gs value
gs_bamm <- matches |>
  select(tip_names_in_data_matrix, gs_value)

write.table(gs_bamm, 
            file      = "insect_gs_bamm.txt",
            sep       = "\t",
            row.names = FALSE,
            col.names = FALSE,
            quote     = FALSE)
