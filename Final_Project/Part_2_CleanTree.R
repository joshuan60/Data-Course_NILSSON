library(ape)
library(tidyverse)

# Reading in our files
matches     <- read_csv("Matched_Tips.csv")
astral_tree <- read.tree("Tree_dat/10outgroups_4854insects_824genes_ASTRAL_rooted_final.tree")
ml_tree     <- read.tree("Tree_Dat/10outgroups_4854insects_824genes_MLbest_rooted.tree")

# Identify tips to keep 
# Only keep tips that have GS data — outgroups included only if they matched
tips_to_keep <- matches$tip_names_in_data_matrix

# This function prunes the tree while leaving the rooted structure
prune_tree <- function(tree, keep_tips) {
  tips_to_drop <- tree$tip.label[!tree$tip.label %in% keep_tips]
  
  if (length(tips_to_drop) == 0) {
    message("No tips to drop.")
    return(tree)
  }
  
  pruned <- drop.tip(tree, tips_to_drop, trim.internal = TRUE)
  message(paste("Dropped:", length(tips_to_drop), "tips"))
  message(paste("Remaining:", length(pruned$tip.label), "tips"))
  return(pruned)
}

# Prune the tree
ml_pruned     <- prune_tree(ml_tree, tips_to_keep)

# Verify the tree is still rooted
is.rooted(ml_pruned)

# Output our updated tree
write.tree(ml_pruned, "ML_pruned.tree")
