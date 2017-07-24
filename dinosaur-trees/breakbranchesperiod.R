# Function that takes a phylogenetic tree and adds taxa to branches
# randomly selected from those in a certain time period
# Can vary the number of taxa to add
# Natalie Cooper July 2017

# This code allows you to add taxa to new branches added as you go along
# Not sure if it should be restricted to the original branches on the tree

# Requires ape and "bind.tree.new" function

library(ape)
source("bind.tree.new.R")

break_branches_period <- function(tree, number.to.add, youngest.date, oldest.date){

  # Identify the branches leading to tips
  tip.lengths <- tree$edge.length[which(tree$edge[,2] < length(tree$tip.label))]
  
  # Loop through adding trees  
  for (i in 1:number.to.add){

    # Extract the nodes of branches in the time period of interest
    branch.in.period <- which(node.depth.edgelength(tree) > youngest.date
                          & node.depth.edgelength(tree) < oldest.date)
    
    

    #-------------------------------------------
    # 1. Select branch to split based on length
    #-------------------------------------------
    #Longer branches have higher probability of selection
    # Probability is directly the branch lengths
    add.to.node <- sample(branch.in.period, size = 1,
                          prob = tree$edge.length[which(match(tree$edge[,2], 
                                                 branch.in.period, nomatch =0) != 0)])

    #----------------------------------------------
    # 2. Determine point where the split will occur
    #-----------------------------------------------
    # First get the length of the branch to split
    # This will be the maximum split point
    length.branch <- tree$edge.length[which(tree$edge[, 2] ==  add.to.node)]

    # Use uniform distribution to select split point 
    # Need to avoid split being at end of branch, so max has small 
    # arbitrary value taken off it. Likewise min is not zero but 0.01
    split.point <- runif(n = 1, max = (length.branch - 0.01), 
                     min = 0.01)

    # Could change this to be the 25th percentile or whatever
    #------------------------------------------------------
    # 3. Determine characteristics of branch/species to add
    #------------------------------------------------------
    # Species name is Taxon_1 etc.
    species.to.add <- paste("Taxon", i, sep = "_")

    # Branch lengths are sampled from existing branch lengths
    # that lead to tips in the tree before manipulation
    # to hopefully get realistic values
    branch.length <- sample(tip.lengths, size = 1)

    #------------------------------------------------------
    # 4. Create subtrees and add
    #------------------------------------------------------
    branch.length <- tree$edge.length[add.to.node]
    
    tree.to.add <- paste("(", species.to.add, ":", branch.length, ");", sep="")
    tree.to.add <- read.tree(text = tree.to.add) 

    try(tree <- bind.tree.new(tree, tree.to.add, where = add.to.node, 
                          position = branch.length))

  }
  # Quick clean up to remove any negative branch lengths introduced
  tree$edge.length[tree$edge.length < 0] <- 0
  return(tree)
}

# Example
# treex <- rtree(20)
# plot(treex)

# new.tree <- break_branches_period(treex, 20, 0.1, 0.5)
# plot(newt)
# lines(x = c(0.1, 0.1), y = c(0, 100), col = "red")
# lines(x = c(0.5, 0.5), y = c(0, 100), col = "blue")
                      