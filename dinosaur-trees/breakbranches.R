# Function that takes a phylogenetic tree and adds taxa to branches
# Probability of adding a taxon is increased if the branch is longer
# Break points are taken from a uniform distribution
# Can vary the number of taxa to add, and change the smallest branch to break
# Natalie Cooper July 2017

# This code allows you to add taxa to new branches added as you go along
# Not sure if it should be restricted to the original branches on the tree

# Requires ape

break_branches <- function(tree, number.to.add, min.branch.length = 0.1){

  # Identify the branches leading to tips
  tip.lengths <- tree$edge.length[which(tree$edge[, 2] < length(tree$tip.label))]
  
  # Loop through adding trees
  for(i in 1:number.to.add){

    #-------------------------------------------
    # 1. Select branch to split based on length
    #-------------------------------------------
    # Longer branches have higher probability of selection
    # Probability is directly the branch lengths
    add.to.node <- sample(tree$edge[, 2][which(tree$edge.length > min.branch.length)],
                          size = 1,
                          prob = tree$edge.length[which(tree$edge.length > min.branch.length)])

    #----------------------------------------------
    # 2. Determine point where the split will occur
    #-----------------------------------------------
    # First get the length of the branch to split
    # This will be the maximum split point
    length.branch <- tree$edge.length[which(tree$edge[, 2] ==  add.to.node)]

    # Use uniform distribution to select split point 
    # Bounded by 0 and branch length being split
    split.point <- runif(n = 1, max = length.branch, min = 0)

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

    tree.to.add <- paste("(", species.to.add, ":", branch.length, ");", sep="")
    tree.to.add <- read.tree(text = tree.to.add) 

    tree <- bind.tree(tree, tree.to.add, where = add.to.node, 
                      position = split.point)
      
    }

  return(tree)
}

# Example
# treex <- rtree(20)
# plot(treex)
  
# new.tree <- break_branches(treex, 20, 0.1)
# plot(newt)
