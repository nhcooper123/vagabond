

break_branches <- function(tree, number.to.break, min.branch.length = 0.1){

  # Identify the branches leading to tips
  tip.lengths <- tree$edge.length[which(tree$edge[,2] < length(tree$tip.label))]
  
  # Loop through adding trees
  for(i in 1:number.to.break){

    #-------------------------------------------
    # 1. Select branch to split based on length
    #-------------------------------------------
    # Longer branches have higher probability of selection
    # Probability is directly the branch lengths
    add.to.node <- sample(which(tree$edge.length > min.branch.length),
                          size = 1,
                          prob = (tree$edge.length[which(tree$edge.length > min.branch.length)]))

    #----------------------------------------------
    # 2. Determine point where the split will occur
    #-----------------------------------------------
    # First get the length of the branch to split
    # This will be the maximum split point
    length.branch <- tree$edge.length[add.to.node]

    # Use uniform distribution to select split point 
    # Need to avoid split being at end of branch, so max has small 
    # arbitrary value taken off it. Likewise min is not zero but 0.01
    split.point <- runif(n = 1, max = (length.branch - 0.01), 
                         min = 0.01)

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

    try(tree <- bind.tree.new(tree, tree.to.add, where = add.to.node, 
                      position = split.point))
      
    }
  
  # Quick clean up to remove any negative branch lengths introduced
  tree <- tree$edge.length[tree$edge.length < 0] <- 0
  return(tree)
}

# Example
# treex <- rtree(20)
# plot(treex)
  
# new.tree <- break_branches(treex, 20, 0.1)
# plot(newt)
