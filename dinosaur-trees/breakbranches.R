

break_branches <- function(tree, number.to.break, min.branch.length = 0.1){

  # Identify the branches leading to tips
  tip.lengths <- tree$edge.length[which(tree$edge[,2] < length(tree$tip.label))]
  
  # Identify branches that are long enough to split
  # And their branch lengths
  branch.sample <- which(tree$edge.length > min.branch.length)
  branch.sample.lengths <- tree$edge.length[branch.sample]
  
  # Loop through adding trees
  for(i in 1:number.to.break){
  print(i)
    #-------------------------------------------
    # 1. Select branch to split based on length
    #-------------------------------------------

    # Longer branches have higher probability of selection
    # Probability is directly the branch lengths
    add.to.node <- sample(which(tree$edge.length > min.branch.length),
                          size = 1,
                          prob = (tree$edge.length[which(tree$edge.length > min.branch.length)]*10))

    #----------------------------------------------
    # 2. Determine point where the split will occur
    #-----------------------------------------------
    # First get the length of the branch to split
    # This will be the maximum split point
    length.branch <- tree$edge.length[which(tree$edge[,2] == add.to.node)]

    # Use uniform distribution to select split point  
    split.point <- runif(n = 1, max = (length.branch-0.01), 
                         min = (min.branch.length-0.01))

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

treex <- rtree(20)
  plot(treex)
  nodelabels()
  
newt <- break_branches(treex, 20, 0.1)
plot(newt)
nodelabels()

I think somethign wrong with the branch length probabilities and also the lengths htemselves

Error in if (position < 0) position <- 0 : 
  missing value where TRUE/FALSE needed
In addition: Warning message:
  In runif(n = 1, max = (length.branch - 0.01), min = (min.branch.length -  :
                                                         NAs produced