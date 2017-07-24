

# Natalie Cooper July 2017

# Requires ape

break_branches_select_age <- function(tree, number.to.add, min.branch.length = 0.1, 
                                      youngest.date, oldest.date){
  
  # Identify the branches leading to tips
  tip.lengths <- tree$edge.length[which(tree$edge[, 2] < length(tree$tip.label))]
  
  # Loop through adding taxa
  for(i in 1:number.to.add){
    
    # Extract branches to split
    age.data <- make_age_data(tree)
    
    # Select branches that fall into the period
    branch.to.include <- branch_in_period(age.data, youngest.date, oldest.date)
    age.data <- age.data[branch.to.include, ]

    # Exclude small branches
    age.data <- subset(age.data, branch.length >= min.branch.length)
    
    #-------------------------------------------
    # 1. Select branch to split based on length
    #-------------------------------------------
    # Longer branches have higher probability of selection
    # Probability is directly the branch lengths
    add.to.node <- sample(age.data$end.node,
                          size = 1,
                          prob = age.data$branch.length)
    
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
treex <- rtree(20)
plot(treex)
axisPhylo()
nodelabels()

new.tree <- break_branches_select_age(treex, 20, 0.1, 1, 2)
plot(new.tree)
axisPhylo()
nodelabels()