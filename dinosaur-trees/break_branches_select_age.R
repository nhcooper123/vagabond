

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
    
    # Extract data just for this node
    node.data <- age.data[age.data$end.node == add.to.node, ] 
    
    #----------------------------------------------
    # 2. Determine point where the split will occur
    #-----------------------------------------------
    # Use uniform distribution to select split point 
    # Split points vary depending on node placement
    # 1. Focal node and the start of the branch are inside time period
    if(node.data$age.start.node < oldest.date & node.data$age.end.node > youngest.date){
      split.point <- runif(n = 1, max = node.data$branch.length, min = 0)
    }
    
    # 2. Focal node is inside time period, but start of the branch is outside
    if(node.data$age.start.node > oldest.date & node.data$age.end.node > youngest.date){
      split.point <- runif(n = 1, max = (oldest.date - node.data$age.end.node), min = 0)
    }
    
    # 3. Focal node is outside time period, but start of the branch is inside
    if(node.data$age.start.node < oldest.date & node.data$age.end.node < youngest.date){
      split.point <- runif(n = 1, max = node.data$branch.length, min = (youngest.date - node.data$age.end.node))
    }    
    
    # 4. Both nodes are outside - branch spans the time period
    if(node.data$age.start.node > oldest.date & node.data$age.end.node < youngest.date){
      split.point <- runif(n = 1, max = (oldest.date - node.data$age.end.node), min = (youngest.date - node.data$age.end.node))
    }    
    
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

new.tree <- break_branches_select_age(treex, number.to.add = 10, min.branch.length = 0.5, 
                                      youngest.date = 1, oldest.date = 2)
plot(new.tree)
axisPhylo()
# Add lines to show where time period is - note that this doesn't line up properly...
lines(c(1.7,1.7), c(0,100), col = "red")
lines(c(2.7,2.7), c(0,100), col = "red")
