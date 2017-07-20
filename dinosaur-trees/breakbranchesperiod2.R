# Function that takes a phylogenetic tree and adds taxa to branches
# randomly selected from those in a certain time period
# Can vary the number of taxa to add
# Natalie Cooper July 2017

# This code allows you to add taxa to new branches added as you go along
# Not sure if it should be restricted to the original branches on the tree

# Requires ape and "bind.tree.new" function

#------------------------------------------------------------------
# Get node/branch ages for the tree and output dataframe
#------------------------------------------------------------------

make_age_data <-function(tree){

  age.data <- data.frame(array(dim = c(length(tree$edge.length), 5)))
  colnames(age.data) <- c("edge.no", "start.node", "end.node", 
                        "age.start.node", "age.end.node")

  # Add edge and node numbers
  age.data$edge.no <- 1:length(tree$edge.length)
  age.data$start.node <- tree$edge[, 1]
  age.data$end.node <- tree$edge[, 2]

  # Extract ages of all nodes
  node.ages <- picante::node.age(tree)

  # Enter ages of end nodes (easy as this is output of node.age, but 
  # need to minus fomr the maximum age to make sense timeline wise)
  age.data$age.end.node <- max(node.ages$ages) - node.ages$ages 

  # Enter ages of start nodes (harder)
  for(i in 1:length(age.data$edge.no)){
    # Give root value of 0
    if(age.data$start.node[i] == length(node.ages$tip.label) + 1){
       age.data$age.start.node[i] <- max(node.ages$ages) - 0
    }else{
      # If not dealing with root, assign ages as for end.nodes    
      age.data$age.start.node[i] <- max(node.ages$ages) -
      (node.ages$ages[node.ages$edge[, 2] == age.data$start.node[i]])
    }
  }
return(age.data)
}

#---------------------------------------------------------------------
# Identify edges within a set period
# This includes branches that start outside the period, but end inside,
# branches that start inside the period, but end outside,
# branches that start and end inside
# and branches that span the whole duration
#---------------------------------------------------------------------

branch.in.period <- function(age.data, youngest.date, oldest.date){
  which((age.data$age.start.node > youngest.date & age.data$age.start.node < oldest.date) |
      (age.data$age.end.node > youngest.date & age.data$age.end.node < oldest.date) |
      (age.data$age.end.node < youngest.date & age.data$age.start.node > oldest.date))
}

#---------------------------
# Break branches
#---------------------------

library(ape)
source("bind.tree.new.R")

break_branches_period <- function(tree, number.to.add, youngest.date, oldest.date){

  # Identify the branches leading to tips
  tip.lengths <- tree$edge.length[which(tree$edge[,2] < length(tree$tip.label))]
  
  # Give tree nodes names
  tree$edge.no <- as.character(1:length(tree$edge[,1]))
  
  # Extract branches to split
  age.data <- make_age_data(tree)
  branch.to.include <- branch.in.period(age.data, youngest.date, oldest.date)
  
  # Loop through adding trees  
  for (i in 1:length(branch.to.include)){
    
    #-------------------------------------------
    # 1. Select branch to split at
    #-------------------------------------------
    
    add.to.node <- tree$edge[, 1][which(tree$edge.no == tree$edge.no[branch.to.include[i]])]
    print(add.to.node)
    
    #----------------------------------------------
    # 2. Determine point where the split will occur
    #-----------------------------------------------
    # First get the length of the branch to split
    # This will be the maximum split point
    length.branch <- tree$edge.length[which(tree$edge[, 2] ==  add.to.node)]
    
    # Use uniform distribution to select split point 
    # Need to avoid split being at end of branch, so max has small 
    # arbitrary value taken off it. Likewise min is not zero but 1e-10
    split.point <- runif(n = 1, max = (length.branch - 1e-10), 
                         min = 1e-10)
    
    
    #if (tree$edge.length[tree$edge[, 2] == add.to.node] < branch.length){
   #   branch.length <- tree$edge.length[tree$edge[, 2] == add.to.node]
    #  message(paste("Branch length is too long to add", species.to.add, "here. 
   #                 New branch length = ", branch.length))
   # }
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
    #-----------------------------------------------------
    tree.to.add <- paste("(", species.to.add, ":", branch.length, ");", sep="")
    tree.to.add <- read.tree(text = tree.to.add) 
    
    print(i)
    try(tree <- bind.tree(tree, tree.to.add, where = add.to.node, 
                          position = split.point))
    plot.phylo(tree, main = paste(i, add.to.node, sep = "_"))
    
  }
  # Quick clean up to remove any negative branch lengths introduced
  # tree$edge.length[tree$edge.length < 0] <- 0
  return(tree)
}

    
new.tree <- break_branches_period(sq, 20, 0.05, 0.08)    



    

    














    
    

    #-------------------------------------------
    # 1. Select branch to split based on length
    #-------------------------------------------
    #Longer branches have higher probability of selection
    # Probability is directly the branch lengths
    add.to.edge <- sample(branch.in.period, size = 1,
                          prob = tree$edge.length[branch.in.period])
    
    add.to.node <- tree$edge[, 1][add.to.edge]

    #----------------------------------------------
    # 2. Determine point where the split will occur
    #-----------------------------------------------
    # First get the length of the branch to split
    # This will be the maximum split point
    length.branch <- tree$edge.length[add.to.node]

    # Use uniform distribution to select split point 
    # Need to avoid split being at end of branch, so max has small 
    # arbitrary value taken off it. Likewise min is not zero but 1e-10
    split.point <- runif(n = 1, max = (length.branch - 1e-10), 
                     min = 1e-10)

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
    species.to.add <- paste("Taxon", i, sep = "_")

    tree.to.add <- paste("(", species.to.add, ":", branch.length, ");", sep="")
    tree.to.add <- read.tree(text = tree.to.add) 

    try(tree <- bind.tree(tree, tree.to.add, where = add.to.node, 
                          position = split.point))

  }
  # Quick clean up to remove any negative branch lengths introduced
  # tree$edge.length[tree$edge.length < 0] <- 0
  return(tree)
}

# Example
# treex <- rtree(20)
# plot(treex)

# new.tree <- break_branches_period(treex, 20, 0.1, 0.5)
# plot(newt)
# lines(x = c(0.1, 0.1), y = c(0, 100), col = "red")
# lines(x = c(0.5, 0.5), y = c(0, 100), col = "blue")






                      