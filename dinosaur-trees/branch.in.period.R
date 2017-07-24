# Extracting edges within a time period
# Natalie Cooper 2017

#------------------------------------------------------------------
# Get node/branch ages for the tree and output dataframe
#------------------------------------------------------------------

make_age_data <-function(tree){
  
  age.data <- data.frame(array(dim = c(length(tree$edge.length), 6)))
  colnames(age.data) <- c("edge.no", "start.node", "end.node", 
                          "age.start.node", "age.end.node", "branch.length")
  
  # Add edge and node numbers
  age.data$edge.no <- 1:length(tree$edge.length)
  age.data$start.node <- tree$edge[, 1]
  age.data$end.node <- tree$edge[, 2]
  
  # Extract ages of all nodes  
  node.ages <- picante::node.age(tree)
  
  # Enter ages of end nodes (easy as this is output of node.age, but 
  # need to minus from the maximum age to make sense timeline wise)
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

  # Add branch lengths
  age.data$branch.length <- age.data$age.start.node - age.data$age.end.node

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

branch_in_period <- function(age.data, youngest.date, oldest.date){
  which((age.data$age.start.node > youngest.date & age.data$age.start.node < oldest.date) |
          (age.data$age.end.node > youngest.date & age.data$age.end.node < oldest.date) |
          (age.data$age.end.node < youngest.date & age.data$age.start.node > oldest.date))
}
