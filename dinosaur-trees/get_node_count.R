# Function to get node counts sensu Sakamoto et al 2016 PNAS
# Input any phylogeny, but only sensible with non ultrametric
# Requires ape
# Natalie Cooper 2017

get_node_count <- function(phy){
  
  # Use nodepath to get all the nodes from all the tips
  # then lapply will allow you to get the length of each
  nodecount <- lapply(ape::nodepath(phy), length)
  
  # Make the nodecount output into a dataframe
  # so we can use it in analyses later
  node.data <- as.data.frame(nodecount)
  
  # Add species names
  names(node.data) <- phy$tip.label

  # Reshape this so the names are a column
  # And the node counts are a column
  node.data <- reshape2::melt(node.data, value.name = "nodecount", 
                              variable.name = "species", id.vars = NULL)
  
  # Finally add the root to tip lengths for each tip
  node.data$time <- ape::node.depth.edgelength(phy)[1:length(phy$tip.label)]
  
  # Return the node.data dataframe
  return(node.data)

}

# Example
mytree <- rtree(50)
nodes <- get_node_count(mytree)
nodes

library(ggplot2)
ggplot(nodes, aes(y = log(nodecount), x = time)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(method = "lm", alpha = 0.2, fill = "cornflowerblue", 
              colour = "cornflowerblue") +
  labs(y = "log node count", x = "Time elapsed (Myr)")
