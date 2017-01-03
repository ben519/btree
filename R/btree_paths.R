btree_paths <- function(btree){
  # Returns a vector of paths, "0" representing traversal to the left and "1" representing traversal to the right

  # Get the root node
  parents <- btree[is.na(ParentNodeID)]
  parents[, Path := ""]

  parentsList <- list(parents)
  while(nrow(parents) > 0){

    # Get the children
    leftChildren <- btree[parents[, list(Path, LeftChildNodeID)], on=c("NodeID"="LeftChildNodeID"), nomatch=0]
    rightChildren <- btree[parents[, list(Path, RightChildNodeID)], on=c("NodeID"="RightChildNodeID"), nomatch=0]

    # Mark the paths
    leftChildren[, Path := paste0(Path, "l")]
    rightChildren[, Path := paste0(Path, "r")]

    # make the new parents
    parents <- rbind(leftChildren, rightChildren, use.names=TRUE)

    # Insert new parents into parentsList
    parentsList <- c(parentsList, list(parents))
  }

  # rbind parentsList into btree
  newbtree <- rbindlist(parentsList)

  # Sort the rows of newbtree to match the given btree
  newbtree <- newbtree[btree[, list(NodeID)], on="NodeID"]

  return(newbtree$Path)
}
