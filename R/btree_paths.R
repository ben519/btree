#' @title
#' btree Paths
#'
#' @description
#' Given a btree, produce a vector of strings indicating the directed steps to take starting at the root to get to each node
#'
#' @details
#' Returns a character vector whose values correspond to each node in the btree
#'
#' @param btree A btree
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' mytree <- data.table(NodeID=c(1, 2, 3), ParentNodeID=c(NA, 1, 1))
#' btree_paths(mytree)

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
