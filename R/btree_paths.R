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
#' mytree <- data.table(NodeId=c(1, 2, 3), ParentNodeId=c(NA, 1, 1))
#' btree_paths(mytree)

btree_paths <- function(btree){
  # Returns a vector of paths, "L" representing traversal to the left and "R" representing traversal to the right

  #--------------------------------------------------
  # Check input

  if(!inherits(btree, "btree"))
    stop("btree must be a btree object")

  #--------------------------------------------------

  # Get the root node
  parents <- btree[is.na(ParentNodeId)]
  parents[, Path := ""]

  parentsList <- list(parents)
  while(nrow(parents) > 0){

    # Get the children
    leftChildren <- btree[parents[, list(Path, LeftChildNodeId)], on=c("NodeId"="LeftChildNodeId"), nomatch=0]
    rightChildren <- btree[parents[, list(Path, RightChildNodeId)], on=c("NodeId"="RightChildNodeId"), nomatch=0]

    # Mark the paths
    leftChildren[, Path := paste0(Path, "L")]
    rightChildren[, Path := paste0(Path, "R")]

    # make the new parents
    parents <- rbind(leftChildren, rightChildren, use.names=TRUE)

    # Insert new parents into parentsList
    parentsList <- c(parentsList, list(parents))
  }

  # rbind parentsList into btree
  newbtree <- rbindlist(parentsList)

  # Sort the rows of newbtree to match the given btree
  newbtree <- newbtree[btree[, list(NodeId)], on="NodeId"]

  return(newbtree$Path)
}
