#' @title
#' Sub btree
#'
#' @description
#' Extract a sub-btree from either an existing btree, or to a specified depth
#'
#' @details
#' Returns a btree, data.table
#'
#' @param btree A data.table object that is a btree
#' @param nodeId Id of the node which should be the root node in the sub-btree
#' @param depth subset btree by nodes <= this depth
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' mytree <- make_btree(nodeIds=c(1,2,3,4,5,6,7), parentNodeIds=c(NA,1,1,2,2,3,3))
#' sub_btree(mytree, nodeId=3)
#' sub_btree(mytree, depth=1)

sub_btree <- function(btree, nodeId=NULL, depth=NULL){
  # Returns the subtree of btree whose root node is the given nodeId

  #--------------------------------------------------
  # Check input

  if(!inherits(btree, "btree"))
    stop("btree must be a btree object")

  #--------------------------------------------------


  # Subset by depth
  if(is.null(nodeId) & !is.null(depth)){
    subtree <- btree[Depth <= depth]
    return(subtree)
  }

  childNodes <- btree[NodeId == nodeId]
  childNodesList <- list(childNodes)

  while(nrow(childNodes) > 0){
    childNodes <- btree[childNodes[, list(NodeId)], on=c("ParentNodeId" = "NodeId"), nomatch=0]
    childNodesList <- c(childNodesList, list(childNodes))
  }

  subtree <- rbindlist(childNodesList)
  subtree[NodeId == nodeId, ParentNodeId := NA]

  # Prepend class with "btree"
  class(subtree) <- class(btree)

  return(subtree[])
}
