#' @title
#' Node Depths
#'
#' @description
#' Given a btree, determine the depth of each node
#'
#' @details
#' Returns a vector of node depths, with root node depth = 0
#'
#' @param btree A btree object
#' @param nodeIds Optionally a vector of node Ids corresponding to nodes in btree
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#'
#' btree1 <- make_btree(nodeIds=c(1,2,3,4,5,6,7), parentNodeIds=c(NA,1,1,2,2,3,3))
#' node_depths(btree1)

node_depths <- function(btree, nodeIds=NULL){
  # Return the depth of nodes in btree
  # Piggy-back the btree_paths() function for this

  #--------------------------------------------------
  # Check input

  if(!inherits(btree, "btree"))
    stop("btree must be a btree object")

  #--------------------------------------------------

  paths <- btree::btree_paths(btree)
  depths <- nchar(paths)

  if(!is.null(nodeIds)){
    tmp <- btree[, list(NodeId, Depth=depths)]
    depths <- tmp[J(NodeId=nodeIds), on="NodeId"]$Depth
  }

  return(depths)
}
