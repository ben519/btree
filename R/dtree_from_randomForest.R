#' @title
#' Extract a dtree from a randomForest object
#'
#' @description
#' Create a dtree object from a tree from a randomForest object
#'
#' @details
#' Returns a dtree object
#'
#' @param rf A randomForest object
#' @param kthTree Integer specifying which tree to extract from rf
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' mytree <- data.table(NodeId=c(1, 2, 3), ParentNodeId=c(NA, 1, 1))
#' btree_paths(mytree)

dtree_from_randomForest <- function(rf, kthTree){
  # Returns a vector of paths, "L" representing traversal to the left and "R" representing traversal to the right

  #--------------------------------------------------
  # Check input

  if(!inherits(rf, "randomForest"))
    stop("rf must be a randomForest object")

  #--------------------------------------------------

  return(dtree)
}
