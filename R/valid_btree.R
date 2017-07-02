#' @title
#' Valid btree
#'
#' @description
#' Check if an object of type btree is valid
#'
#' @details
#' Returns TRUE if
#'
#' @param btree An object of class(btree)
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' mytree <- make_btree(nodeIds=c(1,2,3,4,5,6,7), parentNodeIds=c(NA,1,1,2,2,3,3))
#' sub_btree(mytree, nodeId=3)
#' sub_btree(mytree, depth=1)
#'

valid_btree <- function(btree, verbose=FALSE){
  # Determine whether the given btree is valid

  #--------------------------------------------------
  # Check inherited classes

  # Inherits from btree, data.table, and data.frame
  if(!inherits(btree, "btree")){
    if(verbose)
      message("btree must inherit the class 'btree'")
    return(FALSE)
  }

  if(!inherits(btree, "data.table")){
    if(verbose)
      message("btree must inherit the class 'data.table'")
    return(FALSE)
  }

  if(!inherits(btree, "data.frame")){
    if(verbose)
      message("btree must inherit the class 'data.frame'")
    return(FALSE)
  }

  #--------------------------------------------------
  # Check required columns

  reqcols <- c("NodeId", "ParentNodeId", "LeftChildNodeId", "RightChildNodeId")
  missingcols <- setdiff(reqcols, colnames(btree))
  if(length(missingcols) > 0){
    if(verbose)
      message(paste("Missing columns", paste(reqcols, collapes=", ")))
    return(FALSE)
  }

  #--------------------------------------------------
  # Check special properties

  if(!has_one_root(btree)){
    if(verbose)
      message("btree must have exactly one root, as indicated by ParentNodeId = NA")
    return(FALSE)
  }

  if(!has_two_children(btree)){
    if(verbose)
      message("Every non-leaf node in btree must have exactly two children")
    return(FALSE)
  }

  return(TRUE)
}
