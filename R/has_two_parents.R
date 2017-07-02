#' @title
#' Has Two Children
#'
#' @description
#' Helper method for validating a data.table object as a btree by checking for two children per node, excluding leaves.
#'
#' @details
#' Returns \code{TRUE} or \code{FALSE}
#' \code{NodeId}
#'
#' @param btree A data.table object with fields \code{NodeId} and \code{ParentNodeId}
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' mytree <- data.table(NodeId=c(1, 2, 3), ParentNodeId=c(NA, 1, 1))
#' has_two_children(mytree)

has_two_children <- function(btree){
  # Make sure every node besides the root has exactly two parents

  #--------------------------------------------------
  # Check input

  if(!inherits(btree, "btree"))
    stop("btree must be a btree object")

  #--------------------------------------------------

  dt <- btree[btree[!is.na(ParentNodeId)], on=c("NodeId" = "ParentNodeId")][, list(Children=.N), keyby=NodeId]
  if(nrow(dt) == 0) return(TRUE) else return(all(dt$Children == 2))
}
