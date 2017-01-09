#' @title
#' Has Two Children
#'
#' @description
#' Helper method for validating a data.table object as a btree by checking for two children per node, excluding leaves.
#'
#' @details
#' Returns \code{TRUE} or \code{FALSE}
#' \code{NodeID}
#'
#' @param btree A data.table object with fields \code{NodeID} and \code{ParentNodeID}
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' mytree <- data.table(NodeID=c(1, 2, 3), ParentNodeID=c(NA, 1, 1))
#' has_two_children(mytree)

has_two_children <- function(btree){
  # Make sure every node besides the root has exactly two parents

  dt <- btree[btree[!is.na(ParentNodeID)], on=c("NodeID" = "ParentNodeID")][, list(Children=.N), keyby=NodeID]
  if(nrow(dt) == 0) return(TRUE) else return(all(dt$Children == 2))
}
