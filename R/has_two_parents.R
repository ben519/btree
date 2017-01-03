#' @title
#' Has Two Parents
#'
#' @description
#' Helper method for validating a data.table object as a btree by checking for two parents per node, excluding the root node.
#'
#' @details
#' Returns \code{TRUE} if every unique \code{ParentNodeID} besides NA (the root) has exactly 2 matching values in field
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
#' has_two_parents(mytree)

has_two_parents <- function(btree){
  # Make sure every node besides the root has exactly two parents

  dt <- btree[btree[!is.na(ParentNodeID)], on=c("NodeID" = "ParentNodeID")][, list(Parents=.N), keyby=NodeID]
  if(nrow(dt) == 0) return(TRUE) else return(all(dt$Parents == 2))
}
