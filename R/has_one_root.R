#' @title
#' Has One Root
#'
#' @description
#' Helper method for validating a data.table object as a btree by checking for one root.
#'
#' @details
#' Returns \code{TRUE} if field \code{ParentNodeID} has exactly 1 NA value.
#'
#' @param btree A data.table object with field \code{ParentNodeID}
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' mytree <- data.table(NodeID=c(1, 2, 3), ParentNodeID=c(NA, 1, 1))
#' has_one_root(mytree)

has_one_root <- function(btree){
  # Make sure exactly one root node exists

  return(sum(is.na(btree$ParentNodeID)) == 1)
}
