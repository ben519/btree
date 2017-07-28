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
#' library(randomForest)
#' library(ggplot2)
#' rf <- randomForest(mpg ~ cyl + disp + hp, data = mtcars)
#' dtree <- dtree_from_randomForest(rf, 1L)
#' plot_btree(dtree, labelCol = "Split")

dtree_from_randomForest <- function(rf, kthTree, data=NULL){
  # Returns a vector of paths, "L" representing traversal to the left and "R" representing traversal to the right

  #--------------------------------------------------
  # Check input

  if(!inherits(rf, "randomForest"))
    stop("rf must be a randomForest object")

  #--------------------------------------------------

  # Get the kth tree and prep for conversion to dtree
  rftree <- data.table(getTree(rfobj = rf, k = kthTree, labelVar = TRUE))
  rftree[, NodeId := .I]
  rftree[`left daughter` == 0, `:=`(`left daughter` = NA, `right daughter` = NA)]

  # Convert to dtree
  dtree <- make_dtree(
    nodeIds = rftree$NodeId,
    leftChildIds = rftree$`left daughter`,
    rightChildIds = rftree$`right daughter`,
    splitVars = rftree$`split var`,
    splitVals = rftree$`split point`,
    data = data
  )

  # Insert prediction column
  dtree[rftree, Prediction := i.prediction, on=c("NodeId")]

  return(dtree[])
}
