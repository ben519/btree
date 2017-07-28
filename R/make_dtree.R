#' @title
#' Make dtree
#'
#' @description
#' Make a dtree from one of two constructors:
#'
#' \itemize{
#'  \item{A vector of node Ids and parent node Ids} or
#'  \item{A vector of node Ids, left-child Ids, and right-child Ids}
#' }
#'
#' @details
#' Returns a dtree object
#'
#' @param nodeIds A vector of node Ids
#' @param parentNodeIds A vector of parent node Ids
#' @param leftChildIds A vector of left-child node Ids
#' @param rightChildIds A vector of right-child node Ids
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#'
#' make_dtree(nodeIds=c(1), leftChildIds=c(NA_integer_), rightChildIds=c(NA_integer_))
#' make_dtree(nodeIds=c(1), leftChildIds=c(2), rightChildIds=c(3), splitVars=c("foo"), splitVals=c(3.5))
#' make_dtree(nodeIds=c(1,2,3), leftChildIds=c(2,NA,NA), rightChildIds=c(3,NA,NA), splitVars=c("foo",NA,NA), splitVals=c(3.5,NA,NA))

make_dtree <- function(nodeIds, leftChildIds, rightChildIds, splitVars=NULL, splitVals=NULL, catVars=NULL){
  # Make a dtree
  # Result has columns {NodeId, ParentNodeId, LeftChildNodeId, RightChildNodeId, SplitVar, SplitPoint, Contains}
  # catVars should be a data.table with factor columns

  #--------------------------------------------------
  # Implementation

  # Start by making a btree from the given node structure
  dtree <- make_btree(nodeIds = nodeIds, leftChildIds = leftChildIds, rightChildIds = rightChildIds)

  # Map the split information
  if(all(is.na(leftChildIds))){
    splitVars <- NA_character_
    splitVals <- NA_real_
  }
  tmp <- data.table(NodeId = nodeIds, SplitVar = splitVars, SplitVal = splitVals)
  dtree[tmp, `:=`(SplitVar = i.SplitVar, SplitVal = i.SplitVal), on=c("NodeId")]

  # Insert Split
  dtree[!is.na(SplitVar), Split := paste0(SplitVar, " <= ", SplitVal)]

  # Deal with categorical columns
  if(!is.null(catVars)){
    catVars.ordered <- catVars[, sapply(catVars, function(x) is(x, "factor") & is(x, "ordered")), with=F]
    catVars.unordered <- catVars[, sapply(catVars, function(x) is(x, "factor") & !is(x, "ordered")), with=F]

    # Adjust the Split for unordered categorical variables
    dtree[SplitVar %in% colnames(catVars.unordered), Split := paste0(SplitVar, " == ", SplitVal)]
  }

  # Insert Node Conditions
  dtree[dtree_node_conditions(dtree), NodeCondition := i.Condition, on="NodeId"]

  # Adjust descriptions for factor vars
  if(!is.null(catVars)){
    dtree[, Split := describe_categorical_splits(Split, catVars)]
    dtree[, NodeCondition := describe_categorical_splits(NodeCondition, catVars)]
  }

  return(dtree[])
}
