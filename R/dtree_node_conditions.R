#' @title
#' dtree Node Conditions
#'
#' @description
#' For each node in a dtree, get the logical conditions that send samples to the node
#'
#' @details
#' Returns a data.table with columns {NodeId, Condition}
#'
#' @param dtree A dtree
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#'
#' dtree1 <- make_dtree(nodeIds=c(1), leftChildIds=c(2), rightChildIds=c(3), splitVars=c("foo"), splitVals=c(3.5))
#' dtree_node_conditions(dtree1)

dtree_node_conditions <- function(dtree){
  # Get the node conditions

  conditionsList <- list(data.table(NodeID=integer(0), SplitVar=character(0), LB=numeric(0), RB=numeric(0)))
  for(depth in 1:max(dtree$Depth)){
    treelevel <- dtree[Depth == depth]

    # Left-child nodes
    treelevel[dtree, `:=`(
      IsLeftChild = TRUE,
      ParentSplitVar = i.SplitVar,
      ParentSplitPoint = i.SplitPoint
    ), on=c("NodeID"="LeftChildNodeID")]

    # Right-child nodes
    treelevel[dtree, `:=`(
      IsLeftChild = FALSE,
      ParentSplitVar = i.SplitVar,
      ParentSplitPoint = i.SplitPoint
    ), on=c("NodeID"="RightChildNodeID")]

    treelevel[IsLeftChild == TRUE, `:=`(LB = -Inf, RB = ParentSplitPoint)]
    treelevel[IsLeftChild == FALSE, `:=`(LB = ParentSplitPoint, RB = Inf)]

    # Get the conditions for this level
    conditions.level <- treelevel[, list(NodeID, SplitVar = ParentSplitVar, LB, RB)]

    # Get the conditions inherited by each node's parent {NodeId, SplitVar, LB, RB}
    conditions.inherited <- conditionsList[[depth]][, list(ParentNodeID=NodeID, SplitVar, LB, RB)][
      treelevel[, list(NodeID, ParentNodeID)], on=c("ParentNodeID"), nomatch=0, allow.cartesian = TRUE]
    conditions.inherited[, ParentNodeID := NULL]

    # Combine the current conditions with the inherited ones
    conditions <- rbind(conditions.level, conditions.inherited, use.names=TRUE)

    # Aggregate
    conditions <- conditions[, list(LB=max(LB), RB=min(RB)), keyby=list(NodeID, SplitVar)]

    # Append
    conditionsList <- c(conditionsList, list(conditions))
  }

  # combine list of conditions (each element represents a level of the tree)
  conditions <- rbindlist(conditionsList)

  conditions[LB == -Inf, Condition := paste0(SplitVar, " <= ", RB)]
  conditions[RB == Inf, Condition := paste0(SplitVar, " > ", LB)]
  conditions[is.na(Condition), Condition := paste0("between(", SplitVar, ", ", LB, ", ", RB, ")")]

  result <- conditions[, list(Condition = paste(Condition, collapse = " & ")), keyby=NodeID]
  return(result)
}
