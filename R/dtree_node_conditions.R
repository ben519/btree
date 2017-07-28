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

  conditionsList <- list(data.table(
    NodeId=integer(0), SplitVar=character(0), LB=numeric(0), RB=numeric(0), Last=numeric(0), IsLeftChild=logical(0))
  )

  for(depth in 1:max(dtree$Depth)){
    treelevel <- dtree[Depth == depth]

    # Left-child nodes
    treelevel[dtree, `:=`(
      IsLeftChild = TRUE,
      ParentSplitVar = i.SplitVar,
      ParentSplitVal = i.SplitVal
    ), on=c("NodeId"="LeftChildNodeId")]

    # Right-child nodes
    treelevel[dtree, `:=`(
      IsLeftChild = FALSE,
      ParentSplitVar = i.SplitVar,
      ParentSplitVal = i.SplitVal
    ), on=c("NodeId"="RightChildNodeId")]

    treelevel[IsLeftChild == TRUE, `:=`(LB = -Inf, RB = ParentSplitVal)]
    treelevel[IsLeftChild == FALSE, `:=`(LB = ParentSplitVal, RB = Inf)]

    # Get the conditions for this level
    conditions.level <- treelevel[, list(NodeId, SplitVar = ParentSplitVar, LB, RB, Last = ParentSplitVal, IsLeftChild)]

    # Get the conditions inherited by each node's parent {NodeId, SplitVar, LB, RB}
    conditions.inherited <- conditionsList[[depth]][, list(ParentNodeId=NodeId, SplitVar, LB, RB, Last, IsLeftChild)][
      treelevel[, list(NodeId, ParentNodeId)], on=c("ParentNodeId"), nomatch=0, allow.cartesian = TRUE]
    conditions.inherited[, ParentNodeId := NULL]

    # Combine the current conditions with the inherited ones
    conditions <- rbind(conditions.inherited, conditions.level, use.names=TRUE)

    # Aggregate
    conditions <- conditions[, list(
      LB = max(LB), RB = min(RB), Last = tail(Last, 1), IsLeftChild = tail(IsLeftChild, 1)
    ), keyby=list(NodeId, SplitVar)]

    # Append
    conditionsList <- c(conditionsList, list(conditions))
  }

  # combine list of conditions (each element represents a level of the tree)
  conditions <- rbindlist(conditionsList)

  # Set Conditions, assuming no unordered factors exist
  conditions[LB == -Inf, Condition := paste0(SplitVar, " <= ", RB)]
  conditions[RB == Inf, Condition := paste0(SplitVar, " > ", LB)]
  conditions[is.na(Condition), Condition := paste0("between(", SplitVar, ", ", LB, ", ", RB, ")")]

  result <- conditions[, list(Condition = paste(Condition, collapse = " & ")), keyby=NodeId]

  #--------------------------------------------------
  # Fix everything regarding unordered factors

  unorderedfactors <- as.character(unique(dtree[str_detect(Split, "==")]$SplitVar))
  ufConditions <- conditions[SplitVar %in% unorderedfactors]
  ufConditions[IsLeftChild == TRUE, TrueUFCondition := paste0(SplitVar, " == ", RB)]
  ufConditions[IsLeftChild == FALSE, TrueUFCondition := paste0(SplitVar, " != ", RB)]
  ufConditions[RB == Inf, TrueUFCondition := paste0(SplitVar, " != ", LB)]
  result[ufConditions, `:=`(UFCondition = i.Condition, TrueUFCondition = i.TrueUFCondition), on="NodeId"]
  result[!is.na(UFCondition), Condition := str_replace(Condition, UFCondition, TrueUFCondition)]
  result[, c("UFCondition", "TrueUFCondition") := NULL]

  return(result)
}
