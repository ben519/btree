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

make_dtree <- function(nodeIds, leftChildIds, rightChildIds, splitVars=NULL, splitVals=NULL, data=NULL){
  # Make a dtree
  # Result has columns {NodeId, ParentNodeId, LeftChildNodeId, RightChildNodeId, SplitVar, SplitPoint, Contains}
  # data should be a data.table with factor columns

  #--------------------------------------------------
  # Implementation

  # Start by making a btree from the given node structure
  dtree <- make_btree(nodeIds = nodeIds, leftChildIds = leftChildIds, rightChildIds = rightChildIds)

  # Prepend class with "dtree"
  setattr(dtree, "class", unique(c("dtree", class(dtree))))

  # Map the split information
  if(all(is.na(leftChildIds))){
    splitVars <- NA_character_
    splitVals <- NA_real_
  }
  tmp <- data.table(NodeId = nodeIds, SplitVar = as.character(splitVars), SplitVal = splitVals)
  dtree[tmp, `:=`(SplitVar = i.SplitVar, SplitVal = i.SplitVal), on=c("NodeId")]

  # Insert Split
  dtree[!is.na(SplitVar), Split := paste0(SplitVar, " <= ", SplitVal)]

  # Deal with categorical columns
  cols.ordered <- character(0)
  cols.unordered <- character(0)
  cols.logical <- character(0)
  if(!is.null(data)){
    cols.ordered <- colnames(data)[sapply(data, function(x) is(x, "factor") & is(x, "ordered"))]
    cols.unordered <- colnames(data)[sapply(data, function(x) is(x, "factor") & !is(x, "ordered"))]
    cols.logical <- colnames(data)[sapply(data, function(x) is(x, "logical"))]
  }

  # Adjust the Split for unordered categorical variables
  dtree[SplitVar %in% cols.unordered, Split := paste0(SplitVar, " == ", SplitVal)]

  #--------------------------------------------------
  # Insert Node Conditions

  # print("deleteme")
  # bg <<- copy(dtree)

  conditionsList <- list(data.table(
    NodeId=integer(0), SplitVar=character(0), LB=numeric(0), RB=numeric(0), UFVal=numeric(0), IsLeftChild=logical(0))
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
    conditions.level <- treelevel[, list(NodeId, SplitVar = ParentSplitVar, LB, RB, UFVal = ParentSplitVal, IsLeftChild)]
    conditions.level[SplitVar %in% cols.unordered & IsLeftChild == F, UFVal := {
      x <- as.integer(intToBits(UFVal))
      x[1:length(levels(data[[SplitVar]]))] <- 1L - x[1:length(levels(data[[SplitVar]]))]
      as.double(packBits(x, type="integer"))
    }, by=NodeId]
    conditions.level[!SplitVar %in% cols.unordered, UFVal := NA]

    # Get the conditions inherited by each node's parent {NodeId, SplitVar, LB, RB}
    conditions.inherited <- conditionsList[[depth]][, list(ParentNodeId=NodeId, SplitVar, LB, RB, UFVal, IsLeftChild)][
      treelevel[, list(NodeId, ParentNodeId)], on=c("ParentNodeId"), nomatch=0, allow.cartesian = TRUE]
    conditions.inherited[, ParentNodeId := NULL]

    # Combine the current conditions with the inherited ones
    conditions <- rbind(conditions.inherited, conditions.level, use.names=TRUE)

    # Aggregate
    packUFs <- function(vals){
      if(any(is.na(vals))) return(NA_real_)
      as.double(packBits(as.integer(apply(sapply(vals, function(k) as.integer(intToBits(k))), 1, prod)), type="integer"))
    }
    conditions <- conditions[, list(
      LB = max(LB), RB = min(RB), UFVal = packUFs(UFVal), IsLeftChild = tail(IsLeftChild, 1)
    ), keyby=list(NodeId, SplitVar)]

    # Append
    conditionsList <- c(conditionsList, list(conditions))
  }

  # combine list of conditions (each element represents a level of the tree)
  conditions <- rbindlist(conditionsList)

  # Determing vartypes
  conditions[SplitVar %in% cols.ordered, VarType := "OF"]
  conditions[SplitVar %in% cols.unordered, VarType := "UF"]
  conditions[SplitVar %in% cols.logical, VarType := "logical"]
  conditions[is.na(VarType), VarType := "numeric"]

  # Set Conditions for numeric vars
  conditions[VarType == "numeric" & LB == -Inf, Condition := paste0(SplitVar, " <= ", RB)]
  conditions[VarType == "numeric" & RB == Inf, Condition := paste0(SplitVar, " > ", LB)]
  conditions[VarType == "numeric" & is.na(Condition), Condition := paste0("between(", SplitVar, ", ", LB, ", ", RB, ")")]

  # Fix conditions for ordered factors
  conditions[VarType == "OF" & LB == -Inf, Condition := paste0(SplitVar, ' <= "', levels(data[[SplitVar]])[floor(RB)], '"'), by=list(NodeId, SplitVar)]
  conditions[VarType == "OF" & RB == Inf, Condition := paste0(SplitVar, ' > "', levels(data[[SplitVar]])[ceiling(LB)], '"'), by=list(NodeId, SplitVar)]
  conditions[VarType == "OF" & is.na(Condition) & LB != RB, Condition := paste0(
    'between(', SplitVar, ', "', levels(data[[SplitVar]])[floor(RB)], '", "', levels(data[[SplitVar]])[ceiling(LB)], '")'
  ), by=list(NodeId, SplitVar)]
  conditions[VarType == "OF" & is.na(Condition) & LB == RB, Condition := paste0(
    SplitVar, ' == "', levels(data[[SplitVar]])[ceiling(LB)], '"'
  ), by=list(NodeId, SplitVar)]

  # Fix conditions for unordered factors
  conditions[VarType == "UF", Condition := paste0(
    SplitVar, " %in% c(",
    paste0('"', levels(data[[SplitVar]])[head(as.integer(intToBits(UFVal)), length(levels(data[[SplitVar]]))) == 1], '"', collapse = ', '),
    ")"
  ), by=NodeId]

  # Fix conditions for logical vars
  conditions[VarType == "logical", Condition := paste0(SplitVar, " == ", LB == -Inf)]

  # Insert node conditions
  nodeconditions <- conditions[, list(Condition = paste(Condition, collapse = " & ")), keyby=NodeId]
  dtree[nodeconditions, NodeCondition := i.Condition, on="NodeId"]

  #--------------------------------------------------
  # Fix Split labels for factors

  dtree[SplitVar %in% cols.ordered, Split := paste0(SplitVar, ' <= "', levels(data[[SplitVar]])[floor(SplitVal)], '"'), by=NodeId]
  dtree[SplitVar %in% cols.unordered, Split := paste0(
    SplitVar, ' %in% c(', paste0(
      '"', levels(data[[SplitVar]])[head(as.integer(intToBits(SplitVal)), length(levels(data[[SplitVar]]))) == 1], '"', collapse = ', '
    ), ')'), by=NodeId]

  #--------------------------------------------------
  # Fix Split labels for logicals

  dtree[SplitVar %in% cols.logical, Split := paste0(SplitVar, " == TRUE")]

  return(dtree[])
}
