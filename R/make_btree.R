#' @title
#' Make btree
#'
#' @description
#' Make a btree, data.table object from one of two constructors:
#'
#' \itemize{
#'  \item{A vector of node Ids and parent node Ids} or
#'  \item{A vector of node Ids, left-child Ids, and right-child Ids}
#' }
#'
#' @details
#' Returns a data.table object with fields {NodeId, ParentNodeId, LeftChildNodeId, RightChildNodeId}
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
#' # constructor 1
#' make_btree(nodeIds=c(1), parentNodeIds=c(NA_integer_))
#' make_btree(nodeIds=c(1,2,3), parentNodeIds=c(NA,1,1))
#' make_btree(nodeIds=c(1,2,3,4,5,6,7), parentNodeIds=c(NA,1,1,2,2,3,3))
#'
#' # constructor 2
#' make_btree(nodeIds=c(1), leftChildIds=c(NA_integer_), rightChildIds=c(NA_integer_))
#' make_btree(nodeIds=c(1), leftChildIds=c(2), rightChildIds=c(3))
#' make_btree(nodeIds=c(1,2,3), leftChildIds=c(2,NA,NA), rightChildIds=c(3,NA,NA))


make_btree <- function(nodeIds, parentNodeIds=NULL, leftChildIds=NULL, rightChildIds=NULL){
  # Make a btree
  # Two interfaces are available:
  # {nodeIds, parentNodeIds} or {nodeIds, leftChildIds, rightChildIds}

  #--------------------------------------------------
  # Check inputs

  if(!is.null(parentNodeIds) & (!is.null(leftChildIds) | !is.null(rightChildIds)))
    stop("parentNodeIds is non-null bus so is one of {leftChildIds, rightChildIds}")

  if(is.null(leftChildIds) + is.null(rightChildIds) == 1)
    stop("One of {leftChildIds, rightChildIds} is null and one is not")

  #--------------------------------------------------
  # Constructors

  make_btree1 <- function(nodeIds, parentNodeIds){
    # Generate a BTree using nodeIds and parentIds

    # Make sure inputs are same length
    if(length(nodeIds) != length(parentNodeIds))
      stop("nodeIds and parentNodeIds have different lengths")

    # Initialize the btree
    btree <- data.table(NodeId = nodeIds, ParentNodeId = parentNodeIds)

    # Prepend class with "btree"
    class(btree) <- unique(c("btree", class(btree)))

    # Check btree properties
    if(!has_one_root(btree)) stop("btree property violated: exactly one root node")
    if(!has_two_children(btree)) stop("btree property violated: exactly two children per node (excluding leaves)")

    return(btree)
  }

  make_btree2 <- function(nodeIds, leftChildIds, rightChildIds){
    # Generate a BTree using nodeIds, leftChildIds, rightChildIds
    # Where leftChildIds is NA and rightChildIds is NA, node is assumed to be a terminal node

    # Make sure inputs are same length
    if(length(nodeIds) != length(leftChildIds) | length(nodeIds) != length(rightChildIds))
      stop("nodeIds, leftChildIds, and rightChildIds are not all the same length")

    # Make sure nodes (besides root) are either a left-child or right-child but not both
    if(length(intersect(leftChildIds[!is.na(leftChildIds)], rightChildIds[!is.na(rightChildIds)])) > 0)
      stop("There exists at least one node that is in leftChildIds and rightChildIds")

    # Generate  table of parent nodes
    parentsNodes <- data.table(ParentNodeId=nodeIds, LeftChildId=leftChildIds, RightChildId=rightChildIds)

    # Initialize the tree
    nodeIds <- unique(c(nodeIds, leftChildIds, rightChildIds))
    nodeIds <- nodeIds[!is.na(nodeIds)]
    btree <- data.table(NodeId=nodeIds)

    # Prepend class with "btree"
    class(btree) <- unique(c("btree", class(btree)))

    # Insert ParentNodeId
    btree[parentsNodes, ParentNodeId := i.ParentNodeId, on=c("NodeId" = "LeftChildId")]
    btree[parentsNodes, ParentNodeId := i.ParentNodeId, on=c("NodeId" = "RightChildId")]

    # Check btree properties
    if(!has_one_root(btree)) stop("btree property violated: exactly one root node")
    if(!has_two_children(btree)) stop("btree property violated: exactly two children per node (excluding leaves)")

    return(btree)
  }

  #--------------------------------------------------
  # Call the appropriate constructor

  if(!is.null(parentNodeIds)){
    btree <- make_btree1(nodeIds=nodeIds, parentNodeIds=parentNodeIds)
  } else{
    btree <- make_btree2(nodeIds=nodeIds, leftChildIds=leftChildIds, rightChildIds=rightChildIds)
  }

  #--------------------------------------------------
  # Include columns {LeftChildNodeId, RightChildNodeId}

  btree <- btree[, list(NodeId=ParentNodeId, LeftChildNodeId=NodeId)][btree, on="NodeId", mult="first"]
  btree <- btree[, list(NodeId=ParentNodeId, RightChildNodeId=NodeId)][btree, on="NodeId", mult="last"]
  setcolorder(btree, c("NodeId", "ParentNodeId", "LeftChildNodeId", "RightChildNodeId"))

  #--------------------------------------------------
  # Include column NodePath

  btree[, NodePath := btree_paths(btree)]

  #--------------------------------------------------
  # Include column Depth

  btree[, Depth := nchar(NodePath)]

  return(btree[])
}
