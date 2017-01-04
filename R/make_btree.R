#' @title
#' Make btree
#'
#' @description
#' Make a btree, data.table object from one of two constructors:
#'
#' \itemize{
#'  \item{A vector of node IDs and parent node IDs} or
#'  \item{A vector of node IDs, left-child IDs, and right-child IDs}
#' }
#'
#' @details
#' Returns a data.table object with fields {NodeID, ParentNodeID, LeftChildNodeID, RightChildNodeID}
#'
#' @param nodeIDs A vector of node IDs
#' @param parentNodeIDs A vector of parent node IDs
#' @param leftChildIDs A vector of left-child node IDs
#' @param rightChildIDs A vector of right-child node IDs
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#'
#' # constructor 1
#' make_btree(nodeIDs=c(1), parentNodeIDs=c(NA_integer_))
#' make_btree(nodeIDs=c(1,2), parentNodeIDs=c(NA,1))
#' make_btree(nodeIDs=c(1,2,3), parentNodeIDs=c(NA,1,1))
#' make_btree(nodeIDs=c(1,2,3,4,5,6,7), parentNodeIDs=c(NA,1,1,2,2,3,3))
#'
#' # constructor 2
#' make_btree(nodeIDs=c(1), leftChildIDs=c(NA_integer_), rightChildIDs=c(NA_integer_))
#' make_btree(nodeIDs=c(1), leftChildIDs=c(2), rightChildIDs=c(3))
#' make_btree(nodeIDs=c(1,2,3), leftChildIDs=c(2,NA,NA), rightChildIDs=c(3,NA,NA))
#' make_btree(nodeIDs=c(1,1), leftChildIDs=c(2,3), rightChildIDs=c(3,2))


make_btree <- function(nodeIDs, parentNodeIDs=NULL, leftChildIDs=NULL, rightChildIDs=NULL){
  # Make a btree
  # Two interfaces are available:
  # {nodeIDs, parentNodeIDs} or {nodeIDs, leftChildIDs, rightChildIDs}

  #--------------------------------------------------
  # Check inputs

  if(!is.null(parentNodeIDs) & (!is.null(leftChildIDs) | !is.null(rightChildIDs)))
    stop("parentNodeIDs is non-null bus so is one of {leftChildIDs, rightChildIDs}")

  if(is.null(leftChildIDs) + is.null(rightChildIDs) == 1)
    stop("One of {leftChildIDs, rightChildIDs} is null and one is not")

  #--------------------------------------------------
  # Constructors

  make_btree1 <- function(nodeIDs, parentNodeIDs){
    # Generate a BTree using nodeIDs and parentIDs

    # Make sure inputs are same length
    if(length(nodeIDs) != length(parentNodeIDs))
      stop("nodeIDs and parentNodeIDs have different lengths")

    # Initialize the btree
    btree <- data.table(NodeID = nodeIDs, ParentNodeID = parentNodeIDs)

    # Check btree properties
    if(!has_one_root(btree)) stop("BTree property violated: exactly one root node")
    if(!has_two_parents(btree)) stop("BTree property violated: exactly two parents per node (excluding root)")

    return(btree)
  }

  make_btree2 <- function(nodeIDs, leftChildIDs, rightChildIDs){
    # Generate a BTree using nodeIDs, leftChildIDs, rightChildIDs
    # Where leftChildIDs is NA and rightChildIDs is NA, node is assumed to be a terminal node

    # Make sure inputs are same length
    if(length(nodeIDs) != length(leftChildIDs) | length(nodeIDs) != length(rightChildIDs))
      stop("nodeIDs, leftChildIDs, and rightChildIDs are not all the same length")

    # Make sure nodes (besides root) are either a left-child or right-child but not both
    if(length(intersect(leftChildIDs[!is.na(leftChildIDs)], rightChildIDs[!is.na(rightChildIDs)])) > 0)
      stop("There exists at least one node that is in leftChildIDs and rightChildIDs")

    # Generate  table of parent nodes
    parentsNodes <- data.table(ParentNodeID=nodeIDs, LeftChildID=leftChildIDs, RightChildID=rightChildIDs)

    # Initialize the tree
    nodeIDs <- unique(c(nodeIDs, leftChildIDs, rightChildIDs))
    nodeIDs <- nodeIDs[!is.na(nodeIDs)]
    btree <- data.table(NodeID=nodeIDs)

    # Insert ParentNodeID
    btree[parentsNodes, ParentNodeID := i.ParentNodeID, on=c("NodeID" = "LeftChildID")]
    btree[parentsNodes, ParentNodeID := i.ParentNodeID, on=c("NodeID" = "RightChildID")]

    # Check btree properties
    if(!has_one_root(btree)) stop("BTree property violated: exactly one root node")
    if(!has_two_parents(btree)) stop("BTree property violated: exactly two parents per node (excluding root)")

    return(btree)
  }

  #--------------------------------------------------
  # Call the appropriate constructor

  if(!is.null(parentNodeIDs)){
    btree <- make_btree1(nodeIDs=nodeIDs, parentNodeIDs=parentNodeIDs)
  } else{
    btree <- make_btree2(nodeIDs=nodeIDs, leftChildIDs=leftChildIDs, rightChildIDs=rightChildIDs)
  }

  #--------------------------------------------------
  # Include columns {LeftChildNodeID, RightChildNodeID}

  btree <- btree[, list(NodeID=ParentNodeID, LeftChildNodeID=NodeID)][btree, on="NodeID", mult="first"]
  btree <- btree[, list(NodeID=ParentNodeID, RightChildNodeID=NodeID)][btree, on="NodeID", mult="last"]
  setcolorder(btree, c("NodeID", "ParentNodeID", "LeftChildNodeID", "RightChildNodeID"))

  return(btree)
}
