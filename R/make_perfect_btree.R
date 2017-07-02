#' @title
#' Make Perfect btree
#'
#' @description
#' Generate a perfect btree
#'
#' @details
#' Returns a btree object that is a perfect binary tree
#'
#' @param depth Depth of the btree
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Binary_tree#Types_of_binary_trees}
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' make_perfect_btree(2)

make_perfect_btree <- function(depth){
  # Make a perfect btree with the specified depth

  perfectbtree.Nnodes <- sum(2 ^ seq(0, height))
  perfectbtree.nodeIds <- seq_len(perfectbtree.Nnodes)
  perfectbtree.parentNodeIds <- c(NA_integer_, head(rep(seq_len(perfectbtree.Nnodes), each=2), length(perfectbtree.nodeIds) -1))
  btree <- make_btree(nodeIds=perfectbtree.nodeIds, parentNodeIds=perfectbtree.parentNodeIds)

  return(btree)
}
