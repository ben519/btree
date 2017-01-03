#' @title
#' Make Perfect btree
#'
#' @description
#' Generate a perfect btree
#'
#' @details
#' Returns a btree, data.table object
#'
#' @param height Height of the btree
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

make_perfect_btree <- function(height){
  # Make a perfect btree with the specified height

  perfectbtree.Nnodes <- sum(2 ^ seq(0, height))
  perfectbtree.nodeIDs <- seq_len(perfectbtree.Nnodes)
  perfectbtree.parentNodeIDs <- c(NA_integer_, head(rep(seq_len(perfectbtree.Nnodes), each=2), length(perfectbtree.nodeIDs) -1))
  btree <- make_btree(nodeIDs=perfectbtree.nodeIDs, parentNodeIDs=perfectbtree.parentNodeIDs)

  return(btree)
}

# make_perfect_btree(0)
# make_perfect_btree(1)
# make_perfect_btree(2)
