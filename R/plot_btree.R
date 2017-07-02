#' @title
#' Plot btree
#'
#' @description
#' Plot a btree object using ggplot2
#'
#' @details
#' Returns a ggplot plot of a btree
#'
#' @param btree btree to plot
#' @param labelCol name of the column whose values should be plotted as node labels/info
#'
#' @export
#' @import data.table
#' @import ggplot2
#'
#' @examples
#' library(data.table)
#' library(ggplot2)
#' mytree <- make_perfect_btree(2)
#' plot_btree(mytree)

plot_btree <- function(btree, labelCol="NodeId"){
  # Plot a btree

  #--------------------------------------------------
  # Check input

  if(!inherits(btree, "btree"))
    stop("btree must be a btree object")

  #--------------------------------------------------

  # Algorithm:
  # Determine height of btree
  # Build a perfect btree with the same height
  # Determine (x, y) coords of each node in perfect btree, fit inside a 1x1 bounding box
  # Map (x, y) coords to corresponding nodes of btree

  # Copy the given btree and add a Path column
  btree.nodes <- btree[, list(NodeId, ParentNodeId, LeftChildNodeId, RightChildNodeId)]
  btree.nodes[, Label := btree[[labelCol]]]

  # Calculate btree height
  btree.nodes[, Path := btree_paths(btree.nodes)]
  btree.nodes[, Depth := nchar(Path)]
  btree.height <- max(btree.nodes$Depth)

  # If height is 0, plot a single node at (.5, .5)
  if(btree.height == 0){
    ggplot(btree.nodes, aes(x=.5, y=.5, label=Label))+geom_text()+theme_void()
  }

  # Make a perfect btree with the same height
  btree.perfect <- make_perfect_btree(btree.height)
  btree.perfect[, Path := btree_paths(btree.perfect)]
  btree.perfect[, Depth := nchar(Path)]
  setkey(btree.perfect, "Path")

  # Insert X coordinates
  btree.perfect[Depth==btree.height, X := seq(0, 1, length.out=.N)]  # bottom level
  for(depth in rev(seq(0, btree.height - 1))){
    children <- btree.perfect[Depth == depth + 1]
    children.middles <- children[, list(X.middle = mean(X)), keyby=ParentNodeId]
    btree.perfect[children.middles, X := X.middle, on=c("NodeId"="ParentNodeId")]
  }

  # Insert Y coordinates
  btree.perfect[, Y := seq(1, 0, length.out=btree.height + 1)[Depth + 1]]

  # Join btree.perfect to btree.nodes to get the (x, y) coords
  btree.nodes[btree.perfect, `:=`(X=X, Y=Y), on="Path"]

  # Get the edges
  btree.edges <- btree.nodes[btree.nodes, on=c("NodeId"="ParentNodeId"), nomatch=0]
  btree.edges <- btree.edges[, list(NodeId1=NodeId, NodeId2=i.NodeId, X1=X, Y1=Y, X2=i.X, Y2=i.Y)]

  # Get the (x, y) coord of each node in the perfect btree
  ggplot()+
    geom_text(data=btree.nodes, aes(x=X, y=Y, label=Label), size=3)+
    geom_segment(data=btree.edges, aes(x=X1, y=Y1, xend=X2, yend=Y2), linetype="dotted", alpha=.25)+
    theme_void()
}
