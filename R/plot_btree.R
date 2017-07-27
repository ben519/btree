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
#' mytree <- make_perfect_btree(4)
#' plot_btree(mytree)

plot_btree <- function(btree, labelCol="NodeId"){
  # Plot a btree

  #--------------------------------------------------
  # Check input

  if(!inherits(btree, "btree"))
    stop("btree must be a btree object")

  #--------------------------------------------------

  # Copy the given btree and add columns for Depth, NodePath
  btreeCopy <- btree[, list(NodeId, ParentNodeId, LeftChildNodeId, RightChildNodeId)]
  btreeCopy[, Label := btree[[labelCol]]]
  btreeCopy[, NodePath := btree_paths(btreeCopy)]
  btreeCopy[, Depth := nchar(NodePath)]

  # Order the rows properly
  setorder(btreeCopy, "Depth", "NodePath")

  # Determine the width of each subtree @ each node
  subtree_width <- function(btree, nodeId){
    subtree <- sub_btree(btree, nodeId)
    width <- pmax(1, max(nchar(btree_paths(subtree))))
    return(width)
  }
  btreeCopy[, SubTreeWidth := subtree_width(btreeCopy, NodeId), by=NodeId]

  # Get the max depth
  btree.depth <- max(btreeCopy$Depth)

  # If depth is 0, plot a single node at (.5, .5)
  if(btree.depth == 0) ggplot(btreeCopy, aes(x=.5, y=.5, label=Label))+geom_text()+theme_void()

  # Insert X coordinates
  btreeCopy[Depth == 0, `:=`(Xmin = 0, Xmax = 1, X = 0.5)]
  for(depth in seq(1, btree.depth)){

    # widths: 1, 6  -> 1/7
    # X: 0.5, 1+3

    # Get the nodes at the current depth
    current_level_nodes <- btreeCopy[Depth == depth]

    # For each node, get its parent's Xmin, Xmax
    parent_level_nodes <- btreeCopy[Depth == depth - 1]
    current_level_nodes[parent_level_nodes, `:=`(ParentXmin = i.Xmin, ParentXmax = i.Xmax), on=c("ParentNodeId"="NodeId")]

    # Determine Xmin, X, Xmax
    current_level_nodes[, BlockWidth := (ParentXmax - ParentXmin)/sum(SubTreeWidth), by=ParentNodeId]
    current_level_nodes[, BlocksOver := cumsum(SubTreeWidth) - SubTreeWidth + SubTreeWidth/2, by=ParentNodeId]
    current_level_nodes[, X := ParentXmin + BlockWidth * BlocksOver]
    current_level_nodes[, `:=`(
      Xmin = ParentXmin + (cumsum(SubTreeWidth) - SubTreeWidth) * BlockWidth,
      Xmax = ParentXmin + cumsum(SubTreeWidth) * BlockWidth
    ), by=ParentNodeId]

    # Insert values into btreeCopy
    btreeCopy[current_level_nodes, `:=`(Xmin = i.Xmin, Xmax = i.Xmax, X = i.X), on="NodeId"]
  }

  # Insert Y coordinates
  btreeCopy[, Y := 1 - Depth/btree.depth]
  btreeCopy[Y < 1, Y := Y + (1/btree.depth)* 0.5 * (seq_len(.N) %% 3)/2, by=Depth]

  # Get the edges
  btree.edges <- btreeCopy[btreeCopy, on=c("NodeId"="ParentNodeId"), nomatch=0]
  btree.edges <- btree.edges[, list(NodeId1=NodeId, NodeId2=i.NodeId, X1=X, Y1=Y, X2=i.X, Y2=i.Y)]

  # Get the (x, y) coord of each node in the perfect btree
  ggplot()+
    geom_text(data=btreeCopy, aes(x=X, y=Y, label=Label), size=3)+
    geom_segment(data=btree.edges, aes(x=X1, y=Y1, xend=X2, yend=Y2), linetype="dotted", alpha=.25)+
    xlim(0, 1)+ylim(0, 1)+theme_void()
}
