sub_btree <- function(btree, nodeID){
  # Returns the subtree of btree whose root node is the given nodeID

  childNodes <- btree[NodeID == nodeID]
  childNodesList <- list(childNodes)

  while(nrow(childNodes) > 0){
    childNodes <- btree[childNodes[, list(NodeID)], on=c("ParentNodeID" = "NodeID"), nomatch=0]
    childNodesList <- c(childNodesList, list(childNodes))
  }

  subtree <- rbindlist(childNodesList)
  subtree[NodeID == nodeID, ParentNodeID := NA]

  return(subtree[])
}

# btree <- make_btree(nodeIDs=c(1,2,3,4,5,6,7), parentNodeIDs=c(NA,1,1,2,2,3,3))
# sub_btree(btree, 1)
# sub_btree(btree, 2)
# sub_btree(btree, 5)
