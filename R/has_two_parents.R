has_two_parents <- function(btree){
  # Make sure every node besides the root has exactly two parents

  dt <- btree[btree[!is.na(ParentNodeID)], on=c("NodeID" = "ParentNodeID")][, list(Parents=.N), keyby=NodeID]

  if(nrow(dt) == 0) return(TRUE) else return(all(dt$Parents == 2))
}
