has_one_root <- function(btree){
  # Make sure exactly one root node exists

  return(sum(is.na(btree$ParentNodeID)) == 1)
}
