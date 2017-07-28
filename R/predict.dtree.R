#' @title
#' Predict dtree
#'
#' @description
#' Make predictions with a dtree object
#'
#' @export
#' @import data.table

predict.dtree = function(dtree, data){
  # Predict which NodeId each row of data will map to

  data <- as.data.table(data)
  leaves <- dtree[is.na(LeftChildNodeId)]
  result <- rep(NA_integer_, nrow(data))
  for(leaf in seq_len(nrow(leaves))){
    temp <- data[, eval(parse(text = paste0("list(InLeaf = ", leaves$NodeCondition[leaf], ")")))]$InLeaf
    result[temp] <- leaves$NodeId[leaf]
  }

  return(result)
}
