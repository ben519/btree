#' @title
#' Describe Categorical Splits
#'
#' @description
#' Helper function for make_dtree()
#'
#' @details
#' fill me in
#'
#' @param rules fill me in
#'
#' @export
#' @import data.table

describe_categorical_splits <- function(rules, catVars){
  # convert categorical splits and conditions, for example
  # "size <= 1.5" -> "size <= "medium"
  # "color = 5" -> "color %in% c('blue', 'green')"
  # catVars should be a data.frame containing the categorical data

  print(rules)

  rulesDT <- data.table(Rule = rules)

  catVars <- catVars[1,]
  catVars.ordered <- catVars[, sapply(catVars, function(x) is(x, "factor") & is(x, "ordered")), with=F]
  catVars.unordered <- catVars[, sapply(catVars, function(x) is(x, "factor") & !is(x, "ordered")), with=F]

  # Fix rules for ordered factors
  for(col in colnames(catVars.ordered)){

    # Algorithm:
    # Find all rules that contain category name
    # replace the first number after category name with the corresponding factor label
    # do this again, but only where patter between(catname, ...)

    catlvls <- levels(catVars[[col]])

    matches <- str_match(rulesDT$Rule, paste0(col, ".*?([0-9.]+)"))
    rulesDT[, `:=`(FullMatch = matches[, 1], ValMatch = matches[, 2])]
    rulesDT[!is.na(FullMatch), Label := catlvls[floor(as.numeric(ValMatch))]]
    rulesDT[!is.na(FullMatch), FullMatchNew := str_replace(FullMatch, ValMatch, paste0('"', Label, '"'))]
    rulesDT[!is.na(FullMatch), Rule := str_replace(Rule, FullMatch, FullMatchNew)]
    rulesDT[, c("FullMatch", "ValMatch", "Label", "FullMatchNew") := NULL]

    matches <- str_match(rulesDT$Rule, paste0(col, ",.*?([0-9.]+)"))
    rulesDT[, `:=`(FullMatch = matches[, 1], ValMatch = matches[, 2])]
    rulesDT[!is.na(FullMatch), Label := catlvls[floor(as.numeric(ValMatch))]]
    rulesDT[!is.na(FullMatch), FullMatchNew := str_replace(FullMatch, ValMatch, paste0('"', Label, '"'))]
    rulesDT[!is.na(FullMatch), Rule := str_replace(Rule, FullMatch, FullMatchNew)]
    rulesDT[, c("FullMatch", "ValMatch", "Label", "FullMatchNew") := NULL]
  }

  # Fix rules for unordered factors
  for(col in colnames(catVars.unordered)){

    # Algorithm:
    # Find all rules that contain category name
    # Assume the SplitVal is an integer whose binary expansion represents the factor levels
    # Adjust rules accordingly

    catlvls <- levels(catVars[[col]])

    matches <- str_match(rulesDT$Rule, paste0(col, ".*?([0-9]+)"))
    rulesDT[, `:=`(FullMatch = matches[, 1], ValMatch = as.integer(matches[, 2]))]
    rulesDT[
      !is.na(FullMatch),
      Label := paste0('"', catlvls[head(as.integer(intToBits(ValMatch)), length(catlvls)) == 1], '"', collapse=", ")
      ]
    rulesDT[!is.na(FullMatch) & str_detect(FullMatch, "=="), FullMatchNew := paste0(col, " %in% c(", Label, ")")]
    rulesDT[!is.na(FullMatch) & str_detect(FullMatch, "!="), FullMatchNew := paste0("!", col, " %in% c(", Label, ")")]
    rulesDT[!is.na(FullMatch), Rule := str_replace(Rule, FullMatch, FullMatchNew)]
    rulesDT[, c("FullMatch", "ValMatch", "Label", "FullMatchNew") := NULL]
  }

  return(rulesDT$Rule)
}
