getGroupsAndWeightList <- function() {
  groups <- list(
    list("Group 1" = c("Metric 1", "Metric 2"), "Weight" = NULL),
    list("Group 2" = c("Metric 3", "Metric 4", "Metric 5"), "Weight" = NULL),
    list("Group 3" = c("Metric 6"), "Weight" = NULL)
  )
  
  #PONER EL PESO DEFAULT PARA MANDARLO
  
  weights <- vector("list", length(groups))
  names(weights) <- names(groups)
  
  return(list(groups = groups, weights = weights))
}


