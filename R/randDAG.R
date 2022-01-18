#' randDAG
#' 
#' Generate a random dag with assigned exposure & outcome and standard naming convention
#' 
#' @param n The number of nodes
#' @param p The degree of connectivity 
#'
#' @return A `tidy_dagitty` object
#'
#' @export
#' 
#' @examples
#' library(dagitty)
#' dag <- randDAG(6, .5)
randDAG <- function(n, p){
  
  rd0 <- dagitty::randomDAG(n, p)
  
  draw <- sample(seq(1,n), 2)
  exposure <- paste0('x', min(draw))
  outcome <- paste0('x', max(draw))
  
  #Assign standard names
  df1 <- data.frame(
    oldName = paste0('x', 1:n)
  ) %>% 
    dplyr::mutate(
      exposure = oldName == exposure,
      outcome = oldName == outcome
    ) %>% 
    dplyr::arrange(outcome, exposure) %>% 
    dplyr::mutate(newName = c(paste0("Z", 1:(n-2)), "X", "Y")) %>% 
    dplyr::select(oldName, newName)
  
  # Rename the nodes in the random dagitty object
  rd1 <- rd0
  for (i in 1:n) {
    rd1 <- gsub(df1[i, "oldName"], df1[i, "newName"], rd1)
  }
  
  rd1 <- sub("X", "X [exposure]", rd1) # Designate X as the exposure
  rd1 <- sub("Y", "Y [outcome]", rd1) # Designate Y as the outcome
  rd1 <- sub("X -> Y\n", "", rd1) # Ensure the DAG includes an arrow pointing from X to Y
  rd1 <- sub("}", "X -> Y\n}", rd1) # Ensure the DAG includes an arrow pointing from X to Y
  
  rd2 <- ggdag::node_status(rd1) # create tidy_dagitty object with status variable
  
  return(rd2)
  
}