#' Title
#'
#' @param td 
#'
#' @return
#'
#' @examples
#' sol <- list(c('Z1', 'Z2'), c('Z1', 'Z3'))
#' guess1 <- c('Z1')
#' guess2 <- c('Z1', 'Z2')
#' grader(guess1, sol, length(sol))
#' grader(guess2, sol, length(sol))
grader <- function(submission, solution, nSol=1){
  
  mark <- FALSE
  
  if (length(solution[[1]]) == 0 & all.equal(0, length(submission))==TRUE) {
    mark <- TRUE
  }
  
  else if (length(solution[[1]]) > 0) {
    
    for (i in 1:nSol) {
      
      if (setequal(submission, solution[[i]])) {
        mark <- TRUE
      }
      
    }
    
  }
  
  return(mark)
  
}