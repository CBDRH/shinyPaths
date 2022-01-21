commas <- function(solution){
  
    l <- length(solution)
    
    if(l==1){
      return(solution)
    }
    
    else if(l==2){
      return(paste(solution[1], "and", solution[2]))
    }
    
    else (
      return(paste(paste(solution[1:l-1], collapse = ", "), "and", solution[l]))
    )
    
  }


#' grader
#'
#' @param td 
#'
#' @return
#'
#' @examples
#' sol <- list(c('Z1', 'Z2'), c('Z1', 'Z3'))
#' guess1 <- c('Z1')
#' guess2 <- c('Z1', 'Z2')
#' grader(guess1, sol)
#' grader(guess2, sol)
grader <- function(submission, solution){
  
  mark <- FALSE
  message <- "Try again!"
  nSol <- length(solution)
  introText <- ifelse(nSol==1, "There was one solution", paste("There were", english::as.english(nSol), "solutions. Adust for:"))
  
  if (length(solution[[1]]) == 0 & all.equal(0, length(submission))==TRUE) {
    mark <- TRUE
    message <- "No adjustment required!"
  }
  
  else if (length(solution[[1]]) > 0) {
    
    for (i in 1:nSol) {
      
      if (setequal(submission, solution[[i]])) {
        mark <- TRUE
        message <- solution[[i]]
      }
      
    }
  }
  
  return(c(mark, message))
}
