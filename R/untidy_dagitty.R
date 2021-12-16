#' untidy_dagitty
#' 
#' Revert a basic `tidy_dagitty` object to a `dagitty` object while preserving node positions
#'
#' @param td A `tidy_dagitty` object
#' @return A named list containing two dagitty objects. The first element preserves the layout for plotting with `plot.dagitty()` or on [dagitty.net](http://www.dagitty.net/dags.html). The second element preserves the layout to plot with `ggdag` functions.  
#' @export
#'
#' @examples
#' library(dagitty)
#' library(ggdag)
#' 
#' dag <- dagitty( "dag {
#'   Y <- X <- Z1 <- V -> Z2 -> Y
#'   Z1 <- W1 <-> W2 -> Z2
#'   X <- W1 -> Y
#'   X <- W2 -> Y
#'   X [exposure]
#'   Y [outcome]
#'   }")
#' tidyDag <- ggdag::tidy_dagitty(dag)
#' ud <- untidy_dagitty(tidyDag)
#' plot(ud[["dagitty"]])
#' ggdag(ud[["ggdag"]])
untidy_dagitty <- function(td){
  
  stopifnot(ggdag::is.tidy_dagitty(td))
  
  ts <- td$dag # The string describing the dagitty object
  
  eVar <- stringi::str_match(ts, "\\n\\s*(.*?)\\s*\\[exposure")[[2]] # The exposure variable
  oVar <- stringi::str_match(ts, "\\n\\s*(.*?)\\s*\\[outcome")[[2]] # THe outcome variable
  
  # Use the tidy_dagitty dataframe to create "find" and "replace" character variables for each node
  df <- td$data %>% 
    distinct(name, .keep_all = TRUE) %>% 
    mutate(
      status = case_when(
        name == eVar ~ "exposure, ",
        name == oVar ~ "outcome, ",
        TRUE ~ ""
      ),
      find = case_when(
        name == eVar ~ paste(name, "\\[exposure\\]"),
        name == oVar ~ paste(name, "\\[outcome\\]"),
        TRUE ~ name
      ),
      replace1 = paste0(name, ' [', status, 'pos="',round(x, 2), ',', -1*round(y, 2), '"]'), # dagitty 
      replace2 = paste0(name, ' [', status, 'pos="',round(x, 2), ',', round(y, 2), '"]') # dagitty R package
    ) %>% 
    select(find, replace1, replace2)
  
  outString1 <- outString2 <- ts
  for (i in 1:nrow(df)) {
    outString1 <- sub(df[i, "find"], df[i, "replace1"], outString1)
    outString2 <- sub(df[i, "find"], df[i, "replace2"], outString2)
  }
  
  return(list(dagitty = outString1, ggdag = outString2))
}