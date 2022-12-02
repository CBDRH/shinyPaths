#' drawPath
#'
#' @description Draw a DAG highlighting a path of your choice
#' 
#' @param dag A dagitty object
#' @param adj Variables to adjust for, a vector
#' @param path The path to highlight, a string
#'
#' @return A ggplot dag
#' @noRd
#' 
#' 
#' @examples 
#' drawPath(dag = ggdag::confounder_triangle(x_y_associated = TRUE), 
#' adj = c('z'), 
#' path = "x <- z -> y")
#' 
drawPath <- function(dag, adj=NULL, path="x -> y", open=TRUE, directed=TRUE) {
  
  stopifnot(dagitty::is.dagitty(dag))
  
  data <- dag %>% 
    adjust_for(adj) %>% 
    node_status() %>% 
    magrittr::extract2(1)

  pathDF <- paste("dag {", path, "}") %>%
    dagitty::as.dagitty() %>%
    ggdag::tidy_dagitty() %>%
    magrittr::extract2(1) %>%
    dplyr::filter(!is.na(direction)) %>%
    dplyr::select(name, to) %>% 
    mutate(edgeID = case_when(open==TRUE & directed==TRUE ~ openCol,
                              open==TRUE & directed==FALSE ~ colorspace::lighten(openCol, 0.4),
                              open==FALSE & directed==TRUE ~ closedCol,
                              open==FALSE & directed==FALSE ~ colorspace::lighten(closedCol, 0.4)
                              )) %>%
    full_join(data, by = c('name','to')) %>%
    mutate(col = factor(case_when(status=='exposure' ~ 1,
                                  status=='outcome' ~ 2,
                                  adjusted=='adjusted' ~ 3,
                                  adjusted=='unadjusted' ~ 4),
                        levels = 1:4,
                        labels = c('exposure', 'outcome', 'adjusted', 'unadjusted')
    ))


  title <- dplyr::case_when(
    open==TRUE & directed==TRUE ~ 'An open causal path',
    open==TRUE & directed==FALSE ~ 'An open non-causal path',
    open==FALSE & directed==TRUE ~ 'A closed causal path',
    open==FALSE & directed==FALSE ~ 'A closed non-causal path',
  )
  

  p <- pathDF %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, fill = col, shape = adjusted)) +
    geom_dag_point(aes(color = col)) +
    geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(10, "pt"), type = "closed"), edge_color='grey') +
    geom_dag_edges(aes(edge_color=edgeID), edge_width = 1.05, arrow_directed = grid::arrow(length = grid::unit(10.5, "pt"), type = "closed")) +
    geom_dag_text() +
    theme_dag(legend.position = 'bottom') +
    theme(plot.subtitle = element_text(colour = 'grey30')) +
    guides(color = guide_legend(override.aes = list(size = 8))) +
    scale_fill_manual(NULL,
                      values = c('exposure' = exposureCol, 'outcome' = outcomeCol, 'adjusted' = adjustedCol, 'unadjusted' = unadjustedCol),
                      labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome', 'adjusted' = "Adjusted", 'unadjusted' = "Unadjusted"),
                      na.value = naCol) +
    scale_color_manual(NULL,
                       values = c('exposure' = exposureCol, 'outcome' = outcomeCol, 'adjusted' = adjustedCol, 'unadjusted' = unadjustedCol),
                       labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome', 'adjusted' = "Adjusted", 'unadjusted' = "Unadjusted"),
                       na.value = naCol) +
    scale_shape_manual(NULL,
                       values = c('exposure' = 21, 'outcome' = 21, 'adjusted' = 22, 'unadjusted' = 21),
                       labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome', 'adjusted' = "Adjusted", 'unadjusted' = "Unadjusted"),
                       na.value = naCol) +
    labs(subtitle = title)

  
return(p)  
  
}

