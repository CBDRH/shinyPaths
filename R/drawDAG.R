#' drawDAG
#'
#' @param dag A DAG string
#' @param adj nodes to adjust for
#'
#' @return a ggplot DAG
#' @export
#'
#' @examples
#' g1 <- 'dag {
#' X [exposure, pos="0.000,0.000"]
#' Y [outcome, pos="2.000,0.000"]
#' Z [pos="1.000,1.000"]
#' Z -> X
#' Z -> Y
#' X -> Y
#' }'
#' 
#' drawDAG(g1)
#' drawDAG(g1, 'Z')
drawDAG <- function(dag, adj=NULL){
  dag %>% 
    dagitty() %>% 
    adjust_for(adj) %>% 
    node_status %>% 
    mutate(col = factor(case_when(status=='exposure' ~ 1, 
                                  status=='outcome' ~ 2, 
                                  adjusted=='adjusted' ~ 3, 
                                  adjusted=='unadjusted' ~ 4),
                        levels = 1:4,
                        labels = c('exposure', 'outcome', 'adjusted', 'unadjusted')
    )) %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, fill = col, shape=col)) +
    geom_dag_point(aes(color = col)) +
    geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(10, "pt"), type = "closed")) +
    geom_dag_text() +
    theme_dag(legend.position = 'bottom') +
    #guides(color = guide_legend(override.aes = list(size = 8))) +
    scale_fill_manual(NULL,
                      values = c('exposure' = exposureCol, 'outcome' = outcomeCol, 'adjusted' = adjustedCol, 'unadjusted' = unadjustedCol),
                      labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome', 'adjusted' = "Adjusted", 'unadjusted' = "Unadjusted"),
                      na.value = naCol, drop = FALSE) +
    scale_color_manual(NULL,
                       values = c('exposure' = exposureCol, 'outcome' = outcomeCol, 'adjusted' = adjustedCol, 'unadjusted' = unadjustedCol),
                       labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome', 'adjusted' = "Adjusted", 'unadjusted' = "Unadjusted"),
                       na.value = naCol, drop = FALSE) +
    scale_shape_manual(NULL,
                       values = c('exposure' = 21, 'outcome' = 21, 'adjusted' = 22, 'unadjusted' = 21),
                       labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome', 'adjusted' = "Adjusted", 'unadjusted' = "Unadjusted"),
                       na.value = naCol, drop = FALSE)
}
