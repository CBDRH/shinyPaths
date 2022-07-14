# # Creates the little tutorial image included in the help popup
# 
# df <- tibble(
#   x = rep(c(1:3, 5:7),3),
#   y = c(rep(1,6), rep(4,6), rep(7,6)),
#   adj = rep(c('unadjusted', 'unadjusted', 'unadjusted', 'unadjusted', 'adjusted', 'unadjusted'), 3),
#   lab = rep(c('Z1', 'Z2', 'Z3'), 6),
#   direction = c(rep(c('last', 'first', NA), 2), rep(c('first', 'last', NA), 2), rep(c('last', 'last', NA), 2)),
#   text = c(
#     NA, 'This path is closed', NA,
#     NA, 'Adjusting for Z<sub>2</sub> opens the path', NA,
#     NA, 'This path is open', NA,
#     NA, 'Adjusting for Z<sub>2</sub> closes the path', NA,
#     NA, 'This path is open', NA,
#     NA, 'Adjusting for Z<sub>2</sub> closes the path', NA
#   )
# )
# 
# 
# ggplot(data = df,
#        aes(x=x, y=y, yend=y)) +
#   geom_segment(data = df[df$direction=='last', ], aes(x = x, xend = x + .65), arrow = arrow(length = unit(6, "pt"), ends = 'last', type = 'closed')) +
#   geom_segment(data = df[df$direction=='first', ], aes(x = x + 0.35, xend = x + 1), arrow = arrow(length = unit(6, "pt"), ends = 'first', type = 'closed')) +
#   geom_point(aes(color=adj, shape=adj, fill=adj), size=10)  +
#   geom_text(aes(label=rep(c("Z[1]", "Z[2]", "Z[3]"), 6)), color='white', parse=TRUE) +
#   scale_x_continuous(limits = c(0.8,7.9)) +
#   scale_y_continuous(limits = c(0,8)) +
#   scale_fill_manual(values = c('adjusted' = adjustedCol, 'unadjusted' = naCol),
#                     labels = c('adjusted' = 'Adjusted', 'unadjusted' = 'Unadjusted')) +
#   scale_shape_manual(values = c('adjusted' = 22, 'unadjusted' = 21),
#                      labels = c('adjusted' = 'Adjusted', 'unadjusted' = 'Unadjusted')) +
#   scale_color_manual(values = c('adjusted' = adjustedCol, 'unadjusted' = naCol),
#                      labels = c('adjusted' = 'Adjusted', 'unadjusted' = 'Unadjusted')) +
#   theme_dag(legend.position = 'none') +
#   geom_richtext(aes(x = x -1.2, y = y - 1, label=text), color = naCol, hjust = 0L, size = 3,
#                 fill = NA, label.color = NA, # remove background and outline
#                 label.padding = grid::unit(rep(0, 4), "pt"))
# 
# ggsave(here::here("inst/app/www/dag-examples.png"), height = 3, width = 4.5, unit = 'in')