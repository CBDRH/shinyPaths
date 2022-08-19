# 
# df <- data.frame(
#   x = seq(1:6),
#   y = c(0.9776265, 0.9361624, 1.0098005, 1.0084399, 0.9352483, 0.9657409),
#   c = c('e', 'u','a', 'u', 'a', 'o'),
#   l = c('d', 'a', 'g', 'g', 'l', 'e')
# )
# 
# ggplot(df, aes(x,y, fill = c, color = c, label = l, shape = c)) + 
#   geom_point(size = 8) + 
#   geom_text(color = 'white', size = 3) +
#   theme_dag() +
#   theme(legend.position = 'none') + 
#   scale_x_continuous(limits = c(-1,7)) +
#   scale_y_continuous(limits = c(0,2)) +
#   scale_color_manual(NULL, 
#                     values = c('e' = exposureCol, 'o' = outcomeCol, 'a' = adjustedCol, 'u' = unadjustedCol),
#                     labels = c('e' = 'e', 'o' = 'o', 'a' = 'a', 'u' = 'u')) +
#   scale_fill_manual(NULL, 
#                      values = c('e' = exposureCol, 'o' = outcomeCol, 'a' = adjustedCol, 'u' = unadjustedCol),
#                      labels = c('e' = 'e', 'o' = 'o', 'a' = 'a', 'u' = 'u')) +
#   scale_shape_manual(NULL, 
#                      values = c('e' = 21, 'o' = 21, 'a'=22, 'u'=21), 
#                      labels = c('e' = 'e', 'o' = 'o', 'a' = 'a', 'u' = 'u'))
# 
# ggsave(here::here("inst/app/www/daggle-logo.png"), height = 150, width = 150*1.618, unit = 'px')
