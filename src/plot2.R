
df_plot2 <- df_recent
df_plot2$violation <- ifelse(df_plot2$action == 'No violations were recorded at the time of this inspection.', 'No Violation', 'Violation')

df_plot2 <- df_plot2 %>% 
  group_by(boro,flag) %>% 
  summarise(count = n()) %>% 
  na.omit %>% 
  mutate(freq = count / sum(count))

ggplot(df_plot2, aes(x = flag, y = freq, fill = flag)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~boro) + 
  scale_y_continuous(limits=c(0,0.8)) +
  ggtitle("Which Area has more # of Critical Violations?") +
  scale_fill_manual(values = c("#b2df8a", "#a6cee3")) +
  labs(y = "Frequency", fill = "") + 
  theme(plot.title = element_text(face = "bold"), axis.title.x=element_blank(), legend.position = c(0.85, 0.28))

##############################################3
# 
# df_plot8 <- subset(df_plot2, flag == 'Critical')
# 
# 
# ggplot(df_plot8, aes(x = boro, y = freq, fill = boro), width = 1) +
#   geom_bar(stat = "identity") +
#   theme(aspect.ratio = 1) +
#   coord_flip() + 
#   coord_polar()
# 
# ggplot(data = df_plot8) +
#   geom_bar(
#     mapping = aes(x = boro, y = freq, fill = boro),
#     show.legend = FALSE,
#     width = 1
#   ) +
#   theme(aspect.ratio = 1) +
#   coord_flip()
# 
# 
# bar <- ggplot(data = diamonds) + 
#   geom_bar(
#     mapping = aes(x = cut, fill = cut), 
#     show.legend = FALSE,
#     width = 1
#   ) + 
#   theme(aspect.ratio = 1) +
#   labs(x = NULL, y = NULL)
# 
# bar + coord_flip()
# bar + coord_polar()