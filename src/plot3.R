
df_plot3 <- df_recent

chain <- df %>%
  select(camis,dba)
chain <- distinct(chain)
chain <- chain %>% 
  group_by(dba) %>% 
  summarise(count = n()) %>% 
  subset(count > 3)

df_plot3$chain <- ifelse(df_plot3$dba %in% chain$dba, 'Chain', 'Non-Chain')

# df_plot3 <- df_plot3 %>% 
#   group_by(boro, chain) %>% 
#   summarise(Mean_Score = mean(score))
df_plot3 <- subset(df_plot3, !(boro %in% c('0','210')))


ggplot(df_plot3, aes(x=chain, y=score, fill = chain)) +
  geom_boxplot() +
  facet_wrap(~boro) +
  scale_y_continuous(limits = quantile(df_plot3$score, c(0.1, 0.82))) + 
  ggtitle("Which one violates more?",
          subtitle = "Chain Restaurant vs Non-chain Restaurant") +
  labs(x = "Type", y = "Violation Score") +
  theme_grey(16) +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(legend.position = c(0.85, 0.28))

