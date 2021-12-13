
df_plot4 <- df_recent
df_plot4 <- df_plot4 %>% 
  group_by(cuisine) %>% 
  summarise(Mean = mean(score), n = n())

df_plot4 <- na.omit(df_plot4)
df_plot4 <- subset(df_plot4, n>100)
#################################################################

# create a theme for dot plots, which can be reused
theme_dotplot <- theme_bw(14) +
  theme(axis.text.y = element_text(size = rel(.75)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.x = element_blank())


# create the plot
ggplot(df_plot4, aes(x = Mean, y = reorder(cuisine, Mean))) +
  geom_point(color = "red") +
  theme_dotplot +
  xlab("Average Violation Score") +
  ylab("Cuisine Category \n(Include only n>100)") +
  ggtitle("Which category of Cuisine \nreceived higher violation score?")
