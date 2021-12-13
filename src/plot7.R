df_plot7 <- df %>% drop_na(grade)
df_plot7 <- subset(df_plot7,!(grade %in% c('G')))

df_plot7 <- df_plot7 %>% 
  group_by(boro,grade) %>% 
  summarise(count = n()) %>% 
  na.omit
temp <- df_plot7 %>% 
  group_by(grade) %>% 
  summarise(total = sum(count))
df_plot7 <- merge(df_plot7, temp, by = "grade")
df_plot7$freq <- round(df_plot7$count/df_plot7$total, digits = 2)


ggplot(df_plot7, aes(x = grade, y = freq, fill = boro)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x="Grade",y="Frequency",fill="Borough") + 
  ggtitle("Which Borough gets Better Grade?") +
  theme(plot.title = element_text(face = "bold", size = 15)) +
  scale_fill_viridis(option="G",discrete=TRUE) +
  geom_text(aes(label = ifelse(freq == 0, "", scales::percent(freq))),
            position = position_fill(vjust = 0.5),col = 'white')
