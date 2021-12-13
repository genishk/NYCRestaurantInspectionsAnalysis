
df_plot8 <- df_recent %>% drop_na(grade)
df_plot8 <- subset(df_plot8,!(grade %in% c('G')))
df_plot8 <- na.omit(df_plot8)
df_plot8$grade <- factor(df_plot8$grade)
df_plot8$boro <- factor(df_plot8$boro)

df_plot8 <- subset(df_plot8, select=c('boro','cuisine','flag','grade'))

temp <- df_plot8 %>% group_by(cuisine) %>% summarize(count=n())
df_plot8 <- subset(df_plot8, cuisine %in% temp[order(-temp$count),][0:5,]$cuisine)

df_plot8 <- df_plot8 %>% 
  group_by(boro, cuisine, grade) %>% 
  summarize(n = n())

temp2 <- df_plot8 %>% 
  group_by(boro, grade) %>% 
  summarize(total = sum(n))

df_plot8 <- merge(df_plot8, temp2, by = c('boro','grade'))
df_plot8$prop <- df_plot8$n/df_plot8$total

df_plot8 %>% 
  ggplot(aes(x = grade, y = prop, fill = cuisine)) +
  geom_col() +
  facet_wrap(~boro) +
  ggtitle("Cuisine by Grade", sub = "faceted on Borough") +
  theme_classic() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(5, "Oranges")) +
  labs(x = "Grade", y= "Proportion",fill="Cuisine (Top 5)") 

