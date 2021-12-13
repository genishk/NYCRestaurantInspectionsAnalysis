library(reactable)
library(viridis)

df_plot6 <- df_recent
df_plot6 <- subset(df_plot6,!(vio.code %in% c('02G','02B','20D','04N','02H','02I','20D','02C','02J')))
df_plot6 <- df_plot6 %>% drop_na(vio.code)

recent_vio <- df_plot6 %>% 
  group_by(vio.code,vio.desc) %>% 
  summarize(count = n())
top10vio_recent <- recent_vio[recent_vio$count>1000,]
top10vio_recent <- top10vio_recent[order(-top10vio_recent$count),]
colnames(top10vio_recent) <- c('Violation_Code', 'Description', 'Count')
top10vio_recent$pct <- top10vio_recent$Count / sum(top10vio_recent$Count) * 100
top10vio_recent$pct <- round(top10vio_recent$pct, digits = 0)

ggplot(top10vio_recent, aes(x='', y=pct, fill=reorder(Violation_Code,Count)))+
  geom_bar(stat='identity')+
  theme_void()+
  coord_polar('y', start=0)+
  geom_text(aes(label=paste0(round(pct,1), '%')),
            position=position_stack(vjust=0.5),
            color='white', family='serif', size=7)+
  scale_fill_viridis(option="H",discrete=TRUE) +
  labs(fill="Violation Code") + 
  ggtitle("Most recently, which violations are major?") +
  theme(plot.title = element_text(face = "bold", size = 15))

top10vio_recent <- top10vio_recent[order(top10vio_recent$Count),]
reactable(top10vio_recent[c('Violation_Code','Description')])



