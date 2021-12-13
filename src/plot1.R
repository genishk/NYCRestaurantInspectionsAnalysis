
# ver1
# df_plot1 <- df[df$dba %in% c('ZZ CLAM BAR','ZUM STAMMTISCH','ZURUTTO RAMEN & GYOZA BAR','ZOO BREWS-BRONX ZOO','ZONI CANTINA AND COCINA'),]
df_plot1 <- df[df$camis %in% c(40710411,50075252,40391498,50036714,50042132,50051826,41485818,41303034),]

# ver2
# df_plot1 <- df[df$camis %in% names(which(table(df$camis) > 3)),]
# df_plot1 <- df_plot1[df_plot1$camis %in% sample(unique(df_plot1$camis),10),]

ggplot(df_plot1, aes(x=insp.date, y=score, group = camis, color = dba, shape = boro)) +
  geom_line() + 
  geom_point(size = 2.5) +
  xlab("Inspection Date") +
  ylab("Violation Score") +
  labs(color = "Restaurant", shape = "Borough") +
  ggtitle("Inspection Score changes over Date") +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = c(0.8, 0.65), legend.key.size = unit(0.3, 'cm'))

ggplot(df_plot1, aes(x=insp.date, y=score, group = camis, color = boro)) +
  geom_line() + 
  geom_point(size = 2.5) +
  xlab("Inspection Date") +
  ylab("Violation Score") +
  labs(color = "Borough") +
  ggtitle("Inspection Score changes over Date") +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = c(0.8, 0.65), legend.key.size = unit(0.5, 'cm'))

              