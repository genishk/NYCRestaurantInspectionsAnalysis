library(reactable)

df_plot5 <- df

df_plot5 <- subset(df_plot5,!(vio.code %in% c('02G','02B','20D','04N','02H','02I','20D','02C','02J')))

df_plot5 <- df_plot5 %>% drop_na(vio.code)

vio_count <- df_plot5 %>% 
  group_by(vio.code) %>% 
  summarize(count = n())
# major_vio <- vio_count[vio_count$count>1500,]
# df_plot5 <- df_plot5[df_plot5$vio.code %in% major_vio$vio.code,]
df_plot5 <- df_plot5 %>% 
  group_by(insp.date,vio.code,vio.desc) %>% 
  summarize(count = n())

temp <- df_plot5 %>% 
  group_by(vio.code) %>% 
  summarize(count = n())
temp <- temp[temp$count>1000,]$vio.code
df_plot5 <- df_plot5[df_plot5$vio.code %in% temp,]
unique(df_plot5$vio.code)

prop <- df_plot5 %>% 
  group_by(insp.date) %>% 
  summarize(countsum = sum(count))
prop <- prop[prop$countsum >10,]

df_plot5 <- merge(df_plot5, prop, by = 'insp.date')
df_plot5$prop <- df_plot5$count/df_plot5$countsum
df_plot5 <- df_plot5[!duplicated(df_plot5[c('insp.date','vio.code')]),]
df_plot5 <- df_plot5[df_plot5$insp.date >= '2018-01-01',]

# Add regression lines
ggplot(df_plot5, aes(x=insp.date, y=prop, color =vio.code)) +
  geom_point() + 
  facet_wrap(~vio.code, scales = 'free') +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = 'black', alpha = 0.5)


ggplot(df_plot5, aes(x=insp.date, y=prop, color =vio.code)) +
  geom_point() + 
  facet_wrap(~vio.code, scales = 'free') +
  geom_line(stat = "smooth", method = lm, color = "red", size = 1, alpha = 0.3) +
  labs(y = "Frequency", x = "Inspection Date", color = "Violation Code") + 
  ggtitle("Which Violation increases after COVID-19?") +
  theme(plot.title = element_text(face = "bold"))


codetable <- unique(df_plot5[c('vio.code','vio.desc')])
colnames(codetable) <- c('Violation_Code', 'Description')
reactable(codetable)

