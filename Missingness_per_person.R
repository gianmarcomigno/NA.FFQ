##### Missingness per participant
# plot: donut chart with %NA per participant
df %>% select(na_sum) %>%
  mutate(na_sum_2=case_when(
    na_sum %in% 10:80 ~ "10+",
    TRUE ~ as.character(na_sum))
  ) %>% select(na_sum_2) %>% table %>% prop.table %>% round(3) %>% as.data.frame %>% rename(., missing = .) %>%
  slice(c(1:2,4:11,3)) %>%
  mutate(missing = paste0(factor(missing, levels = as.character(missing)), " NA"),
         Freq = Freq*100) %>%
  plot_ly(labels = ~missing, values = ~Freq, sort =F, textposition = 'inside',textinfo = 'label+percent') %>% 
  add_pie(hole = 0.6) %>% 
  layout(title = "Distribution of NA per participant", showlegend=F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# look at the most common variables missing among those with 1--3 NA
df %>% filter(na_sum==1) %>% select(19:98) %>%
  sapply(., function(x) sum(is.na(x))/length(x)*100)  %>%
  sort(T)
  
# plot: y-log-scaled
ggplot(as.data.frame(table(df$na_sum)/nrow(df)), 
       aes(x=factor(Var1), y = Freq)) + 
  #geom_bar(stat="identity") +
  geom_point(shape=19, size=2) +
  scale_y_continuous(trans = scales::log10_trans(),
                     labels = function(x) format(x, scientific = FALSE,
                                                 drop0trailing = T), # labels=comma
                     limits = c(10^-5,1)) +
  annotation_logticks(sides = 'l') +
  theme(axis.text.x=element_text(hjust=1), panel.background = element_blank()) +
  labs(x="Number of NA per participant", y="Proportion of sample") + 
  geom_vline(xintercept = 4.12) # mean(df$na_sum)=3.12, but shifted for participants w/ 0 NA
