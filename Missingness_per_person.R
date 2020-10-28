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
df %>% pull(na_sum) %>% table %>% `/`(nrow(df)) %>% as.data.frame %>%
  select(2:1) %>% rename(., x = Freq, y=.) %>%
  ggplot(., aes(rev(x),y)) +
  geom_point(shape=19, size=2) +
  labs(y="Number of NA per participant", x="Proportion of sample", title = "Distribution of missing data across all the SNM cohort") +
  scale_x_continuous(trans = scales::log10_trans(),
                     labels = function(x) format(x, scientific = FALSE,
                                                 drop0trailing = T), # labels=comma
                     limits = c(10^-5,1)) +
  scale_y_discrete(labels = c(80:77, 74:0)) +
  theme(axis.text.y=element_text(angle=0,hjust=1), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=14),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.major.x = element_line(color = "grey80")) +
  geom_hline(yintercept = 79-3.12, linetype='dotted', col = 'black') # mean(df$na_sum)=3.12, but shifted for participants w/ 0 NA
