##### Missingness per food-item 
# Check missingness patterns
sort(sapply(df, function(x) sum(is.na(x))/length(x)*100)) 

# Dataset with %NA per variable for each questions group (variables 19-98 are food-related)
map(df[, 19:98], ~sum(is.na(.))) %>% unlist %>% prop.table(.) %>% round(digits = 6) %>% as.data.frame() %>% tibble::rownames_to_column() %>%
  mutate(group = {c(rep(1,17),rep(2,8),rep(3,8),rep(4,7),rep(5,4),rep(6,11),rep(7,7),rep(8,12),rep(9,6)) %>% 
             as.factor()}) %>%
  rename(., perc = .) -> na_per_var_perc

# info on %NA, min and max number of values per question group
na_per_var_perc %>% 
  group_by(group) %>%
  summarize(mean_size = mean(perc/nrow(df)*100),
            min_val = min(perc),
            max_val = max(perc))

# test equal proportion inside all questions
prop.test(na_per_var_perc$perc, rep(nrow(df),80))

# to fit negative binomial regression (NOT used)
nbGLM_idk <- MASS::glm.nb(perc ~ rowname, na_per_var_perc)
summary(nbGLM_idk)
nbGLM_idk$z <- as.factor(1:80)
nbGLM_idk$model$fitted <- predict(nbGLM_idk, type = "response")/nrow(df)

# Plot 1: %NA per item
na_per_var_perc %>%
  mutate(labelthis = ifelse(perc %in% unlist({na_per_var_perc %>% group_by(group) %>% summarise(maxperc = max(perc), minperc = min(perc)) %>% 
      select(maxperc, minperc)}), rowname, NA)) %>%
  ggplot(., aes(x=factor(1:80), y=perc, color = group)) +
  geom_point(shape=19, size=2, fill = na_per_var_perc$group) +
  labs(x="", y="Proportion of participants with NA", col="Question\ngroup") +
  scale_fill_discrete(breaks = levels(na_per_var_perc$group)) +
  scale_color_manual(labels=c("Dairy/Bread", "Corn", "Meat", "Bird/Fish", "Potato", 
                              "Vegetables", "Fruit", "Snack", "Alcohol"),
                     values=c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", 
                              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_x_discrete(breaks= 1:80, labels = na_per_var_perc$rowname) +
  scale_y_continuous(trans = scales::log10_trans(),
                     labels = function(x) format(x, scientific = FALSE,
                                                 drop0trailing = T), # labels=comma
                     limits = c(10^-3,1)) +
  annotation_logticks(sides = 'l') +
  theme(axis.text.x=element_text(angle=90,hjust=1), panel.background = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA)) +
  geom_text(aes(label=labelthis),hjust=0,vjust=-0.5,size=5, show.legend = F) 
  # fit negative binomial regression
  #geom_point(data = nbGLM_idk$model, aes(nbGLM_idk$z, nbGLM_idk$model$fitted), shape = '-', size = 6)
