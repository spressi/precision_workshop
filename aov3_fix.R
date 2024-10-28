library(tidyverse)

myGgTheme <- # you can save your theme in a local variable instead, add it to every plot, and save your environment across sessions
  theme_bw() + # start with the black-and-white theme
  theme( 
    #aspect.ratio = 1,
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "white"),
    legend.background = element_rect(fill = "white", color = "grey"),
    legend.key = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
    axis.ticks.x = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.title = element_text(size = 16, color = "black"),
    legend.text = element_text(size = 14, color = "black"),
    legend.title = element_text(size = 14, color = "black"),
    strip.text = element_text(size = 12, color = "black"))

fhch2010_summary <- 
  afex::fhch2010 %>% 
  summarise(rt = mean(rt), .by = c(id, task, stimulus))

# old version: error bars suitable for lexdec vs. naming comparison
old_plot <- 
  fhch2010_summary %>% 
  pivot_wider(names_from = task, values_from = rt, id_cols = c(id, stimulus)) %>% 
  summarize(
    .by = stimulus, # task is implicitly kept due to pivot_wider
    ci.length = confintr::ci_mean_diff(naming, lexdec)$interval %>% diff(), # do this first so we can overwrite naming & lexdec
    naming = mean(naming, na.rm = TRUE), 
    lexdec = mean(lexdec, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = c(naming, lexdec), names_to = "task", values_to = "rt.m") %>% 
  
  ggplot(aes(y = rt.m, x = stimulus, color = task)) + # task and stimulus switched compared to aov2
  facet_wrap(vars(task), labeller = label_both) + # task and stimulus switched compared to aov2
  geom_point(position = position_dodge(.9)) + # explicitly specify default width = .9
  geom_errorbar(aes(ymin = rt.m - ci.length/2, ymax = rt.m + ci.length/2), 
                position = position_dodge(.9)) + # explicitly specify default width = .9
  scale_y_continuous(limits = c(.6, 1.4)) + # explicitly set axis to make plots comparable
  myGgTheme +
  theme(legend.position = "top")

# however, we want to compare word vs. nonword, which is a within-subject comparison
# ci_mean_diff does not help here

# one option would be: use sqrt(2)/correlation-adjustment method (Cousineau, 2017)

# here: take CI from paired t-test

# for reference:

ttest_naming <- 
  with(
    fhch2010_summary,
    t.test(
      rt[task == "naming" & stimulus == "word"],
      rt[task == "naming" & stimulus == "nonword"],
      paired = TRUE
    )
  )

ttest_lexdec <- 
  with(
    fhch2010_summary,
    t.test(
      rt[task == "lexdec" & stimulus == "word"],
      rt[task == "lexdec" & stimulus == "nonword"],
      paired = TRUE
    )
  )

# We can get the confidence interval like this:
ttest_lexdec$conf.int[1:2]

# so:
new_plot <- 
  fhch2010_summary %>% 
  pivot_wider(names_from = stimulus, values_from = rt, id_cols = c(id, task)) %>% 
  summarize(
    .by = task, # stimulus is implicitly kept due to pivot_wider
    #ci.length = t.test(word, nonword, paired = TRUE)$conf.int[1:2] %>% diff(), # do this first so we can overwrite word & nonword
    ci.length = confintr::ci_mean(word - nonword)$interval %>% diff(), # do this first so we can overwrite word & nonword
    word = mean(word, na.rm = TRUE), 
    nonword = mean(nonword, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = c(word, nonword), names_to = "stimulus", values_to = "rt.m") %>% 
  # Can be deleted later, but here: convert to factor to keep ordering of the plot 
  # and make it comparable to previous plot
  mutate(
    task = factor(task, levels = c("lexdec", "naming")),
    stimulus = factor(stimulus, levels = c("word", "nonword"))
  ) %>% 
  
  ggplot(aes(y = rt.m, x = stimulus, color = task)) + # task and stimulus switched compared to aov2
  facet_wrap(vars(task), labeller = label_both) + # task and stimulus switched compared to aov2
  geom_point(position = position_dodge(.9)) + # explicitly specify default width = .9
  geom_errorbar(aes(ymin = rt.m - ci.length/2, ymax = rt.m + ci.length/2), 
                position = position_dodge(.9)) + # explicitly specify default width = .9
  scale_y_continuous(limits = c(.6, 1.4)) + # explicitly set axis to make plots comparable
  myGgTheme +
  theme(legend.position = "top")

cowplot::plot_grid(old_plot, new_plot, nrow = 1)
