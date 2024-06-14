library(tidyverse)
library(afex)
library(apa)

se = function(x, na.rm = TRUE) {
  sd(x, na.rm) / sqrt(if(!na.rm) length(x) else sum(!is.na(x)))
}


# Load Data ---------------------------------------------------------------
data = read_rds("eye.rds") %>% tibble() %>% 
  filter(phase=="Gen") %>% 
  select(subject, trial, dwell, dwell.non, threat, diagnostic) %>% 
  pivot_longer(cols=contains("dwell"), names_to="diagnosticity", values_to="dwell") %>% 
  mutate(diagnosticity = if_else(diagnosticity %>% grepl("non", .), "Non-Diagnostic", "Diagnostic") %>% as_factor()) %>% 
  summarize(.by = c(subject, diagnostic, diagnosticity),
            dwell = mean(dwell))


# ANOVA -------------------------------------------------------------------
afex::aov_ez(data=data,
             dv = "dwell",
             id = "subject", 
             within = c("diagnostic", "diagnosticity"),
             include_aov = T) %>% apa::anova_apa(force_sph_corr = T)


# Compute Standard Error --------------------------------------------------
#wrong
se.wrong = data %>% summarize(.by="diagnosticity", dwell.m = mean(dwell), dwell.se = se(dwell))

data %>% summarize(.by="diagnosticity", 
                   subjects = subject %>% unique %>% length(),
                   rows = n(),
                   ratio = paste((rows / subjects), "1", sep=":"))

#correct
data.se = data %>% summarize(.by=c("subject", "diagnosticity"), 
                   dwell = mean(dwell)) %>% 
  summarize(.by="diagnosticity", dwell.m = mean(dwell), dwell.se = se(dwell))
full_join(data.se, se.wrong %>% rename(dwell.se.wrong = dwell.se))


#plot
plot.between = data.se %>% ggplot(aes(y = dwell.m, x = diagnosticity, fill = diagnosticity)) + 
  geom_col(color="black") + 
  geom_errorbar(aes(ymin = dwell.m - dwell.se*CI95, ymax = dwell.m + dwell.se*CI95), width=.5) +
  scale_fill_viridis_d() + theme_bw()
plot.between


# Compute Within Standard Error -------------------------------------------
data.se.within = data %>% summarize(.by=c("subject", "diagnosticity"), 
                                    dwell = mean(dwell)) %>% 
  pivot_wider(names_from = diagnosticity, values_from = dwell) %>% 
  mutate(diff = Diagnostic - `Non-Diagnostic`) %>% 
  summarize(dwell.m = mean(diff), dwell.se = se(diff)) %>% mutate(diagnosticity = "Difference")

#plot
# plot.within = data.se.within %>% ggplot(aes(y = dwell.m, x = diagnosticity)) + 
#   geom_col(color="black") + 
#   geom_errorbar(aes(ymin = dwell.m - dwell.se*CI95, ymax = dwell.m + dwell.se*CI95), width=.5) +
#   theme_bw()
# plot.within  
# 
# library(patchwork)
# plot.between + plot.within + plot_annotation(tag_levels = 'a') + plot_layout(axes = "collect")

plot.within = data.se %>% bind_rows(data.se.within) %>% mutate(diagnosticity = diagnosticity %>% as_factor()) %>% 
  ggplot(aes(y = dwell.m, x = diagnosticity, fill = diagnosticity)) + 
  geom_col(color="black") + 
  geom_errorbar(aes(ymin = dwell.m - dwell.se*CI95, ymax = dwell.m + dwell.se*CI95), width=.5) +
  scale_fill_viridis_d() + theme_bw()
plot.within
