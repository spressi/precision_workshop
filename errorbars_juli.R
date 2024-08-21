library(tidyverse)
library(afex)
library(apa)

# J: Rmisc kann man leider nicht unfallfrei zusammen laden, weil Rmisc aus 
# gründen plyr lädt, was dann mit dplyr clasht. Das kann man ansonsten auch 
# umgehen, wenn man Rmisc vor dplyr bzw. dem tidyverse lädt, aber da ich eh 
# nur die eine isolierte Funktion brauche, rufe ich Rmisc da lieber explizit 
# auf.

se = function(x, na.rm = TRUE) {
  sd(x, na.rm) / sqrt(if(!na.rm) length(x) else sum(!is.na(x)))
}

CI <- .95
z.CI <- qnorm(1-(1-CI)/2) # two-sided CI

# Load Data ---------------------------------------------------------------

data <-  
  read_rds("eye.rds") %>% 
  filter(phase == "Gen") %>% 
  select(subject, trial, dwell, dwell.non, threat, diagnostic) %>% 
  pivot_longer(
    cols = contains("dwell"), 
    names_to = "diagnosticity", 
    values_to = "dwell"
  ) %>% 
  mutate(
    diagnosticity = 
      as.factor(
        ifelse(grepl("non", diagnosticity), "Non-Diagnostic", "Diagnostic")
      )
  ) %>% 
  summarize(
    .by = c(subject, diagnosticity, diagnostic),
    dwell = mean(dwell)
  )

data_aggr <- 
  data %>% 
  summarize(.by = c(subject, diagnosticity), dwell = mean(dwell))

# Compute Standard Error --------------------------------------------------

# wrong
se.wrong <-  
  data %>% 
  summarize(.by = diagnosticity, dwell.m = mean(dwell), dwell.se = se(dwell)) %>% 
  rename(normal_se = dwell.se)

# J: also wrong?
data.se <- 
  data_aggr %>% 
  summarize(.by = diagnosticity, dwell.m = mean(dwell), dwell.se = se(dwell)) %>% 
  rename(aggr_se = dwell.se)

# J: Replikation von data.se mit den aggregierten Werten
Rmisc::summarySE(
  data = data_aggr,
  measurevar = "dwell",
  groupvars = "diagnosticity"
)

# J: Das confidence interval ist per default .95
# Es gäbe auch betweenvars als Argument
se.rmisc <- 
  Rmisc::summarySEwithin(
    data = data_aggr,
    measurevar = "dwell",
    withinvars = "diagnosticity",
    idvar = "subject",
    conf.interval = 
  ) %>% 
  select(diagnosticity, dwell, se) %>% 
  rename(dwell.m = dwell, rmisc_se = se)

# Vergleich aller SE-Varianten
Reduce(merge.data.frame, list(se.wrong, data.se, se.rmisc))

# aggregated plot
data.se %>% 
  ggplot(aes(y = dwell.m, x = diagnosticity, fill = diagnosticity)) + 
  geom_col(color = "black") + 
  geom_errorbar(
    aes(ymin = dwell.m - aggr_se*z.CI, 
        ymax = dwell.m + aggr_se*z.CI), 
    width = .5
  ) +
  scale_fill_viridis_d() + 
  theme_bw() +
  theme(legend.position = "top")

# Rmisc plot
se.rmisc %>% 
  ggplot(aes(y = dwell.m, x = diagnosticity, fill = diagnosticity)) + 
  geom_col(color = "black") + 
  geom_errorbar(
    aes(ymin = dwell.m - rmisc_se, ymax = dwell.m + rmisc_se), 
    width = .5
  ) +
  scale_fill_viridis_d() + 
  theme_bw() +
  theme(legend.position = "top")
