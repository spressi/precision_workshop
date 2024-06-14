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


# t-test ------------------------------------------------------------------
with(data, t.test(dwell ~ diagnosticity, paired=T)) %>% apa::t_apa(es_ci=T)

# Correlation -------------------------------------------------------------
data %>% pivot_wider(names_from=diagnosticity, values_from=dwell) %>% 
  summarize(.by=diagnostic, cortest = cor.test(Diagnostic, `Non-Diagnostic`) %>% apa::cor_apa(r_ci=T, print=F))
#see also: psych::r.con

# ANOVA -------------------------------------------------------------------
?apaTables::get.ci.partial.eta.squared
