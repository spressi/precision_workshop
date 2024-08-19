library(tidyverse)
library(afex)
library(apa)

# Ich füge hier mal meine libraries der Übersicht halber hinzu - wie erwartet 
# gibt's dann natürlich Konflikte, also dass z.B. cohens_d() aus effectsize 
# dann das aus apa maskiert.
library(effectsize)
library(confintr)
library(rstatix)

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


# J: Nur am Rande; sofern man confintr eh geladen hat:
data %>% summarize(se1 = se(dwell),
                   se2 = confintr::se_mean(dwell),
                   check = se1 == se2)

# ANOVA -------------------------------------------------------------------

anova1 <- 
  afex::aov_ez(data=data,
               dv = "dwell",
               id = "subject", 
               within = c("diagnostic", "diagnosticity"),
               include_aov = T) 

anova1 %>% apa::anova_apa(force_sph_corr = T)

# t-test ------------------------------------------------------------------
with(data, t.test(dwell ~ diagnosticity, paired=T)) %>% apa::t_apa(es_ci=T)

# J: paired + formula notation kann (seit 4.4.0, glaube ich?) nicht mehr 
# verwendet werden, weil die Reihenfolge der Daten(-Paare) in dem Fall nicht 
# explizit ist. Der Code läuft also für mich nicht, sondern:

with(
  data, 
  t.test(
    dwell[diagnosticity == "Diagnostic"], 
    dwell[diagnosticity == "Non-Diagnostic"], 
    paired = TRUE
  )
) %>% 
  apa::t_apa(es_ci = TRUE)

# J: Ich mache das CI über effectsize, sieht dann so aus:
# (wenn ich eh schon die Formel nicht nehmen kann, kann ich mir auch das data 
# masking sparen :-D )
(
  ttest1 <- 
    t.test(
      data$dwell[data$diagnosticity == "Diagnostic"], 
      data$dwell[data$diagnosticity == "Non-Diagnostic"], 
      paired = TRUE
    )
)

(
  cohen1 <- 
    cohens_d(
      data$dwell[data$diagnosticity == "Diagnostic"], 
      data$dwell[data$diagnosticity == "Non-Diagnostic"], 
      paired = TRUE
    )
)

# J: Die Werte sind bei mir zumindest schon mal deckungsgleich, das ist doch 
# was. Ich nutze fürs Formatieren im Manuskript immer ein "privates" Paket, 
# hier nur der Vollständigkeit halber aufgeführt. Ich denke, dem gegenüber ist 
# dein Weg zu bevorzugen.

# devtools::install_github("m-Py/prmisc")
# prmisc::print_ttest(ttest1, cohen1, confidence = TRUE)

# J: Für tidy syntax mag ich immer gerne rstatix, das gibt auch direkt ein 
# confidence interval ums estimate mit aus.
# Es akzeptiert die paired + formula notation scheinbar noch, bzw. geht sogar 
# nicht anders.
data %>%
  rstatix::t_test(
    dwell ~ diagnosticity, 
    paired = TRUE,
    detailed = TRUE # bisschen dumm; braucht man, um das conf int zu sehen ...
  )

# J: Kann auch cohens d, hier weicht das upper CI allerdings ab!
# Muss man mal schauen, woran das liegt, aber vorher haben wir die Werte ja nun 
# häufig genug reproduziert ...
data %>%
  rstatix::cohens_d(
    dwell ~ diagnosticity, 
    paired = TRUE,
    ci = TRUE
  )

# J: PS: Ich verwende niemals T für TRUE, weil (mal for safety reason 
# auskommentiert :-D):

# T == TRUE
# T <- FALSE
# T == TRUE

# Correlation -------------------------------------------------------------
data %>% pivot_wider(names_from=diagnosticity, values_from=dwell) %>% 
  summarize(.by=diagnostic, cortest = cor.test(Diagnostic, `Non-Diagnostic`) %>% apa::cor_apa(r_ci=T, print=F))
#see also: psych::r.con

# J: Für dieses Beispiel finde ich gerade wenn die tidy syntax das Ziel ist, 
# rstatix leichter nachvollziehbar. Werte sind auch hier wieder 
# dieselben.

data %>% 
  pivot_wider(names_from = diagnosticity, values_from = dwell) %>% 
  group_by(diagnostic) %>% 
  rstatix::cor_test(Diagnostic, `Non-Diagnostic`)

# J: confintr ist als package vielleicht auch noch grundsätzlich interessant, 
# aber das kann ich nur in base R, sorry :-D
# Ebenfalls dieselben Werte.

confintr::ci_cor(
  data$dwell[data$diagnosticity == "Diagnostic" & data$diagnostic == "Eyes"],
  data$dwell[data$diagnosticity == "Non-Diagnostic" & data$diagnostic == "Eyes"]
)


# ANOVA -------------------------------------------------------------------
?apaTables::get.ci.partial.eta.squared

# J: Also für die ANOVA von oben, für diagnostic:
apaTables::get.ci.partial.eta.squared(
  F.value = 5.55, df1 = 1, df2 = 42, conf.level = .95
)

# J: Bzw., flexibler und genauer, da exakte Werte:
anova1_table <- anova1$anova_table
effect <- "diagnostic"

apaTables::get.ci.partial.eta.squared(
  F.value = anova1_table[effect, "F"], 
  df1 = anova1_table[effect, "num Df"], 
  df2 = anova1_table[effect, "den Df"], 
  conf.level = .95
)

# J: Eventuell ist auch dieser Blogpost von Daniel Lakens interessant (war 
# allerdings schon 2014 ...):
# https://daniellakens.blogspot.com/2014/06/calculating-confidence-intervals-for.html

# Mal der Lakens-Code 1:1 übernommen, außer, dass ich das confidence Level auf 
# 95 for comparability gesetzt habe. Lakens argumentiert im Blogpost, dass man 
# für eta squared immer .90 nehmen sollte.
Lims <- MBESS::conf.limits.ncf(
  F.value = anova1_table[effect, "F"], 
  conf.level = 0.95, 
  df.1 <- anova1_table[effect, "num Df"], 
  df.2 <- anova1_table[effect, "den Df"]
)

Lower.lim <- Lims$Lower.Limit/(Lims$Lower.Limit + df.1 + df.2 + 1)
Upper.lim <- Lims$Upper.Limit/(Lims$Upper.Limit + df.1 + df.2 + 1)

Lower.lim
Upper.lim

# J: Die Werte sind tatsächlich exakt identisch mit dem, was apaTable 
# produziert!
