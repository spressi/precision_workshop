library(tidyverse)
library(afex)
library(apa)

# Ich füge hier mal meine libraries der Übersicht halber hinzu - wie erwartet 
# gibt's dann natürlich Konflikte, also dass z.B. cohens_d() aus effectsize 
# dann das aus apa maskiert.
library(effectsize)
library(confintr)
library(rstatix)

se <- function(x, na.rm = TRUE) {
  sd(x, na.rm) / sqrt(if(!na.rm) length(x) else sum(!is.na(x)))
}

# Load Data ---------------------------------------------------------------

data <- read_rds("eye.rds") %>% tibble() %>% 
  filter(phase == "Gen") %>% 
  select(subject, trial, dwell, dwell.non, threat, diagnostic) %>% 
  pivot_longer(cols = contains("dwell"), names_to = "diagnosticity", values_to = "dwell") %>% 
  mutate(
    diagnosticity = 
      if_else(diagnosticity %>% grepl("non", .), "Non-Diagnostic", "Diagnostic") %>% 
      as_factor()
  ) %>% 
  summarize(.by = c(subject, diagnostic, diagnosticity),
            dwell = mean(dwell))


# J: Nur am Rande; sofern man confintr eh geladen hat:
data %>% summarize(se1 = se(dwell),
                   se2 = confintr::se_mean(dwell),
                   check = se1 == se2)
checkNAs <- data; checkNAs[1:50, "dwell"] <- NA
checkNAs %>% summarize(se1 = se(dwell),
                       se2 = confintr::se_mean(dwell),
                       check = se1 == se2)

# ANOVA -------------------------------------------------------------------

anova1 <- 
  afex::aov_ez(data = data,
               dv = "dwell",
               id = "subject", 
               within = c("diagnostic", "diagnosticity"),
               include_aov = TRUE) 

anova1 %>% apa::anova_apa(force_sph_corr = TRUE)

# with CIs
# petasq_ci <- anova1$anova_table %>% lapply(apaTables::get.ci.partial.eta.squared, . %>% pull("F"), . %>% pull("num Df"), . %>% pull("den Df"))
# anova1 %>% apa::anova_apa(force_sph_corr = TRUE, print = FALSE) %>% mutate(text = text %>% lapply(paste, petasq_ci))

peta.ci.vec <- function(F.values, dfs1, dfs2, conf.level = .9) {
  if (length(F.values) != length(dfs1) | length(F.values) != length(dfs2)) stop("Different length of arguments.")
  
  result <- tibble()
  for (i in seq_along(F.values))
    result <- apaTables::get.ci.partial.eta.squared(F.values[i], dfs1[i], dfs2[i], conf.level = conf.level) %>% bind_rows() %>% bind_rows(result, .)
  return(result)
}

peta.ci <- function(anova_table, conf.level = .9, intercept = FALSE) {
  result <- peta.ci.vec(anova_table$`F`, anova_table$`num Df`, anova_table$`den Df`, conf.level = conf.level)
  if (intercept) result <- result %>% bind_rows(tibble(LL = NA, UL = NA), .) #TODO get F of intercept
  
  #result %>% rename(!!paste0("ci", conf.level*100, "_low") = LL, !!paste0("ci", conf.level*100, "_up") = UL)
  result <- result %>% bind_cols(tibble(conf.level = conf.level))
  
  return(result)
}

# J: base R for the win :-)
peta.ci2 <- 
  function(anova_table, conf.level = .9, intercept = FALSE) { #intercept currently unused argument
  
  result <- 
    apply(anova_table, 1, function(x) {
      ci <- 
        apaTables::get.ci.partial.eta.squared(
          F.value = x["F"], df1 = x["num Df"], df2 = x["den Df"], conf.level = conf.level
        )
      
      return(setNames(c(ci$LL, ci$UL), c("LL", "UL")))
    }) %>% 
    t() %>% 
    as.data.frame()
  
  result$conf.level <- conf.level
  
  return(result)
}


# peta.ci(anova1$anova_table)
peta.ci2(anova1$anova_table)

anova1 %>% 
  apa::anova_apa(force_sph_corr = TRUE, print = FALSE) %>% 
  bind_cols(peta.ci(anova1$anova_table, intercept = TRUE)) %>% 
  mutate(LL = LL %>% round(2), UL = UL %>% round(2)) %>% 
  group_by(effect) %>% 
  transmute(text = paste0(text, ", ", conf.level*100, "% CI [", LL, ", ", UL, "]"))

# t-test ------------------------------------------------------------------
#with(data, t.test(dwell ~ diagnosticity, paired = TRUE)) %>% apa::t_apa(es_ci = TRUE)

# J: paired + formula notation kann (seit 4.4.0, glaube ich?) nicht mehr 
# verwendet werden, weil die Reihenfolge der Daten(-Paare) in dem Fall nicht 
# explizit ist. Der Code läuft also für mich nicht, sondern:

# M: Schade. Ein warning hätte es wohl auch getan für backwards compatibility...
# Ich mag nämlich nicht, wenn man redundant info eingeben muss
# z.B. "dwell" doppelt callen oder gar explizit die levels von diagnosticity :/
# aber wenn das jetzt einen error wirft, haben wir ja keine Wahl mehr

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
    effectsize::cohens_d(
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

# M: guter Punkt! Ich wusste nicht, dass man "TRUE" nicht als Variablennamen verwenden kann, aber T schon
# können wir gerne konsistent im Kurs so handhaben, dass wir TRUE nie abkürzen

# Correlation -------------------------------------------------------------
data %>% pivot_wider(names_from = diagnosticity, values_from = dwell) %>% 
  summarize(.by = diagnostic, 
            cortest = cor.test(Diagnostic, `Non-Diagnostic`) %>% 
              apa::cor_apa(r_ci = TRUE, print = FALSE))
#see also: psych::r.con

# J: Für dieses Beispiel finde ich gerade wenn die tidy syntax das Ziel ist, 
# rstatix leichter nachvollziehbar. Werte sind auch hier wieder 
# dieselben.

# M: Syntax gefällt mir besser, output gefällt mir weniger :D
# Wir könnten ja beide Varianten zeigen und dann die Teilnehmenden entscheiden lassen
# zumal rstatix ein richtig gutes allround-Paket zu sein scheint - ähnlich wie apa

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
confintr::ci_cor(
  data$dwell[data$diagnosticity == "Diagnostic" & data$diagnostic == "Mouth/Nose"],
  data$dwell[data$diagnosticity == "Non-Diagnostic" & data$diagnostic == "Mouth/Nose"]
)

# M: best I can do to reduce baseR :P
with(data %>% filter(diagnostic == "Eyes"), #das hier so vorzuziehen muss nicht sein
     confintr::ci_cor(dwell[diagnosticity == "Diagnostic"],
                      dwell[diagnosticity == "Non-Diagnostic"]))
with(data %>% filter(diagnostic == "Mouth/Nose"),
     confintr::ci_cor(dwell[diagnosticity == "Diagnostic"],
                      dwell[diagnosticity == "Non-Diagnostic"]))


# M: Fazit
# apa und rstatix gefallen mir am besten, weil man damit in data frames bleiben kann
# sobald man nämlich was Komplexeres als eine Hand voll Tests vor hat, stößt man schnell ohne data.frames an Grenzen


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
