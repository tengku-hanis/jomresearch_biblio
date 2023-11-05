# Bibliometric analysis - theory/law-related metrics
# Tengku Hanis (https://tengkuhanis.netlify.app/)
# Oct31, 2023

# Packages ----------------------------------------------------------------

library(bibliometrix)
library(tidyverse)
theme_set(theme_bw())

# Data --------------------------------------------------------------------

dat <- convert2df(file = "wos.bib", dbsource = "wos", format = "bibtex")


# Theory related metrics --------------------------------------------------

## 1) Lotka's law ----

result <- biblioAnalysis(dat)
L <- lotka(result)

L$AuthorProd #observed distribution of author productivity
L$Beta #beta coeeficient of Lotka's law
L$R2 #GOF of Lotka's law (r^2)

# P value of K-S two sample test
L$p.value #there is no sig diff btwn observed and theoretical distribut.

# Theoretical distribution with Beta = 2
Theoretical <- 10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

# Using ggplot
ldata <- 
  L$AuthorProd %>% 
  bind_cols(theory = Theoretical) %>% 
  pivot_longer(cols = c(Freq, theory), names_to = "distribution", values_to = "val_distr") %>% 
  mutate(distribution = as.factor(distribution), 
         distribution = fct_recode(distribution, Observed = "Freq", Theoretical = "theory"))

ldata %>% 
  ggplot(aes(N.Articles, val_distr, color = distribution)) +
  geom_line() +
  labs(color = "Distribution:") +
  ylab("Frequency of authors") +
  xlab("Number of articles") +
  theme(legend.position = "top") +
  annotate(geom = "text", x = 5.5, y = 0.2, label = paste("P-value = ", round(L$p.value, digits = 3)))

## 2) Bradford's law ----

bl <- bradford(dat)
bl

# Summary for each zone
bl$table %>% 
  group_by(Zone) %>% 
  summarise(n = n())

# Core journals
bl$table %>% 
  filter(Zone == "Zone 1") %>% 
  select(-SO)

