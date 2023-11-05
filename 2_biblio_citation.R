# Bibliometric analysis - performance analysis-related metrics
# Tengku Hanis (https://tengkuhanis.netlify.app/)
# Oct31, 2023

# Packages ----------------------------------------------------------------

library(bibliometrix)
library(tidyverse)
theme_set(theme_bw())

# Tags --------------------------------------------------------------------

data("bibtag"); View(bibtag)


# Data --------------------------------------------------------------------

dat <- convert2df(file = "wos.bib", dbsource = "wos", format = "bibtex")
names(dat)
dim(dat)


# NAs ---------------------------------------------------------------------

missingData(dat)


# Duplicate ---------------------------------------------------------------

dat %>% 
  select(DI) %>%
  na.omit(DI) %>% 
  summarise(DI = sum(duplicated(DI)))


# Descriptive -------------------------------------------------------------

result <- biblioAnalysis(dat)
summary(result, k=10)

P <- plot(result, k = 10)

P$MostProdAuthors
P$MostProdCountries
P$AnnualScientProd
P$AverArtCitperYear
P$AverTotCitperYear


# Funded research ---------------------------------------------------------

table(is.na(dat$FU)) %>% 
  prop.table()*100 #18% funded


# Citation related metrics  -----------------------------------------------

## References for first paper
dat$CR[1] #separator is ;
result$MostCitedPapers %>% 
  head()

## 1) Frequently cited manuscripts ----
fc <- citations(dat, field = "article", sep = ";")
cbind("Freq" = fc$Cited[1:5])

## 2) Frequently cited first authors ----
fcfa <- citations(dat, field = "author", sep = ";")
cbind("Freq" = fcfa$Cited[1:10])






