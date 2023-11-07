# How to write a bibliometric paper - performance analysis-related metrics
# Tengku Hanis (https://tengkuhanis.netlify.app/)
# Jom Research (https://jomresearch.netlify.app/)
# Nov7, 2023

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


# Filter data -------------------------------------------------------------

dat %>% 
  group_by(DT) %>% 
  count()

dat2 <- 
  dat %>% 
  filter(DT %in% c("ARTICLE", "ARTICLE; PROCEEDINGS PAPER", "PROCEEDINGS PAPER", "REVIEW"))

# Descriptive -------------------------------------------------------------

result <- biblioAnalysis(dat2)
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


# Manual plot -------------------------------------------------------------

## Number of authors per paper ----

## Data for author per paper frequency
no_author <- stringi::stri_count_regex(dat2$AU, c(";"))
auth_data <- 
  data.frame(paper = dat2$TI,
             author = dat2$AU, 
             no_auth = no_author + 1, 
             type = dat2$DT) %>% 
  group_by(no_auth, type) %>% 
  summarise(freq = n(), .groups = "drop")
auth_data

## Percentage of papers with 10 author or less
sum(auth_data$freq[1:14])/sum(auth_data$freq) * 100

## Plot
auth_data %>% 
  mutate(type = as.factor(type), 
         type = fct_collapse(type, "Proceeding paper" = c("PROCEEDINGS PAPER", "ARTICLE; PROCEEDINGS PAPER")),
         type = fct_recode(type, 
                           Article = "ARTICLE", 
                           Review = "REVIEW")) %>% 
  filter(no_auth < 50) %>% #try removing this line
  ggplot(aes(no_auth, freq, fill = type)) + 
  geom_bar(stat = "identity") + 
  xlab("Number of authors") +
  ylab("Frequency") + 
  labs(title = "Number of authors per paper for publications related to male breast cancer", fill = "Type:")


# Citation related metrics  -----------------------------------------------

## References for first paper
dat2$CR[1] #separator is ;
result$MostCitedPapers %>% 
  head()

## 1) Frequently cited manuscripts ----
fc <- citations(dat2, field = "article", sep = ";")
cbind("Freq" = fc$Cited[1:5])

## 2) Frequently cited first authors ----
fcfa <- citations(dat2, field = "author", sep = ";")
cbind("Freq" = fcfa$Cited[1:10])






