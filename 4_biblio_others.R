# How to write a bibliometric paper - other measures
# Tengku Hanis (https://tengkuhanis.netlify.app/)
# Jom Research (https://jomresearch.netlify.app/)
# Nov7, 2023

# Packages ----------------------------------------------------------------

library(bibliometrix)
library(tidyverse)
theme_set(theme_bw())

# Data --------------------------------------------------------------------

dat <- convert2df(file = "wos.bib", dbsource = "wos", format = "bibtex")

dat2 <- 
  dat %>% 
  filter(DT %in% c("ARTICLE", "ARTICLE; PROCEEDINGS PAPER", "PROCEEDINGS PAPER", "REVIEW"))

# Miscellaneous metrics ----------------------------------------------------

## 1) Thematic map ----

Map <- thematicMap(dat2, field = "ID", #"ID","DE", "TI", "AB"
                   minfreq = 3, stemming = FALSE, n.labels = 3, repel = T)
plot(Map$map)

Map$documentToClusters %>% view()
Map$documentToClusters %>% 
  filter(Assigned_cluster == "tamoxifen") %>% 
  select(TI, DI)

## 2) Thematic evolution ----

years <- c(2003, 2013) #cut off points
theme_evo <- thematicEvolution(dat2, field = "ID", years = years, n = 100, minFreq = 2)
plotThematicEvolution(theme_evo$Nodes, theme_evo$Edges)


## 3) Trending keywords ----

trend_kw <- fieldByYear(dat2, field = "ID", timespan = c(2010,2023),
                        min.freq = 1, n.items = 5, graph = TRUE) 

# Another way to plot trending keywords
dat_kw <- trend_kw$df_graph

ggplot(dat_kw, aes(year_med, freq)) + 
  geom_point() +
  ggrepel::geom_text_repel(aes(label = tolower(dat_kw$item)), max.overlaps = 50) +
  scale_x_continuous(breaks = seq(2010, 2023, 1)) +
  xlab("Year") +
  ylab("Frequency")

## 4) Authors' dominance ----

result <- biblioAnalysis(dat2)
dom <- dominance(result, k=10)
dom
?dominance #detail how dominance factor calculated

## 5) Top-author productivity over time ----

topAU <- authorProdOverTime(dat2, k=10)
topAU$graph

head(topAU$dfAU) #author's productivity per year
head(topAU$dfPapersAU) #author's document list

## 6) Three fields plot
threeFieldsPlot(dat2, fields = c("AU", "DE", "SO"), n = c(20, 20, 20))
