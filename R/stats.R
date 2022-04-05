# SETUP ---------------------------------------------------------------------------------

# load packages
require(here)
require(tidyverse)
require(rstatix)
require(PMCMRplus)
require(rcompanion)

# load data
source(here::here("R", "getdata_binary.R"))

# recode factor as character, otherwise is interpreted as incomplete block design
df.pairs <- df.pairs %>% 
  mutate(pair_ordered = as.character(pair_ordered))

# GET MAIN EFFECTS ----------------------------------------------------------------------

# Jaccard index: hand-hand
df.pairs %>%
  filter(pc1 == "Hand-Hand") %>%
  rstatix::friedman_test(ji ~ pair_ordered | id)

# Jaccard index: forearm-forearm
df.pairs %>%
  filter(pc1 == "Forearm-Forearm") %>% 
  rstatix::friedman_test(ji ~ pair_ordered | id)

# Pointwise mutual information: hand-hand
df.pairs %>% 
  filter(pc1 == "Hand-Hand") %>% 
  rstatix::friedman_test(pmi ~ pair_ordered | id)

# Pointwise mutual information: forearm-forearm
df.pairs %>%
  filter(pc1 == "Forearm-Forearm") %>% 
  rstatix::friedman_test(pmi ~ pair_ordered | id)

# GET PAIRWISE COMPARISONS --------------------------------------------------------------

# get dataframes
pairwise.ji.hh <- rcompanion::PMCMRTable(
  with(filter(df.pairs, pc1 == "Hand-Hand"),
       PMCMRplus::frdAllPairsNemenyiTest(ji,
                                         groups = pair_ordered,
                                         blocks = id))) %>% 
  mutate(id = "ji_hh")

pairwise.ji.ff <- rcompanion::PMCMRTable(
  with(filter(df.pairs, pc1 == "Forearm-Forearm"),
       PMCMRplus::frdAllPairsNemenyiTest(ji,
                                         groups = pair_ordered,
                                         blocks = id))) %>% 
  mutate(id = "ji_ff")

pairwise.pmi.hh <- rcompanion::PMCMRTable(
  with(filter(df.pairs, pc1 == "Hand-Hand"),
       PMCMRplus::frdAllPairsNemenyiTest(pmi,
                                         groups = pair_ordered,
                                         blocks = id))) %>% 
  mutate(id = "pmi_hh")

pairwise.pmi.ff <- rcompanion::PMCMRTable(
  with(filter(df.pairs, pc1 == "Forearm-Forearm"),
       PMCMRplus::frdAllPairsNemenyiTest(pmi,
                                         groups = pair_ordered,
                                         blocks = id))) %>% 
  mutate(id = "pmi_ff")

# join dataframes
df.pairwise <- bind_rows(
  pairwise.ji.hh,
  pairwise.ji.ff,
  pairwise.pmi.hh,
  pairwise.pmi.ff
) %>% 
  # variables
  transmute(comparison = substr(Comparison, 1, 17),
            p = round(as.numeric(p.value), 3),
            id = id) %>% 
  mutate(pair1 = substr(comparison, 1, 7),
         pair2 = substr(comparison, 11, 17))

# inspect pairwise comparisons where main effects are significant ...
df.pairwise %>% filter(id == "ji_hh")
df.pairwise %>% filter(id == "ji_ff")
df.pairwise %>% filter(id == "pmi_ff")

# ... only comparisons which include a certain muscle pair
df.pairwise %>% filter(id == "ji_hh") %>% 
  filter(grepl("FDI_ADM", comparison, fixed = T)) 
df.pairwise %>% filter(id == "ji_ff") %>% 
  filter(grepl("FCR_ECR", comparison, fixed = T))
df.pairwise %>% filter(id == "pmi_ff") %>% 
  filter(grepl("FCR_EDC", comparison, fixed = T))




