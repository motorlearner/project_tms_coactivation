# README ................................................................................
#
# This script takes the binary data (csv files extracted via MATLAB) and returns the
# following objects: 
#   
#   df.binary....dataframe containing muscle binary responses for each stimulation
#   df.muscles...dataframe containing muscle response frequencies and more
#   df.pairs.....dataframe containing muscle pair response frequencies and more
#   plotcolors...vector of colors for plotting
#   
# .......................................................................................


# SETUP ---------------------------------------------------------------------------------

# load packages
require(here)
require(tidyverse)
require(colortools)
require(gdata)

# IMPORT BINARY DATA --------------------------------------------------------------------

# initialize list
datlist <- list()

# fill list
for (i in 1:20){
  # store dataframe for subject i in list position i
  datlist[[i]] = read.csv(here(paste("data/binary", toString(i), ".csv", sep="")))
}

# glue together data for all subjects
df.binary <- bind_rows(datlist, .id = "id")


# GET VECTOR OF MUSCLES, VECTOR OF MUSCLE PAIRS -----------------------------------------

# vector of muscle names
muscles <- names(df.binary)[!(names(df.binary) %in% c("id", "protocol"))]

# vector of pair names
combs <- combn(muscles, 2, simplify = FALSE) # get all combinations of two muscles
pairs <- vector(length = length(combs))      # initialize vector of pairs

for (i in seq_along(combs)){                 # fill vector of pairs 
  # a muscle pair is formatted NAME1_NAME2
  pairs[i] = paste(combs[[i]][1], combs[[i]][2], sep="_")
}

# GET FREQUENCIES OF RESPONSES FOR MUSCLES ----------------------------------------------

# initialize dataframe
df.muscles <- data.frame(
  id     = rep(1:20, each = length(muscles)), # subject id
  m      = rep(muscles, times = 20),          # muscle name
  n      = NA,                                # number of responses of that muscle
  n_stim = NA,                                # number of stimulations (subject-specific)
  n_mep  = NA                                 # number of stimulation with any response (...)
)

# fill dataframe
for (i in 1:20){
  
  # select binary data for current subject
  bindat <- df.binary %>% filter(id == i)
  # row-indices for current subject in dataframe
  is_id <- (df.muscles$id == i)
  # column-indices for all muscles in binary data
  col_all <- which(names(bindat) %in% muscles)
  # get total number of stimulations
  n_stim <- bindat %>% 
    nrow()
  # get total number of stimulations with any response
  n_mep <- bindat[names(bindat) %in% muscles] %>% 
    mutate(n = rowSums(across(everything()))) %>% 
    filter(n > 0) %>% 
    nrow()
  # fill dataframe
  df.muscles$n_stim[is_id] <- n_stim
  df.muscles$n_mep[is_id]  <- n_mep
  
  for(j in seq_along(muscles)){
    
    # column-index of current muscle in binary data
    col <- which(names(bindat) == muscles[j])
    # number of responses of current muscle
    n <- sum(bindat[ ,col] == 1)
    # row-indices for current muscle in dataframe
    is_muscle <- (df.muscles$m == muscles[j])
    # fill dataframe
    df.muscles$n[is_id & is_muscle] <- n
    
  }
}

# GET FREQUENCIES OF RESPONSES FOR MUSCLE PAIRS -----------------------------------------

# initiate dataframe
df.pairs <- data.frame(
  id      = rep(1:20, each = length(pairs)),      # subject id
  pair    = rep(pairs, times = 20),               # muscle pair name
  m1      = rep(substr(pairs, 1, 3), times = 20), # muscle 1 name
  m2      = rep(substr(pairs, 5, 7), times = 20), # muscle 2 name
  n_stim  = NA,                                   # number of stimulations 
  n_mep   = NA,                                   # number of stimulations with some mep
  f_m1    = NA,                                   # response frequency muscle 1
  f_m2    = NA,                                   # response frequency muscle 2
  f_and   = NA,                                   # response frequency both together
  f_or    = NA,                                   # response frequency either
  ji      = NA,                                   # Jaccard index
  pmi     = NA                                    # pointwise mutual information
)

# fill in dataframe
for (i in 1:20){
  
  # select binary data for current subject
  bindat = df.binary %>% filter(id == i)
  # row-indices for current subject in dataframe
  is_id <- (df.pairs$id == i)
  # column-indices for all muscles in binary data
  col_all <- which(names(bindat) %in% muscles)
  # get total number of stimulations
  n_stim <- bindat %>% 
    nrow()
  # get total number of stimulations with any response
  n_mep <- bindat[names(bindat) %in% muscles] %>% 
    mutate(n = rowSums(across(everything()))) %>% 
    filter(n > 0) %>% 
    nrow()
  # fill dataframe
  df.pairs$n_stim[is_id] <- n_stim
  df.pairs$n_mep[is_id]  <- n_mep
  
  for (j in seq_along(pairs)){
    
    # get muscles
    muscle1 <- substr(pairs[j], 1, 3)
    muscle2 <- substr(pairs[j], 5, 7)
    # column-indices of current muscles in binary data
    col1 <- which(names(bindat) == muscle1)
    col2 <- which(names(bindat) == muscle2)
    # frequencies for current muscles
    f_m1  <- mean(bindat[ ,col1] == 1)
    f_m2  <- mean(bindat[ ,col2] == 1)
    f_and <- mean(bindat[ ,col1] == 1 & bindat[ ,col2] == 1)
    f_or  <- mean(bindat[ ,col1] == 1 | bindat[ ,col2] == 1)
    # row-indices for current muscle pair in dataframe
    is_pair <- (df.pairs$pair == pairs[j])
    # fill dataframe
    df.pairs$f_m1[is_id & is_pair]  <- f_m1
    df.pairs$f_m2[is_id & is_pair]  <- f_m2
    df.pairs$f_and[is_id & is_pair] <- f_and
    df.pairs$f_or[is_id & is_pair]  <- f_or
    df.pairs$ji[is_id & is_pair]    <- f_and / f_or
    df.pairs$pmi[is_id & is_pair]   <- log2(f_and / (f_m1 * f_m2))
    #                               == log(f_and / f_m2) - log(f_m1) (+ numerical error)
    #                               == log(f_and / f_m1) - log(f_m2) (+ numerical error)

  }
}

# add categories for muscle pairs
cat1 <- c(rep("Hand", times = 4), rep("Forearm", times = 4)) # category 1
cat2 <- c("Index", "Little", "Thumb", "Thumb",               # category 2
          "Extension", "Flexion", "Extension", "Flexion")    

df.pairs <- df.pairs %>% 
  rowwise() %>% 
  # add muscle categories 1 and 2
  mutate(m1c1 = cat1[which(muscles == m1)],
         m2c1 = cat1[which(muscles == m2)],
         m1c2 = cat2[which(muscles == m1)],
         m2c2 = cat2[which(muscles == m2)]) %>% 
  # add pair category 1
  mutate(pc1 = paste(m1c1, m2c1, sep = "-")) %>%
  # add pair category 2
  mutate(pc2 = case_when(
    # start with the one farther in alphabet
    m1c2 > m2c2 ~ paste(m1c2, m2c2, sep = "-"),
    TRUE        ~ paste(m2c2, m1c2, sep = "-")
  )) %>% 
  # edit pair category 2
  mutate(pc2 = case_when(
    pc2 == "Little-Index" ~ "Index-Little", # Index-Little sounds better
    pc1 == "Hand-Forearm" ~ "-",            # get rid of those
    TRUE                  ~ pc2             # leave the rest as is
  ))

# ADD MUSCLE / MUSCLE PAIR NAMES IN NEW ORDER -------------------------------------------

# order muscles manually (thumb, index, little, wrist flexion, wrist extension)
muscles_ordered <- c(
  "APB", "FPB", "FDI", "ADM", "FDS", "FCR", "EDC", "ECR"
)

# order muscle pairs manually (Thumb-Thumb, Thumb-Index, Thumb-Little, Index-Little,
#                              Flexion-Flexion, Extension-Extension, Flexion-Extension)
pairs_ordered <- c(
  "APB_FPB", "FDI_APB", "FDI_FPB", "ADM_APB", "ADM_FPB", "FDI_ADM",
  "FDS_FCR", "EDC_ECR", "FDS_ECR", "EDC_FDS", "EDC_FCR", "ECR_FCR",
  unique(df.pairs$pair[df.pairs$pc1 == "Hand-Forearm"])
)

# set muscle pair_names such that it aligns with cat2
pairnames_ordered <- vector(length = length(pairs_ordered))
for (i in seq_along(pairs_ordered)){
  pair <- pairs_ordered[i]
  m    <- c(substr(pair, 1, 3), substr(pair, 5, 7))
  ind  <- c(which(muscles_ordered == m[1]), which(muscles_ordered == m[2]))
  first <- which(ind == min(ind))
  last  <- which(ind == max(ind))
  pairnames_ordered[i] <- paste(m[first], m[last], sep = "_")
}

# add to dataframes
df.muscles <- df.muscles %>%
  mutate(m_ordered = factor(m, levels = muscles_ordered))

df.pairs <- df.pairs %>% 
  mutate(pair_ordered = factor(pair, levels = pairs_ordered, labels = pairnames_ordered))

# COLORS FOR PLOTTING -------------------------------------------------------------------

plotcolors <- colortools::splitComp("steelblue", plot = FALSE)[order(c(2,1,3))]
  
# KEEP ONLY RELEVANT STUFF --------------------------------------------------------------

gdata::keep(df.binary,
            df.muscles,
            df.pairs,
            plotcolors,
            sure = TRUE)




