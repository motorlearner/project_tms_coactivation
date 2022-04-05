# SETUP ---------------------------------------------------------------------------------

# load packages
require(here)
require(tidyverse)
require(patchwork)

# load plot theme
source(here::here("R", "theme.R"))

# SIMULATION ----------------------------------------------------------------------------

# number of repetitions per location in parameter space
nreps <- 100

# parameters
n <- 500                 # number of stimulations
p <- seq(.01, .99, .01)  # unconditional probabilities
r <- seq(1.0, 1.9, 0.3)  # ratio of joint to joint under independence

# initialize dataframe
df <- expand_grid(
  px  = p,
  py  = p,
  r   = r,
  jac = NA,
  pmi = NA
) %>% 
  # constrain parameter space with probability axioms
  rowwise() %>% 
  filter((px+py-min(px+py,1))/(px*py) <= r & r <= 1/max(px,py))

# fill in dataframe
for (i in 1:nrow(df)){
  # get current parameters
  px <- df$px[i]
  py <- df$py[i]
  r  <- df$r[i]
  # get P(y=1|x=1), P(y=1|x=0)
  pyifx1 <- (r * py)
  pyifx0 <- (py - px * pyifx1) / (1 - px)
  # correct numerical error
  if(pyifx0 < 0 & pyifx0 > -0.000000001){pyifx0 = 0}
  if(pyifx0 > 1 & pyifx0 <  1.000000001){pyifx0 = 1}
  # initialize containers to store outcomes from each rep
  pmi = jac <- vector(length = nreps)
  # carry out reps
  for(j in 1:nreps){
    # get x
    x <- rbinom(n, 1, px)
    # get x=1, x=0 indices
    xis1  <- which(x == 1)
    xis0 <- which(x == 0)
    # get y
    y <- vector(length = n)
    y[xis1] <- rbinom(length(xis1), 1, pyifx1)
    y[xis0] <- rbinom(length(xis0), 1, pyifx0)
    # get freqs
    fx <- mean(x==1)
    fy <- mean(y==1)
    fxy <- mean(x==1 & y==1)
    # get outcomes
    jac[j] <- 1 / (((fx+fy)/fxy)-1)
    pmi[j] <- log2(fxy/(fx*fy))
  }
  # fill in means outcomes across all reps
  df$jac[i] <- mean(jac)
  df$pmi[i] <- mean(pmi)
  # print progress report
  percent <- round(i /nrow(df) * 100, 0)
  howmany <- percent %/% 5
  cat(
    "\r",
    "Running Simulation ... ",
    "Progress:",
    "   ",
    "|", rep("=", howmany), rep(" ", 100/5 - howmany), "|", 
    "   ",
    i, " of ", nrow(df),
    sep = ""
  )
}

# CREATE PLOTS -------------------------------------------------------------------------

# Jaccard index
p1 <- df %>% ggplot() +
  # theme
  theme_master() + theme(legend.position = "right",
                         aspect.ratio = 1) +
  # data
  facet_wrap(~ r, nrow=1, labeller = label_bquote(R==.(r))) +
  geom_tile(aes(x = px, y = py, fill = jac)) +
  # color
  scale_fill_viridis_c(name = expression(frac(1,k)~sum(JAC[j],j==1,k)(x,y)),
                       limits = c(0,1),
                       breaks = seq(0,1,0.2),
                       option = "viridis") +
  # axes
  scale_x_continuous(name = expression(p[X](x)),
                     limits = c(0,1),
                     breaks = c(0,1),
                     expand = expansion(0)) +
  scale_y_continuous(name = expression(p[Y](y)),
                     limits = c(0,1),
                     breaks = c(0,1),
                     expand = expansion(0))

# pointwise mutual information
p2 <- df %>% ggplot() +
  # theme
  theme_master() + theme(legend.position = "right",
                         aspect.ratio = 1) +
  # data
  facet_wrap(~ r, nrow=1, labeller = label_bquote(R==.(r))) +
  geom_tile(aes(x = px, y = py, fill = pmi)) +
  # color
  scale_fill_viridis_c(name = expression(frac(1,k)~sum(PMI[j],j==1,k)(x,y)),
                       breaks = seq(0, 1, 0.2),
                       option = "viridis") +
  # axes
  scale_x_continuous(name = expression(p[X](x)),
                     limits = c(0,1),
                     breaks = c(0,1),
                     expand = expansion(0)) +
  scale_y_continuous(name = expression(p[Y](y)),
                     limits = c(0,1),
                     breaks = c(0,1),
                     expand = expansion(0))

# SAVE PLOTS ----------------------------------------------------------------------------

p1 %>% ggsave(filename = "sim_jac.png",
              path     = here::here("plots"),
              dpi      = 500, 
              width    = 500,
              height   = 250,
              units    = "mm",
              scale    = .26)

p2 %>% ggsave(filename = "sim_pmi.png",
              path     = here::here("plots"),
              dpi      = 500, 
              width    = 500,
              height   = 250,
              units    = "mm",
              scale    = .26)

