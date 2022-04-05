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

# up to which n?
nmax <- 100000

# parameters
n  <- round(10^(seq(2,log10(nmax),0.05)),0)
r  <- c(0.5, 1, 1.5) 
p  <- c(0.1, 0.2, 0.3)

# initialize dataframe
df <- expand_grid(
  rep = 1:nreps,
  n   = n,
  px  = p,
  py  = p,
  r   = r,
  jac = NA,
  pmi = NA
) %>% 
  # constrain parameter space with probability axioms
  rowwise() %>% 
  filter((px+py-min(px+py,1))/(px*py) <= r & r <= 1/max(px,py)) %>% 
  ungroup() %>% 
  # get true jac and pmi
  group_by(px, py, r) %>% 
  mutate(pxy = r*px*py) %>% 
  mutate(truepmi = log2(r),
         truejac = 1 / (((px+py)/pxy)-1))

# fill in dataframe
for (i in 1:nrow(df)){
  # get current parameters
  n  <- df$n[i]
  px <- df$px[i]
  py <- df$py[i]
  r  <- df$r[i]
  # get P(y=1|x=1), P(y=1|x=0)
  pyifx1 <- (r * py)
  pyifx0 <- (py - px * pyifx1) / (1 - px)
  # correct numerical error
  if(pyifx0 < 0 & pyifx0 > -0.000001){pyifx0 = 0}
  if(pyifx0 > 1 & pyifx0 <  1.000001){pyifx0 = 1}
  # get x
  x <- rbinom(n, 1, px)
  # get x=1, x=0 indices
  xis1 <- which(x == 1)
  xis0 <- which(x == 0)
  # get y
  y <- vector(length = n)
  y[xis1] <- rbinom(length(xis1), 1, pyifx1)
  y[xis0] <- rbinom(length(xis0), 1, pyifx0)
  # get freqs
  fx  <- mean(x==1)
  fy  <- mean(y==1)
  fxy <- mean(x==1 & y==1)
  # get outcomes
  df$jac[i] <- 1 / (((fx+fy)/fxy)-1)
  df$pmi[i] <- log2(fxy/(fx*fy))
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

p <- df %>%
  # convert -inf to lower axis limit; otherwise ribbons will have no fill
  mutate(pmi = case_when(
    is.finite(pmi)   ~ pmi,
    is.infinite(pmi) ~ -4
  )) %>% 
  ggplot() +
  # design
  theme_master() +
  theme(aspect.ratio = 1) +
  theme(panel.spacing = unit(15, "pt")) +
  # axes
  coord_cartesian(ylim = c(-4, 4)) +
  scale_x_continuous(name = expression(n),
                     limits = c(min(df$n),max(df$n)),
                     breaks = 10^(2:log10(nmax)),
                     labels = expression(10^2, 10^3, 10^4, 10^5),
                     trans  = "log10",
                     expand = expansion(0)) +
  scale_y_continuous(name = "PMI(x,y)",
                     breaks = c(-2,0,2),
                     expand = expansion(0)) +
  # colors
  scale_color_manual(values = c("deepskyblue", "gray20", "darkorange")) +
  scale_fill_manual (values = c("deepskyblue", "gray20", "darkorange")) +
  # data
  facet_grid(rows = vars(py),
             cols = vars(px),
             labeller = label_bquote(
               rows = {p[Y]("y")==.(py)}, 
               cols = {p[X]("x")==.(px)}
             )
  ) +
  stat_summary(aes(x = n, y = pmi, 
                   group = as.character(truepmi), 
                   color = as.character(truepmi),
                   fill  = as.character(truepmi)),
               geom = "ribbon",
               size = 0.5,
               alpha = 0.2,
               fun     = function(z){quantile(z, probs=0.50)},
               fun.max = function(z){quantile(z, probs=0.90)},
               fun.min = function(z){quantile(z, probs=0.10)}) +
  geom_line(aes(x = n, y = truepmi, 
                color = as.character(truepmi), 
                group = as.character(truepmi)),
            size = 0.5, linetype = "dashed"); p

# SAVE PLOTS ----------------------------------------------------------------------------

p %>% ggsave(filename = "sim_pmi_spreadbyn.png",
             path     = here::here("plots"),
             dpi      = 500, 
             width    = 500,
             height   = 500,
             units    = "mm",
             scale    = .26)


