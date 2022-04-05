# SETUP ---------------------------------------------------------------------------------

# load packages
require(here)
require(tidyverse)
require(ggridges)
require(patchwork)

# load data
source(here::here("R", "getdata_binary.R"))
# load plot theme
source(here::here("R", "theme.R"))

# PLOT ALL PAIRS ------------------------------------------------------------------------

# Jaccard index
p1 <- df.pairs %>% 
  # change pc1 order for coloring
  mutate(pc1 = factor(pc1, 
                      levels = c("Hand-Hand", "Forearm-Forearm", "Hand-Forearm")
  )) %>% 
  # theme
  ggplot() + theme_master() + theme(
    plot.margin = margin(10,10,10,2),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(vjust=0)
  ) +
  # data
  ggridges::geom_density_ridges(aes(x = ji, y = pair_ordered, fill = pc1),
                                color = "white",
                                scale = 1.5,
                                bandwidth = .03,
                                quantile_lines = TRUE,
                                quantiles = 2) +
  # colors
  scale_fill_manual(values = plotcolors) +
  # axes
  scale_x_continuous(name = "JAC",
                     limits = c(0,1),
                     breaks = seq(0, 1, 0.25),
                     expand = expansion(0)) +
  scale_y_discrete(name = "Muscle Pair",
                   limits = rev,
                   expand = expansion(add = c(0, 1.2)))

# Pointwise mutual information
p2 <- df.pairs %>% 
  # change pc1 order for coloring
  mutate(pc1 = factor(pc1, 
                      levels = c("Hand-Hand", "Forearm-Forearm", "Hand-Forearm")
  )) %>% 
  # theme
  ggplot() + theme_master() + theme(
    panel.grid = element_blank(),
    plot.margin = margin(10,10,10,2),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  # data
  ggridges::geom_density_ridges(aes(x = pmi, y = pair_ordered, fill = pc1),
                                color = "white",
                                scale = 2.0,
                                bandwidth = .03*2,
                                quantile_lines = TRUE,
                                quantiles = 2) +
  # colors
  scale_fill_manual(values = plotcolors) +
  # axes
  scale_x_continuous(name = "PMI",
                     limits = c(NA,NA),
                     breaks = seq(0, 10, 1),
                     expand = expansion(0)) +
  scale_y_discrete(name = "Muscle Pair",
                   limits = rev,
                   expand = expansion(add = c(0, 1.2))) +
  # custom legend
  coord_cartesian(xlim = c(NA, 3.7), ylim = c(1,28), clip = "off") +
  theme(plot.margin = margin(10,30,10,10)) +
  annotate("rect",
           xmin = 3.7 + 0.12,
           xmax = 3.7 + 0.18,
           ymin = c(1, 17.0, 23.0),
           ymax = c(16.5, 22.5, 29.0),
           color = rev(plotcolors),
           fill = rev(plotcolors)) +
  annotate("segment",
           x = 3.7 + 0.18,
           xend = 3.7 + 0.18,
           y = c(1, 17.0, 23.0),
           yend = c(16.5, 22.5, 29.0)) +
  annotate("segment",
           x = 3.7 + 0.06,
           xend = 3.7 + 0.18,
           y = c(1, 16.5, 17.0, 22.5, 23.0, 29.0),
           yend = c(1, 16.5, 17.0, 22.5, 23.0, 29.0)) +
  annotate("text",
           size = 3,
           hjust = 0, 
           vjust = .5,
           label = c("HF", "FF", "HH"),
           x = 3.7 + 0.22,
           y = c(8.5, 20.0, 26.0))

# PLOT HAND-HAND PAIRS ------------------------------------------------------------------

# Jaccard index
p3 <- df.pairs %>% 
  # only hand-hand pairs
  filter(pc1 == "Hand-Hand") %>% 
  # theme
  ggplot(aes(x = ji, y = pair_ordered)) + theme_master() + theme(
    plot.margin = margin(10,10,10,2),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(vjust=0)
  ) +
  # data
  ggdist::stat_halfeye(.width = 0,
                       point_colour = NA,
                       adjust = 0.4,
                       height = 0.5,
                       justification = -0.15,
                       fill = plotcolors[1]) +
  geom_boxplot(size = .5,
               width = 0.12,
               outlier.shape = NA,
               color = "black",
               fill = "white") +
  ggdist::stat_dots(side = "left",
                    dotsize = 1.25,
                    shape = 21,
                    justification = 1.1,
                    binwidth = .02,
                    stackratio = .6,
                    color = plotcolors[1],
                    fill = plotcolors[1]) +
  # axes
  scale_x_continuous(name = "JAC",
                     limits = c(0,1),
                     breaks = seq(0, 1, 0.25),
                     expand = expansion(0)) +
  scale_y_discrete(name = "Muscle Pair",
                   limits = rev,
                   expand = expansion(add = c(0.5, 0.5))) +
  coord_cartesian(xlim = c(0,1), ylim = c(1,NA), clip="off") # remove whitespace

# Pointwise mutual information
p4 <- df.pairs %>% 
  # only hand-hand pairs
  filter(pc1 == "Hand-Hand") %>% 
  # theme
  ggplot(aes(x = pmi, y = pair_ordered)) + theme_master() + theme(
    plot.margin = margin(10,10,10,2),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  # data
  ggdist::stat_halfeye(.width = 0,
                       point_colour = NA,
                       adjust = 0.4,
                       height = 0.5,
                       justification = -0.15,
                       fill = plotcolors[1]) +
  geom_boxplot(size = .5,
               width = 0.12,
               outlier.shape = NA,
               color = "black",
               fill = "white") +
  ggdist::stat_dots(side = "left",
                    dotsize = 2.3,
                    shape = 21,
                    justification = 1.1,
                    binwidth = .02,
                    stackratio = .6,
                    color = plotcolors[1],
                    fill = plotcolors[1]) +
  # axes
  scale_x_continuous(name = "PMI",
                     limits = c(NA,NA),
                     breaks = seq(0, 10, 0.5),
                     expand = expansion(0)) +
  scale_y_discrete(name = "Muscle Pair",
                   limits = rev,
                   expand = expansion(add = c(0.5, 0.5))) +
  coord_cartesian(xlim = c(0.62,2.45), ylim = c(1,NA), clip = "off") + # remove whitespace
  # custom legend
  theme(plot.margin = margin(10, 20, 10, 10)) +
  annotate("segment",
           x = 2.45 + .05,
           xend = 2.45 + .05,
           y = c(0.6, 1.6, 3.6, 5.6),
           yend = c(1.4, 3.4, 5.4, 6.4)) +
  annotate("segment",
           x = 2.45 + .02,
           xend = 2.45 + .05,
           y = c(0.6, 1.4, 1.6, 3.4, 3.6, 5.4, 5.6, 6.4),
           yend = c(0.6, 1.4, 1.6, 3.4, 3.6, 5.4, 5.6, 6.4)) +
  annotate("text",
           size = 3,
           hjust = 0, 
           vjust = .5,
           label = c("IL", "TL", "TI", "TT"),
           x = 2.45 + .08,
           y = c(1, 2.5, 4.5, 6))

# PLOT FOREARM-FOREARM PAIRS ------------------------------------------------------------------

# Jaccard index
p5 <- df.pairs %>% 
  # only forearm-forearm pairs
  filter(pc1 == "Forearm-Forearm") %>% 
  # theme
  ggplot(aes(x = ji, y = pair_ordered)) + theme_master() + theme(
    plot.margin = margin(10,10,10,2),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(vjust=0)
  ) +
  # data
  ggdist::stat_halfeye(.width = 0,
                       point_colour = NA,
                       adjust = 0.4,
                       height = 0.5,
                       justification = -0.15,
                       fill = plotcolors[2]) +
  geom_boxplot(size = .5,
               width = 0.12,
               outlier.shape = NA,
               color = "black",
               fill = "white") +
  ggdist::stat_dots(side = "left",
                    dotsize = 1.25,
                    shape = 21,
                    justification = 1.1,
                    binwidth = .02,
                    stackratio = .6,
                    color = plotcolors[2],
                    fill = plotcolors[2]) +
  # axes
  scale_x_continuous(name = "JAC",
                     limits = c(0,1),
                     breaks = seq(0, 1, 0.25),
                     expand = expansion(0)) +
  scale_y_discrete(name = "Muscle Pair",
                   limits = rev,
                   expand = expansion(add = c(0.5, 0.5))) +
  coord_cartesian(xlim = c(0,1), ylim = c(1,NA), clip="off") # remove whitespace

# Pointwise mutual information
p6 <- df.pairs %>% 
  # only forearm-forearm pairs
  filter(pc1 == "Forearm-Forearm") %>% 
  # theme
  ggplot(aes(x = pmi, y = pair_ordered)) + theme_master() + theme(
    plot.margin = margin(10,10,10,2),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  # data
  ggdist::stat_halfeye(.width = 0,
                       point_colour = NA,
                       adjust = 0.4,
                       height = 0.5,
                       justification = -0.15,
                       fill = plotcolors[2]) +
  geom_boxplot(size = .5,
               width = 0.12,
               outlier.shape = NA,
               color = "black",
               fill = "white") +
  ggdist::stat_dots(side = "left",
                    dotsize = 3.7,
                    shape = 21,
                    justification = 1.1,
                    binwidth = .02,
                    stackratio = .6,
                    color = plotcolors[2],
                    fill = plotcolors[2]) +
  # axes
  scale_x_continuous(name = "PMI",
                     limits = c(NA,NA),
                     breaks = seq(0, 10, 0.5),
                     expand = expansion(0)) +
  scale_y_discrete(name = "Muscle Pair",
                   limits = rev,
                   expand = expansion(add = c(0.5, 0.5))) +
  coord_cartesian(xlim = c(0.27,3.38), ylim = c(1,NA), clip = "off") + # remove whitespace
  # custom legend
  theme(plot.margin = margin(10, 20, 10, 10)) +
  annotate("segment",
           x = 3.38 + .08,
           xend = 3.38 + .08,
           y = c(0.6, 4.6, 5.6),
           yend = c(4.4, 5.4, 6.4)) +
  annotate("segment",
           x = 3.38 + .03,
           xend = 3.38 + .08,
           y = c(0.6, 4.4, 4.6, 5.4, 5.6, 6.4),
           yend = c(0.6, 4.4, 4.6, 5.4, 5.6, 6.4)) +
  annotate("text",
           size = 3,
           hjust = 0, 
           vjust = .5,
           label = c("FE", "EE", "FF"),
           x = 3.38 + .12,
           y = c(2.5, 5, 6))

# PLOT MUSCLES --------------------------------------------------------------------------

# response frequency
p7 <- df.muscles %>% 
  # theme
  ggplot(aes(x = n/n_stim, y = m_ordered)) + theme_master() + theme(
    plot.margin = margin(10,10,10,2),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(vjust=0)
  ) +
  # data
  ggdist::stat_halfeye(.width = 0,
                       point_colour = NA,
                       adjust = 0.4,
                       height = 0.5,
                       justification = -0.15,
                       fill = "gray50") +
  geom_boxplot(size = .5,
               width = 0.12,
               outlier.shape = NA,
               color = "black",
               fill = "white") +
  ggdist::stat_dots(side = "left",
                    dotsize = 1,
                    shape = 21,
                    justification = 1.1,
                    binwidth = .02,
                    stackratio = .6,
                    color = "gray50",
                    fill = "gray50") +
  # axes
  scale_x_continuous(name = "Response Frequency",
                     limits = c(0,1),
                     breaks = seq(0, 1, 0.25),
                     expand = expansion(0)) +
  scale_y_discrete(name = "Muscle",
                   limits = rev,
                   expand = expansion(add = c(0, 1)))

# response frequency rank
p8 <- df.muscles %>%
  # rank muscles by response frequency
  group_by(id) %>% 
  mutate(rank = rank(n, ties.method = "max")) %>% 
  # theme
  ggplot(aes(x = rank, y = m_ordered)) + theme_master() + theme(
    plot.margin = margin(10,10,10,2),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    aspect.ratio = 3
  ) +
  # data
  ggdist::stat_dots(side = "right",
                    dotsize = 20,
                    shape = 22,
                    justification = -.02,
                    binwidth = .02,
                    stackratio = .5,
                    color = "gray50",
                    fill = "gray50") +
  geom_hline(yintercept = 1:8, color="gray") +
  # axes
  scale_x_continuous(name = "Rank",
                     limits = c(1,8),
                     breaks = 1:8,
                     expand = expansion(0.2)) +
  scale_y_discrete(name = "Muscle",
                   limits = rev,
                   expand = expansion(add = c(1.1, 0)))

# COMBINE AND SAVE PLOTS ----------------------------------------------------------------

(p1 + p2 + plot_layout(ncol=2)) %>%  
  ggsave(filename = "pairs_all.png", 
         path     = here::here("plots"), 
         dpi      = 500, 
         width    = 500,
         height   = 500,
         units    = "mm",
         scale    = .27)

(p3 + p4 + plot_layout(ncol=2)) %>% 
  ggsave(filename = "pairs_hh.png", 
         path     = here::here("plots"), 
         dpi      = 500, 
         width    = 500,
         height   = 350,
         units    = "mm",
         scale    = .27)

(p5 + p6 + plot_layout(ncol=2)) %>% 
  ggsave(filename = "pairs_ff.png", 
         path     = here::here("plots"), 
         dpi      = 500, 
         width    = 500,
         height   = 350,
         units    = "mm",
         scale    = .27)

(p7 + p8 + plot_layout(ncol=2)) %>% 
  ggsave(filename = "muscles.png", 
         path     = here::here("plots"), 
         dpi      = 500, 
         width    = 500,
         height   = 400,
         units    = "mm",
         scale    = .27)
