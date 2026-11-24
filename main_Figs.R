library(patchwork)
library(tidyverse)
library(ggh4x)
library(ggridges)
library(cowplot)
library(broom)

source("Fn_myplot_lm.R")

####---------------------Main Figs---------------------####
####Fig 2: community stability and its components####
All_dat <- bind_rows(
  read.csv("Dataset/Duolun_functional_group_stability.csv"),
  read.csv("Dataset/e001_functional_group_stability.csv")) %>%
  filter(Method %in% "Raw")
groups_now <- c("All", "C3 grass", "C4 grass", "Forb")

p1 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Scom",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Scom)),
  ncol = 4, degree = 1)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Spop",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Spop)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Asyn",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Asyn)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank())

tiff("Figs/Fig 2.tiff", width = 16, height = 12, units = "in", res = 300)
wrap_plots(
  p1, p2, p3, 
  ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()

####Fig 3: compensatory effects and statistical-averaging effects####
p1 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "CPE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(CPE^bold("2"))),
  ncol = 4, degree = 1) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.03, 0.20))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "SAE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(SAE)),
  ncol = 4, degree = 1) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.03, 0.20))) +
  theme(strip.text = element_blank())

tiff("Figs/Fig 3.tiff", width = 15.5, height = 8, units = "in", res = 300)
wrap_plots(
  p1, p2,
  ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()


####Fig 4: species-level temporal variance####
#### 1. Read plotting data
dumbbell_plot_data <- read.csv("Dataset/Functional_group_variance_dumbbell_plot_data.csv")

#### 2. Basic settings
field_levels <- c("Duolun", "CDR B", "CDR C")
series_levels <- c("C3 variance", "Community variance",
                   "C4 variance", "Forb variance")
series_labels <- c("C3 variance" = "C3 var.",
                   "Community variance" = "Whole var.",
                   "C4 variance" = "C4 var.",
                   "Forb variance" = "Forb var.")
series_shapes <- c("C3 variance" = 17,
                   "Community variance" = 16,
                   "C4 variance" = 0,
                   "Forb variance" = 5)
series_sizes <- c("C3 variance" = 5.5,
                  "Community variance" = 6,
                  "C4 variance" = 4.5,
                  "Forb variance" = 4.5)
series_alpha <- c("C3 variance" = 1,
                  "Community variance" = 1,
                  "C4 variance" = 1,
                  "Forb variance" = 1)
  
#### 3. Nitrogen order
n_levels <- rev(sort(unique(dumbbell_plot_data$Nitrogen)))

#### 4. Separate point and link layers
plot_points <- dumbbell_plot_data %>%
  filter(Layer == "point") %>%
  mutate(
    Field = factor(Field, levels = field_levels),
    Treatment_key = paste(Field, NTrt, sep = "__"),
    Nitrogen_f = factor(Nitrogen, levels = n_levels),
    Series = factor(Series, levels = series_levels))

links_C3 <- dumbbell_plot_data %>%
  filter(Layer == "link") %>%
  mutate( Field = factor(Field, levels = field_levels),
          Nitrogen_f = factor(Nitrogen, levels = n_levels))

#### 5. Treatment color palettes
col1 <- c("#F0E2DD", "#DDBEB3", "#C58F82", "#AF665B", "#983451")
col2 <- c("#F3E8D2", "#EED6AE", "#E8C58F", "#E4B77A", "#D6A45E", "#C18D4A", "#A8753C", "#8E6338")
col3 <- c("#DCE3ED", "#C4D0E1", "#A9B9D0", "#8EA3C0", "#738BAC", "#58688F", "#485775", "#39475F")
palettes <- list("Duolun" = col1, "CDR B"  = col2, "CDR C"  = col3)
  
#### 6. Match treatment colors
color_map <- unlist(
  lapply(field_levels, function(f) {
    
    trts <- plot_points %>%
      filter(Field == f) %>%
      distinct(NTrt, Nitrogen) %>%
      arrange(Nitrogen)
    
    if (nrow(trts) != length(palettes[[f]])) {
      stop("Number of treatment colors does not match: ", f)
    }
    
    setNames(
      palettes[[f]],
      paste(f, trts$NTrt, sep = "__")
    )
  })
)


tiff("Figs/Fig 4.tiff", width = 10, height = 4, units = "in", res = 300)
ggplot() +
  #### Reference line
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "grey50", linewidth = 0.7) +
  #### C3-community connection
  geom_segment(data = links_C3,
               aes(x = x, xend = xend, y = Nitrogen_f,
                   yend = Nitrogen_f), color = "grey65",
               linewidth = 0.8) +
  geom_point(data = plot_points,
             aes(x = Estimate, y = Nitrogen_f,
                 color = Treatment_key, shape = Series, 
                 size = Series, alpha = Series), stroke = 1.05) +
  facet_wrap(~ Field, nrow = 1, scales = "free_x") +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0.03, 0.08))) +
  scale_color_manual(values = color_map, guide = "none") +
  scale_shape_manual(values = series_shapes,
                     labels = series_labels,
                     name = NULL,
                     guide = guide_legend(
                       override.aes = list(
                         size = c(3, 3.7, 2.7, 2.3),
                         alpha = c(1, 1, 1, 1)))) +
  scale_size_manual(values = series_sizes, guide = "none") +
  scale_alpha_manual(values = series_alpha, guide = "none") +
  labs(x = "Variance fold change relative to control",
       y = paste0("Nitrogen treatment",  "<br>",
                  "<span style='font-size:11pt; font-weight:normal;'>",
                  "(g N m<sup>-2</sup> yr<sup>-1</sup>)", "</span>")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.background = element_rect( fill = "transparent", colour = NA),
        legend.key.height = grid::unit(0.3, "cm"),
        strip.text = element_text(face = "bold", size = 14),
        axis.title.x = element_text(size = 15, color = "black", face = "bold"),
        axis.title.y = ggtext::element_markdown(size = 15, color = "black", face = "bold", lineheight = 0.85),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.spacing = grid::unit(1.2, "lines"),
        legend.direction = "vertical",
        legend.position = c(0.32, 1),
        legend.justification = c(1, 1))
dev.off()

####Fig 5: species richness and statistical-averaging effects####
p1 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Richness",
  groups = groups_now, facet_by = "Group",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = "Richness",
  ncol = 4, degree = 1)+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.03, 0.2)))

p2 <- myplot.field.3(
  data = All_dat, xvar = "Richness", yvar = "SAE",
  facet_scales = "free",
  groups = groups_now, facet_by = "Group",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Richness",
  ylab = expression(bolditalic(SAE)),
  ncol = 4, degree = 1)+
  theme(strip.text = element_blank())+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.03, 0.25)))


tiff("Figs/Fig 5.tiff", width = 16, height = 9, units = "in", res = 300)
wrap_plots(
  p1, p2,
  ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()

####---------------------Figs for SI---------------------####
####Fig S1: species richness and community stability and its components####
p1 <- myplot.field.3(
  data = All_dat, xvar = "Richness", yvar = "Scom",
  groups = "All", facet_by = "Group",
  facet_scales = "free",
  xlab = "Richness",
  ylab = expression(bolditalic(Scom)),
  degree = 1)+
  theme(strip.text = element_blank())

p2 <- myplot.field.3(
  data = All_dat, xvar = "Richness", yvar = "Spop",
  groups = "All", facet_by = "Group",
  facet_scales = "free",
  xlab = "Richness",
  ylab = expression(bolditalic(Spop)),
  degree = 1) +
  theme(strip.text = element_blank())

p3 <- myplot.field.3(
  data = All_dat, xvar = "Richness", yvar = "Asyn",
  groups = "All", facet_by = "Group",
  facet_scales = "free",
  xlab = "Richness",
  ylab = expression(bolditalic(Asyn)),
  degree = 1) +
  theme(strip.text = element_blank())

p4 <- myplot.field.3(
  data = All_dat, xvar = "Richness", yvar = "CPE",
  groups = "All", facet_by = "Group",
  facet_scales = "free",
  xlab = "Richness",
  ylab = expression(bolditalic(CPE^bold("2"))),
  degree = 1) +
  theme(strip.text = element_blank())

p5 <- myplot.field.3(
  data = All_dat, xvar = "Richness", yvar = "SAE",
  groups = "All", facet_by = "Group",
  facet_scales = "free",
  xlab = "Richness",
  ylab = expression(bolditalic(SAE)),
  degree = 1) +
  theme(strip.text = element_blank())


legend <- cowplot::get_legend(
  p1 + 
    theme(legend.position = "right",
          legend.direction = "vertical",
          legend.title = element_blank()))

plots <- lapply(
  list(p1, p2, p3, p4, p5),
  function(p){
    wrap_elements(
      full = ggplotGrob(
        p + theme(legend.position = "none")))
  }
)

design <- c(
  area(t = 1, l = 2, b = 1, r = 3),  # p1
  area(t = 2, l = 1, b = 2, r = 2),  # p2
  area(t = 2, l = 3, b = 2, r = 4),  # p3
  area(t = 3, l = 1, b = 3, r = 2),  # p4
  area(t = 3, l = 3, b = 3, r = 4),  # p5
  area(t = 1, l = 4, b = 1, r = 4))   # Legend

tiff("SI_Figs/Fig S1.tiff", width = 9, height = 12, units = "in", res = 300)
wrap_plots(
  plots[[1]],
  plots[[2]],
  plots[[3]],
  plots[[4]],
  plots[[5]],
  wrap_elements(full = legend),
  design = design,
  widths = rep(1, 4),
  heights = rep(1, 3))
dev.off()


####Fig S20: among-group stability components####
p1 <- myplot.field.3(
  data = All_dat %>% filter(Group == "Between"),
  xvar = "Nitrogen_log", yvar = "Spop",
  groups = "Between", facet_by = "Group",
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Spop[Group])),
  degree = 1, facet_scales = "free") +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- myplot.field.3(
  data = All_dat %>% filter(Group == "Between"),
  xvar = "Nitrogen_log", yvar = "Asyn",
  groups = "Between", facet_by = "Group",
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Asyn[Group])),
  degree = 1, facet_scales = "free") +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p3 <- myplot.field.3(
  data = All_dat %>% filter(Group == "Between"),
  xvar = "Nitrogen_log", yvar = "CPE",
  groups = "Between", facet_by = "Group",
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(CPE[Group])),
  degree = 1, facet_scales = "free") +
  theme(strip.text = element_blank())

p4 <- myplot.field.3(
  data = All_dat %>% filter(Group == "Between"),
  xvar = "Nitrogen_log", yvar = "SAE",
  groups = "Between", facet_by = "Group",
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(SAE[Group])),
  degree = 1, facet_scales = "free") +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.03, 0.20))) +
  theme(strip.text = element_blank())

tiff("SI_Figs/Fig S20.tiff", width = 10, height = 10, units = "in", res = 300)
wrap_plots(
  p1, p2, p3, p4,
  ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()


####Fig S10: linear detrending####
All_dat <- bind_rows(
  read.csv("Dataset/Duolun_functional_group_stability.csv"),
  read.csv("Dataset/e001_functional_group_stability.csv")) %>%
  filter(Method %in% "Linear")

p1 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Scom",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Scom)),
  ncol = 4, degree = 1)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Spop",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Spop)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Asyn",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Asyn)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p4 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "CPE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(CPE^bold("2"))),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p5 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "SAE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(SAE)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank())

tiff("SI_Figs/Fig S10.tiff", width = 16, height = 18, units = "in", res = 300)
wrap_plots(
  p1, p2, p3, p4, p5,
  ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()

####Fig S11: T3 detrending####
All_dat <- bind_rows(
  read.csv("Dataset/Duolun_functional_group_stability.csv"),
  read.csv("Dataset/e001_functional_group_stability.csv")) %>%
  filter(Method %in% "T3")

p1 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Scom",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Scom)),
  ncol = 4, degree = 1)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Spop",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Spop)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Asyn",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Asyn)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p4 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "CPE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(CPE^bold("2"))),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p5 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "SAE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grasses",
                   "C4 grass" = "NSR in C4 grasses",
                   "Forb"  = "NSR in forbs"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(SAE)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank())

tiff("SI_Figs/Fig S11.tiff", width = 16, height = 18, units = "in", res = 300)
wrap_plots(
  p1, p2, p3, p4, p5,
  ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()
####Figs S2, S12 and S13: five plant functional groups for raw,linear detrending and T3 detrending####
All_dat <- bind_rows(
  read.csv("Dataset/Duolun_5_functional_group_stability.csv"),
  read.csv("Dataset/e001_5_functional_group_stability.csv")) %>%
  filter(Method %in% "Raw")
groups_now <- c("All", "C3 grass", "C4 grass", "Legume", "Sedge", "Non-legume forb")

p1 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Scom",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Scom)),
  ncol = 6, degree = 1)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Spop",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Spop)),
  ncol = 6, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Asyn",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Asyn)),
  ncol = 6, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p4 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "CPE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(CPE^bold("2"))),
  ncol = 6, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p5 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "SAE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(SAE)),
  ncol = 6, degree = 1) +
  theme(strip.text = element_blank())

tiff("SI_Figs/Fig S2.tiff", width = 24, height = 18, units = "in", res = 300)
wrap_plots(
  p1, p2, p3, p4, p5,
  ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()

All_dat <- bind_rows(
  read.csv("Dataset/Duolun_5_functional_group_stability.csv"),
  read.csv("Dataset/e001_5_functional_group_stability.csv")) %>%
  filter(Method %in% "Linear")
groups_now <- c("All", "C3 grass", "C4 grass", "Legume", "Sedge", "Non-legume forb")

p1 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Scom",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Scom)),
  ncol = 6, degree = 1)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Spop",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Spop)),
  ncol = 6, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Asyn",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Asyn)),
  ncol = 6, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p4 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "CPE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(CPE^bold("2"))),
  ncol = 6, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p5 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "SAE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(SAE)),
  ncol = 6, degree = 1) +
  theme(strip.text = element_blank())

tiff("SI_Figs/Fig S12.tiff", width = 24, height = 18, units = "in", res = 300)
wrap_plots(
  p1, p2, p3, p4, p5,
  ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()

All_dat <- bind_rows(
  read.csv("Dataset/Duolun_5_functional_group_stability.csv"),
  read.csv("Dataset/e001_5_functional_group_stability.csv")) %>%
  filter(Method %in% "T3")
groups_now <- c("All", "C3 grass", "C4 grass", "Legume", "Sedge", "Non-legume forb")

p1 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Scom",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Scom)),
  ncol = 6, degree = 1)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Spop",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Spop)),
  ncol = 6, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Asyn",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Asyn)),
  ncol = 6, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p4 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "CPE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(CPE^bold("2"))),
  ncol = 6, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p5 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "SAE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "C3 grass" = "NSR in C3 grass",
                   "C4 grass" = "NSR in C4 grass",
                   "Legume" = "NSR in legume",
                   "Sedge"  = "NSR in sedge",
                   "Non-legume forb"  = "NSR in non-legume forb"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(SAE)),
  ncol = 6, degree = 1) +
  theme(strip.text = element_blank())

tiff("SI_Figs/Fig S13.tiff", width = 24, height = 18, units = "in", res = 300)
wrap_plots(
  p1, p2, p3, p4, p5,
  ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()

####Figs S3, S14 and S15:three life-history groups for raw,linear detrending and T3 detrending####
All_dat <- bind_rows(
  read.csv("Dataset/Duolun_3_functional_group_stability.csv"),
  read.csv("Dataset/e001_3_functional_group_stability.csv")) %>%
  filter(Method %in% "Raw")
groups_now <- c("All", "Annual", "Perennial", "Others")

p1 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Scom",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Scom)),
  ncol = 4, degree = 1)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Spop",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Spop)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Asyn",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Asyn)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p4 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "CPE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(CPE^bold("2"))),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p5 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "SAE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(SAE)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank())

tiff("SI_Figs/Fig S3.tiff", width = 16, height = 18, units = "in", res = 300)
wrap_plots(
  p1, p2, p3, p4, p5,
  ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()

All_dat <- bind_rows(
  read.csv("Dataset/Duolun_3_functional_group_stability.csv"),
  read.csv("Dataset/e001_3_functional_group_stability.csv")) %>%
  filter(Method %in% "Linear")
groups_now <- c("All", "Annual", "Perennial", "Others")

p1 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Scom",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Scom)),
  ncol = 4, degree = 1)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Spop",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Spop)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Asyn",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Asyn)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p4 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "CPE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(CPE^bold("2"))),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p5 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "SAE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(SAE)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank())

tiff("SI_Figs/Fig S14.tiff", width = 16, height = 18, units = "in", res = 300)
wrap_plots(
  p1, p2, p3, p4, p5,
  ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()

All_dat <- bind_rows(
  read.csv("Dataset/Duolun_3_functional_group_stability.csv"),
  read.csv("Dataset/e001_3_functional_group_stability.csv")) %>%
  filter(Method %in% "T3")
groups_now <- c("All", "Annual", "Perennial", "Others")

p1 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Scom",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Scom)),
  ncol = 4, degree = 1)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Spop",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Spop)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "Asyn",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(Asyn)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p4 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "CPE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(CPE^bold("2"))),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p5 <- myplot.field.3(
  data = All_dat, xvar = "Nitrogen_log", yvar = "SAE",
  groups = groups_now, facet_by = "Group",
  facet_scales = "free_y",
  facet_labels = c("All"   = "NSR in whole",
                   "Annual" = "NSR in annuals",
                   "Perennial" = "NSR in perennials",
                   "Others"  = "NSR in others"),
  xlab = "Nitrogen addition",
  ylab = expression(bolditalic(SAE)),
  ncol = 4, degree = 1) +
  theme(strip.text = element_blank())

tiff("SI_Figs/Fig S15.tiff", width = 16, height = 18, units = "in", res = 300)
wrap_plots(
  p1, p2, p3, p4, p5,
  ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank())
dev.off()

####Figs S4-S9: biomass of functional groups#####
## A function to plot relative biomass by treatment and group
plot_relative_biomass <- function(
    data,
    group_cols,
    ylab = "Relative biomass",
    legend_position = "top",
    legend_nrow = 1,
    mycex = 1
) {
  ## keep functional group order
  data <- data %>%
    mutate(
      Group = factor(
        Group, levels = names(group_cols)))
  ggplot(data, aes(x = NTrt, y = Mean_relative, fill = Group)) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7, color = "white",
      linewidth = 0.3) +
    geom_errorbar(
      aes(ymin = pmax(0, Mean_relative - SE_relative),
          ymax = pmin(1, Mean_relative + SE_relative)),
      position = position_dodge(width = 0.8),
      width = 0.18, linewidth = 0.5) +
    scale_fill_manual(values = group_cols, drop = FALSE) +
    scale_y_continuous(
      labels = scales::percent_format(
        accuracy = 1),
      limits = c(0,1),
      expand = expansion(mult = c(0,0.05))) +
    labs(x = NULL,y = ylab,fill = NULL) +
    guides(fill = guide_legend(nrow = legend_nrow)) +
    theme_classic(base_size = 14 * mycex) +
    theme(axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black",face = "bold"),
          legend.position = legend_position,
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.text = element_text(size = 10 * mycex))
  
}

group_cols_5 <- c("C3 grass"        = "#60966D",
                  "C4 grass"        = "#83B09A",
                  "Sedge"           = "#547A9B",
                  "Legume"          = "#B58B3C",
                  "Non-legume forb" = "#C55645")

group_cols_3 <- c("Annual"    = "#C55645",
                     "Perennial" = "#60966D",
                     "Others"     = "#547A9B")

Duolun_bio <- read.csv("Dataset/Biomass_group_summary.csv") %>%
  filter(Field %in% "Duolun")
CDRB_bio <- read.csv("Dataset/Biomass_group_summary.csv") %>%
  filter(Field %in% "CDR B")
CDRC_bio <- read.csv("Dataset/Biomass_group_summary.csv") %>%
  filter(Field %in% "CDR C")

tiff("SI_Figs/Fig S4.tiff", width = 7, height = 5, units = "in", res = 300)
plot_relative_biomass(data = Duolun_bio %>% 
                        filter(Grouping_scheme %in% "Functional group"), 
                      group_cols = group_cols_5)
dev.off()

tiff("SI_Figs/Fig S5.tiff", width = 7, height = 5, units = "in", res = 300)
plot_relative_biomass(data = CDRB_bio %>% 
                        filter(Grouping_scheme %in% "Functional group"), 
                      group_cols = group_cols_5)
dev.off()

tiff("SI_Figs/Fig S6.tiff", width = 7, height = 5, units = "in", res = 300)
plot_relative_biomass(data = CDRC_bio %>% 
                        filter(Grouping_scheme %in% "Functional group"), 
                      group_cols = group_cols_5)
dev.off()

tiff("SI_Figs/Fig S7.tiff", width = 7, height = 5, units = "in", res = 300)
plot_relative_biomass(data = Duolun_bio %>% 
                        filter(Grouping_scheme %in% "Life-history group"), 
                      group_cols = group_cols_3)
dev.off()

tiff("SI_Figs/Fig S8.tiff", width = 7, height = 5, units = "in", res = 300)
plot_relative_biomass(data = CDRB_bio %>% 
                        filter(Grouping_scheme %in% "Life-history group"), 
                      group_cols = group_cols_3)
dev.off()

tiff("SI_Figs/Fig S9.tiff", width = 7, height = 5, units = "in", res = 300)
plot_relative_biomass(data = CDRC_bio %>% 
                        filter(Grouping_scheme %in% "Life-history group"), 
                      group_cols = group_cols_3)
dev.off()


####Fig S16: model comparisons############
Model_res <- read.csv("Dataset/Model robustness results for fig.csv")%>%
  mutate(
    Field = factor(Field, levels = c("Duolun", "CDR B", "CDR C")),
    Group = factor(Group, levels = c("Whole", "C3 grasses", "C4 grasses", "Forbs")),
    Response = factor(Response, levels = rev(c("Scom", "Spop", "Asyn", "CPE²", "SAE"))),
    Model = factor(Model, levels = c("Linear", "Quadratic", "Treatment")))

pd <- position_dodge(width = 0.6)

tiff("SI_Figs/Fig S16.tiff", width = 10, height = 16, units = "in", res = 300)
ggplot(Model_res, aes(x = Estimate, y = Response, 
                     colour = Model, group = Model)) +
  #### Zero reference
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey55", linewidth = 0.6) +
  #### 95% confidence intervals
  geom_errorbar(
    aes(xmin = CI_low, xmax = CI_high), orientation = "y",
    position = pd, width = 0.18, linewidth = 0.7) +
  #### Effect estimates
  geom_point(aes(shape = Model), position = pd, size = 2.8) +
  #### Facets
  facet_grid(Group ~ Field) +
  #### Model colours
  scale_colour_manual(
    values = c("Linear" = "#252525",
               "Quadratic" = "#62849A",
               "Treatment" = "#C29165")) +
  scale_shape_manual(
    values = c("Linear" = 16,
               "Quadratic" = 17,
               "Treatment" = 15)) +
  #### X axis
  scale_x_continuous(
    labels = scales::label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.06, 0.08))) +
  labs(x = "Estimated difference (highest N − control)",
       y = NULL, colour = NULL, shape = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "grey65", linewidth = 0.5),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y.right = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(size = 11, colour = "black"),
        axis.text.y = element_text(size = 12, face = "bold", colour = "black"),
        axis.title.x = element_text(size = 13, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        panel.spacing = grid::unit(0.7, "lines"))
dev.off()

####Figs S17-19: mean community biomass and temporal variance in Duolun, CDR B and CDR C#########
All_dat <- bind_rows(
  read.csv("Dataset/Duolun_functional_group_stability.csv"),
  read.csv("Dataset/e001_functional_group_stability.csv")) %>%
  filter(Method == "Raw",
         Group == "All")

col1 <- c("#F0E2DD", "#DDBEB3", "#C58F82", "#AF665B", "#983451")
col2 <- c("#F3E8D2", "#EED6AE", "#E8C58F", "#E4B77A", "#D6A45E", "#C18D4A", "#A8753C", "#8E6338")
col3 <- c("#DCE3ED", "#C4D0E1", "#A9B9D0", "#8EA3C0", "#738BAC", "#58688F", "#485775", "#39475F")

make_cv_plot <- function(field_name, palette) {
  
  #### Extract data
  dat <- All_dat %>%
    filter(
      Field == field_name,
      is.finite(B.sum),
      is.finite(Vij.sum),
      is.finite(Nitrogen),
      B.sum > 0,
      Vij.sum > 0)
  
  #### Order treatments by nitrogen addition
  trt_info <- dat %>%
    distinct(NTrt, Nitrogen) %>%
    arrange(Nitrogen)
  
  if (nrow(trt_info) != length(palette)) {
    stop(
      "Number of treatment colors does not match: ",
      field_name
    )
  }
  
  dat <- dat %>%
    mutate(NTrt = factor(NTrt, levels = trt_info$NTrt))
  
  #### Match colors to treatments
  color_map <- setNames(palette, trt_info$NTrt)
  
  #### Legend labels
  legend_labels <- if (field_name == "Duolun") {
    paste0("N", trt_info$Nitrogen)
  } else {
    as.character(trt_info$Nitrogen)
  }
  
  #### N0 reference point
  base_N0 <- dat %>%
    filter(Nitrogen == 0) %>%
    summarise(mu0 = mean(B.sum), var0 = mean(Vij.sum))
  
  mu0 <- base_N0$mu0
  var0 <- base_N0$var0
  
  if (
    length(mu0) != 1 ||
    !is.finite(mu0) || mu0 <= 0 ||
    !is.finite(var0) || var0 <= 0
  ) {
    stop("Invalid N0 reference point: ", field_name)
  }
  
  #### Constant-CV reference line, slope = 2 on log-log axes
  ref_dat <- tibble(
    B.sum = seq(
      min(dat$B.sum),
      max(dat$B.sum),
      length.out = 200)) %>%
    mutate(Vij.sum = var0 * (B.sum / mu0)^2)
  
  #### Plot
  p <- ggplot(dat, aes(x = B.sum, y = Vij.sum)) +
    
    #### Observed log-log regression with 95% CI
    geom_smooth(
      aes(group = 1),
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      color = "black",
      fill = "grey70",
      alpha = 0.25,
      linewidth = 1.2
    ) +
    
    #### N0-anchored constant-CV reference line
    geom_line(
      data = ref_dat,
      aes(x = B.sum, y = Vij.sum),
      inherit.aes = FALSE,
      color = "grey45",
      linetype = "dashed",
      linewidth = 1
    ) +
    
    #### Individual plots
    geom_point(
      aes(color = NTrt),
      size = 5
    ) +
    
    #### Log-log axes
    scale_x_log10() +
    scale_y_log10() +
    
    #### Custom colors
    scale_color_manual(
      values = color_map,
      breaks = trt_info$NTrt,
      labels = legend_labels,
      drop = FALSE
    ) +
    
    labs(
      x = expression("Mean total biomass (" * mu[tot] * ")"),
      y = expression("Community temporal variance (" * sigma[tot]^2 * ")"),
      color = "Treatment"
    ) +
    
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_text(size = 20, color = "black"),
      axis.title.y = element_text(size = 20, color = "black"),
      axis.text.x = element_text(size = 17),
      axis.text.y = element_text(size = 17),
      legend.position = "bottom"
    )
  
  return(p)
}

tiff("SI_Figs/Fig S17.tiff", width = 6, height = 6, units = "in", res = 300)
make_cv_plot("Duolun", col1)
dev.off()

tiff("SI_Figs/Fig S18.tiff", width = 6, height = 6, units = "in", res = 300)
make_cv_plot("CDR B", col2)
dev.off()

tiff("SI_Figs/Fig S19.tiff", width = 6, height = 6, units = "in", res = 300)
make_cv_plot("CDR C", col3)
dev.off()



####Figs S22-27: residual and QQ###########
duolun_stability <- read.csv("Dataset/Duolun_functional_group_stability.csv")%>%
  filter(Method %in% "Raw")
e001_stability <- read.csv("Dataset/e001_functional_group_stability.csv")%>%
  filter(Method %in% "Raw")

## Three independent experiments
dat_list <- list(
  Duolun = duolun_stability,
  `CDR B` = e001_stability %>%
    filter(Field=="CDR B"),
  `CDR C` = e001_stability %>%
    filter(Field=="CDR C"))

responses <- c("Scom", "Spop", "Asyn", "CPE", "SAE")
groups <- c("All", "C3 grass", "C4 grass", "Forb")

## Fit nitrogen response models
fit_nitrogen_model <- function(dat, group, response) {
  
  dat %>%
    filter(
      Group == group,
      is.finite(.data[[response]]),
      is.finite(Nitrogen_log)
    ) %>%
    lm(
      reformulate("Nitrogen_log", response = response),
      data = .
    )
}

## Extract model diagnostics
get_diagnostic_data <- function(dat) {
  
  map_dfr(groups, function(g) {
    
    map_dfr(responses, function(r) {
      
      model <- fit_nitrogen_model(dat, g, r)
      
      augment(model) %>%
        transmute(
          Group = g,
          Response = r,
          Fitted = .fitted,
          Std_resid = .std.resid
        )
    })
  }) %>%
    mutate(
      Group = factor(
        Group,
        levels = groups,
        labels = c(
          "Whole", "C3 grass", "C4 grass", "Forb"
        )
      ),
      Response = factor(
        Response,
        levels = responses,
        labels = c(
          "Scom", "Spop", "Asyn", "CPE^2", "SAE"
        )
      )
    )
}

## Extract once for both diagnostic plots
diagnostic_list <- lapply(dat_list, get_diagnostic_data)

##  Common diagnostic plot style
diagnostic_theme <- function() {
  
  theme_bw() +
    theme(
      panel.grid = element_blank(),
      
      #### Column headings
      strip.background = element_blank(),
      strip.text.x = element_text(
        size = 12,
        face = "bold"
      ),
      
      #### Row headings
      strip.text.y.left = element_text(
        size = 11,
        face = "bold",
        angle = 0
      ),
      strip.placement = "outside",
      
      #### Shared axis titles
      axis.title.x = element_text(
        size = 13,
        color = "black"
      ),
      axis.title.y = element_text(
        size = 13,
        color = "black"
      ),
      
      #### Individual axis tick labels
      axis.text.x = element_text(
        size = 9,
        color = "black"
      ),
      axis.text.y = element_text(
        size = 9,
        color = "black"
      ),
      
      #### Spacing
      panel.spacing = grid::unit(0.6, "lines"),
      
      #### Experiment title
      plot.title = element_text(
        size = 15,
        face = "bold",
        hjust = 0.5
      )
    )
}

## Common facet layout
diagnostic_facet <- function() {
  
  ggh4x::facet_grid2(
    rows = vars(Group),
    cols = vars(Response),
    
    #### Independent scales for each panel
    scales = "free",
    independent = "all",
    axes = "all",
    
    #### Group labels on the left
    switch = "y",
    
    #### Display CPE squared
    labeller = labeller(
      Response = label_parsed
    )
  )
}

## Residual versus fitted plots
make_residual_panel <- function(experiment) {
  
  dat <- diagnostic_list[[experiment]]
  
  ggplot(dat, aes(x = Fitted, y = Std_resid)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.5) +
    geom_point(size = 1.8, alpha = 0.7) +
    diagnostic_facet() +
    labs(
      x = "Fitted values",
      y = "Standardized residuals") +
    diagnostic_theme()
}

## Normal Q-Q plots
make_QQ_panel <- function(experiment) {
  
  dat <- diagnostic_list[[experiment]]
  
  ggplot(dat, aes(sample = Std_resid)) +
    stat_qq(size = 1.8) +
    stat_qq_line(
      linetype = "dashed",
      linewidth = 0.5) +
    diagnostic_facet() +
    labs(
      x = "Theoretical quantiles",
      y = "Standardized residual quantiles") +
    diagnostic_theme()
}

### Residual plots
Duolun_residual <- make_residual_panel("Duolun")
CDRB_residual <- make_residual_panel("CDR B")
CDRC_residual <- make_residual_panel("CDR C")

ggsave("SI_Figs/Fig S22.tiff", Duolun_residual, width = 12, height = 11, dpi = 300, compression = "lzw")
ggsave("SI_Figs/Fig S23.tiff", CDRB_residual, width = 12, height = 11, dpi = 300, compression = "lzw")
ggsave("SI_Figs/Fig S24.tiff", CDRC_residual, width = 12, height = 11, dpi = 300, compression = "lzw")

### Normal Q-Q plots
Duolun_QQ <- make_QQ_panel("Duolun")
CDRB_QQ <- make_QQ_panel("CDR B")
CDRC_QQ <- make_QQ_panel("CDR C")

ggsave("SI_Figs/Fig S25.tiff", Duolun_QQ, width = 12, height = 11, dpi = 300, compression = "lzw")
ggsave("SI_Figs/Fig S26.tiff", CDRB_QQ, width = 12, height = 11, dpi = 300, compression = "lzw")
ggsave("SI_Figs/Fig S27.tiff", CDRC_QQ, width = 12, height = 11, dpi = 300, compression = "lzw")
