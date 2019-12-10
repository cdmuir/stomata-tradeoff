source("r/header.R")

# Figure 1 ----

# Panel A: Equilateral triangular grid ----
s1 <- 1      # interstomatal distance
n <- 2
s2 <- n * s1 # side length of hexagonal grid
lim <- c(-s2, s2)

df_stomata <- data.frame(
  from = -s1 * c(seq(n / 2, n, 1 / 2), seq(n - 1/2, n / 2, -1 / 2)),
  to = s1 * c(seq(n / 2, n, 1 / 2), seq(n - 1/2, n / 2, -1 / 2)),
  length.out = c((n + 1):(2 * n + 1), (2 * n):(n + 1)),
  n = sqrt(3) / 2 * seq(-n * s1, n * s1, s1)
) %>%
  pmap_dfr(~ {data.frame(
      x = seq(..1, ..2, length.out = ..3),
      y = rep(..4, ..3)
  )}) %>%
  mutate(image = "figures/open-stomate.png")

xy <- combn(nrow(df_stomata), 2)
df_grid <- bind_cols(
  df_stomata[xy[1,], ],
  df_stomata[xy[2,], ]
) %>%
  mutate(d = sqrt((x1 - x) ^ 2 + (y1 - y) ^ 2)) %>%
  filter(abs(d - s1) < 1e-6)
  
pA <- ggplot(df_stomata, mapping = aes(x, y)) +
  geom_segment(
    data = df_grid,
    mapping = aes(xend = x1, yend = y1),
    color = "grey", size = 2,
  ) +
  geom_image(
    mapping = aes(image = image),
    size = 0.1
  ) +
  labs(title = "Stomata in grid of\nequilateral triangles") +
  xlim(1.1 * lim) +
  ylim(1.1 * lim) +
  coord_equal() +
  theme_void()

pA

# Panel B: Focal Equilateral Triangle ----

df_stomata %<>% 
  filter(x >= -s1 / 2, x <= s1 / 2, y <= 0, y >= -sqrt(3) / 2 * s1)
xlim <- range(df_stomata$x) + c(-1, 1) * 0.15
ylim <- range(df_stomata$y) + c(-1, 1) * 0.15

pB <- ggplot(df_stomata, mapping = aes(x, y)) +
  geom_polygon(color = "grey", fill = NA, size = 2) +
  geom_image(mapping = aes(image = image), size = 0.2) +
  scale_x_continuous(limits = xlim) +
  scale_y_continuous(limits = ylim) +
  labs(title = "Focal triangle") +
  coord_equal() +
  theme_void()

pB

# Panel C: Focal Equilateral Triangle ----

df_stomata %<>%
  select(x1 = x, y1 = y) %>%
  mutate(x2 = x1[c(2:nrow(.), 1)], y2 = y1[c(2:nrow(.), 1)]) %>%
  mutate(x = x1[c(nrow(.), 1:(nrow(.) - 1))], 
         y = y1[c(nrow(.), 1:(nrow(.) - 1))]) %>%
  mutate(xend = (x1 + x2) / 2, yend = (y1 + y2) / 2)

df_poly <- df_stomata %>%
  crossing(., summarise(., x_center = mean(x1), y_center = mean(y1))) %>%
  transmute(
    x_stomata = x1, 
    y_stomata = y1, 
    x_center, 
    y_center, 
    x_mid = xend,
    y_mid = yend 
  ) %>%
  filter(x_stomata <= x_center, y_stomata <= y_center) %>%
  pivot_longer(matches("^x|y_[a-z]+$"), names_to = c("axis", "which"), names_sep = "_") %>%
  pivot_wider(values_from = value, names_from = axis)

df_text <- df_poly %>%
  summarize_if(is.numeric, mean) %>%
  mutate(label = "Focal\nArea")

pC <- delete_layers(pB, match_type = "GeomImage") +
  geom_polygon(
    data = df_poly, fill = "grey"
  ) +
  geom_segment(
    data = df_stomata,
    mapping = aes(x = x, y = y, xend = xend, yend = yend),
    inherit.aes = FALSE,
    color = "grey", linetype = "dashed", lineend = "round", size = 1
  ) +
  geom_text(
    data = df_text,
    mapping = aes(label= label), 
    parse = FALSE, size = 4, lineheight = 0.75, angle = 30
  ) +
  geom_image(mapping = aes(image = image), size = 0.2) +
  labs(title = "Focal triangle has\nsix equivalent areas")

pC

# Panel D: ----

df_segment <- df_text %>% 
  mutate(
    l = s1 / 10,
    xend = x + sqrt(3) / 4 * l,
    yend = y + l / 4,
    x = x - sqrt(3) / 4 * l,
    y = y - l / 4
  )

R <- 0.125
U <- s1

x_pathogen <- df_text$x
y_pathogen <- df_text$y

X_stomata <- df_stomata$x
Y_stomata <- df_stomata$y

# m <- get_tangent_slopes(R, X_stomata, Y_stomata, x_pathogen, y_pathogen)

df_tangent_points <- 
  get_tangent_points(R, X_stomata, Y_stomata, x_pathogen, y_pathogen) %>%
  mutate(stomate = rep(LETTERS[1:3], 2))

df_tangent_lines <- tibble(
  x = rep(x_pathogen, 3),
  y = rep(y_pathogen, 3),
  stomate = LETTERS[1:3]
) %>% bind_rows(., df_tangent_points, .)

pD <- pC %>%
  delete_layers(match_type = "GeomSegment") %>%
  delete_layers(match_type = "GeomText") %>%
  delete_layers(idx = 2) +
  geom_polygon(
    data = df_tangent_lines, 
    mapping = aes(group = stomate),
    alpha = 0.5
  ) +
  # geom_point(data = df_tangent_points, size = 3, pch = 21, fill = "white") +
  geom_segment(
    data = df_segment,
    mapping = aes(xend = xend, yend = yend),
    size = 4, lineend = "round", color = "grey25"
  ) +
  geom_text(
    data = df_segment,
    mapping = aes(x = xend, y = yend, label = "pathogen"),
    hjust = 0, vjust = 0, nudge_x = 0.05 * s1, nudge_y = 0.05 * s1
  ) +
  labs(title = "Pathogen locates a stomate\nat the correct angle")

pD %<>% move_layers(match_type = "GeomImage", position = "top")

pD

# Combine Panels ----

f <- plot_grid(
  pA + theme(plot.margin = unit(c(0, 0, 0, 0), "in"), plot.title = element_text(hjust = 0.5, vjust = 0.5)), 
  pB + theme(plot.margin = unit(c(0, 0, 0, 0), "in"), plot.title = element_text(hjust = 0.5, vjust = 0.5)), 
  pC + theme(plot.margin = unit(c(0, 0, 0, 0), "in"), plot.title = element_text(hjust = 0.5, vjust = 0.5)), 
  pD + theme(plot.margin = unit(c(0, 0, 0, 0), "in"), plot.title = element_text(hjust = 0.5, vjust = 0.5)), 
  ncol = 2, labels = "auto"
  )
f

ggsave("figures/fig1.jpeg", width = 6.5, height = 6.5, units = "in")

# Figure 2 ----

df_stomata %<>% mutate(
  position = c("$(U/2, \\sqrt{3} / 2 U)$", "$(0, 0)$", "$(U, 0)$"),
  label = c("", "Reference\nstomate", "")
)

fig2a <- pD %>%
  delete_layers("GeomImage") %>% 
  delete_layers("GeomText") + 
  theme(plot.title = element_blank()) +
  geom_text(
    data = df_segment,
    mapping = aes(x = xend, y = yend, label = "$(x_p, y_p)$"),
    hjust = 0, vjust = 0, nudge_x = 0.05 * s1, nudge_y = 0.05 * s1
  ) +
  # geom_point(
  #   data = df_tangent_points, 
  #   size = 3, pch = 21, fill = "white"
  # ) +
  geom_text(
    data = df_stomata,
    mapping = aes(label = position),
    hjust = 0.5, vjust = 1, nudge_x = 0, nudge_y = 1.75 * R
  ) +
  geom_text(
    data = df_stomata,
    mapping = aes(label = label),
    hjust = 0.5, vjust = 0, nudge_x = 0, nudge_y = -2.75 * R
  ) +
  scale_x_continuous(expand = expand_scale(mult = 0.125)) +
  scale_y_continuous(expand = expand_scale(mult = 0.125)) +
  geom_circle(
    data = df_stomata,
    mapping = aes(x0 = x, y0 = y, r = R),
    inherit.aes = FALSE, fill = "white", size = 2
  ) +
  geom_spoke(
    data = df_stomata,
    mapping = aes(angle = 3 * pi / 2, radius = R),
    size = 2
  ) +
  geom_spoke(
    data = df_stomata,
    mapping = aes(angle = pi / 2, radius = R),
    size = 2
  ) +
  geom_circle(
    data = df_stomata,
    mapping = aes(x0 = x, y0 = y, r = R / 2),
    inherit.aes = FALSE, fill = "white", size = 2
  )

df_segment <- df_stomata %>%
  filter(label == "Reference\nstomate") %>%
  select(x, y) %>%
  mutate(x1 = x + s1 / 2, x2 = x1, y1 = y, y2 = y + sqrt(3) * s1 / 6) %>%
  pivot_longer(matches("^x[1-2]{1}$"), values_to = "xend") %>%
  select(-name) %>%
  pivot_longer(matches("^y[1-2]{1}$"), values_to = "yend") %>%
  select(-name)

df_ribbon <- tibble(
  x = seq(sqrt(3) / 2 * R + 1e-8, s1 / 2, length.out = 1e2),
  ymin = get_ymin(x, R, s1),
  ymax = get_ymax(x, R, s1)
) %>%
  mutate_at(vars(starts_with("x")), ~ {.x + min(df_segment$x)}) %>%
  mutate_at(vars(starts_with("y")), ~ {.x + min(df_segment$y)}) 

df_bounds <- df_ribbon %>%
  pivot_longer(starts_with("y"), names_to = "Bounds", values_to = "y") %>%
  mutate(Bounds = str_replace(Bounds, "^y([a-z]{3})$", "$y_\\\\mathrm{\\1}$"))

df_points <- tibble(
  x = c(0, R, s1 / 2, sqrt(3) / 2 * R, s1 / 2),
  y = c(0, 0, 0     , R / 2      , sqrt(3) / 6 * s1),
  label = glue(
    "$({x},{y})$", 
    x = c("0", "R", "\\frac{U}{2}", "\\frac{\\sqrt{3}}{2} R", "\\frac{U}{2}"), 
    y = c("0", "0", "0", "\\frac{R}{2}", "\\frac{\\sqrt{3}}{6} U")
  ),
  nudge_y = c(rep(-s1, 3) * c(0.05, 0.1, 0.05), rep(s1, 2) * 0.1),
  y_label = y + nudge_y,
  y_seg = y_label - sign(nudge_y) * 0.025 * s1
) %>%
  mutate_at(vars(starts_with("x")), ~ {.x + min(df_segment$x)}) %>%
  mutate_at(vars(starts_with("y")), ~ {.x + min(df_segment$y)}) 

fig2b <- fig2a %>%
  delete_layers("GeomCircle") %>% 
  delete_layers("GeomSpoke") %>% 
  delete_layers("GeomSegment") %>% 
  delete_layers("GeomPoint") %>% 
  delete_layers("GeomPolygon") %>% 
  delete_layers("GeomText") + 
  geom_circle(
    data = df_stomata,
    mapping = aes(x0 = x, y0 = y, r = R),
    inherit.aes = FALSE, fill = "white", size = 2, color = "grey"
  ) +
  scale_x_continuous(
    limits = c(min(df_segment$x) - R, max(df_segment$xend) + R)
  ) +
  scale_y_continuous(
    limits = c(min(df_segment$y) - R, max(df_segment$yend) + R)
  ) +
  geom_point(data = df_stomata, aes(x = xend, y = yend)) +
  geom_segment(
    data = df_segment, 
    aes(x = x, y = y, xend = xend, yend = yend), color = "grey"
  ) +
  geom_ribbon(
    data = df_ribbon, 
    mapping = aes(x = x, ymin = ymin, ymax = ymax), 
    inherit.aes = FALSE, fill = "grey"
  ) +
  geom_line(
    data = df_bounds, 
    mapping = aes(linetype = Bounds),
    size = 2,
  ) +
  geom_segment(
    data = df_points, 
    mapping = aes(xend = x, yend = y_seg),
    size = 1, color = "grey"
  ) + 
  geom_point(
    data = df_points, shape = 21, fill = "white", size = 4, stroke = 2
  ) + 
  geom_text(
    data = df_points,
    mapping = aes(y = y_label, label = label)
  ) + 
  theme(legend.position = "bottom")

fig2b

fig2 <- plot_grid(fig2a, fig2b, labels = "auto")

# Save for separate quickly checking dimensions
ggsave("figures/fig2.pdf", width = 6.5, height = 3.25, units = "in")

tikz(file = "figures/fig2.tex", width = 6.5, height = 3.25)
print(fig2)  
dev.off()
