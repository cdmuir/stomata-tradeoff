source("r/header.R")

df <- read_rds("objects/spatially-explicit-results.rds")

# Figure 3: Effect of f_s and gmax on p_colonize ----

df1 <- df %>%
  filter(D %in% c(100, 500)) %>%
  mutate(H = glue("H = {H}", H = H), D = factor(round(D), levels = c("100", "500")))

f3a <- ggplot(df1, aes(x = f_s, y = p_colonize, linetype = D)) +
  facet_grid(H ~ .) +
  geom_line(size = 1.1, lineend = "round") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab(expression(paste("Stomatal cover (", italic(f)[s], ")"))) +
  ylab(expression(paste("Probability of colonization (", italic(p)[colonize], ")"))) +
  scale_linetype_manual(
    values = c(2, 1), 
    name = expression(paste("Stomatal density [m", m^-2, "]"))
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(margin = margin(0.3, 0, 0, 0, unit = "cm")),
    legend.key.size = unit(0.75, "in"),
    legend.position = "top",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10)
  )

l <- get_legend(f3a)

f3a <- f3a + theme(legend.position = "none")

df2 <- df1 %>%
  filter(g_smax < 5)

f3b <- ggplot(df2, aes(x = g_smax, y = p_colonize, linetype = D)) +
  facet_wrap(H ~ ., strip.position = "left", ncol = 1) +
  geom_line(size = 1.1, lineend = "round") +
  xlab(expression(paste("Stomatal conductance (", italic(g)[list(s,max)], ") [mol ", m^-2~s^-1, "]"))) +
  ylab(expression(paste("Probability of colonization (", italic(p)[colonize], ")"))) +
  scale_linetype_manual(values = c(2, 1)) +
  scale_y_continuous(position = "right") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10)
  )

f3 <- plot_grid(f3a, f3b, labels = "auto", align = "hv", axis = "lr")

plot_grid(l, f3, ncol = 1, rel_heights = c(0.1, 0.9))
# ggsave("figures/fig3.jpeg", width = 6.5, height = 6.5, units = "in")
ggsave("figures/fig3.pdf", width = 6.5, height = 6.5, units = "in")


# Figure S1: Effect of D and S on p_colonize ----

## Effect of S on p_colonize ----

df1 <- df %>%
  filter(D %in% c(10, 100, 1000)) %>%
  mutate(H = glue("H = {H}", H = H), D = factor(round(D), levels = c("10", "100", "1000")))

df1_label <- df1 %>%
  group_by(H, D) %>%
  summarize(S = max(S), p_colonize = max(p_colonize)) %>%
  mutate(label = glue("italic(D)=={D}", D = D))

fS1a <- ggplot(df1, aes(x = S, y = p_colonize, linetype = D)) +
  facet_grid(H ~ .) +
  geom_line(size = 1.1, lineend = "round") +
  geom_text(
    data = df1_label, 
    mapping = aes(label = label), 
    vjust = 0, hjust = 1, parse = TRUE
  ) +
  xlab(expression(paste("Stomatal size [", mu, m^2, "]"))) +
  ylab(expression(paste("Probability of colonization (", italic(p)[colonize], ")"))) +
  scale_x_log10() +
  scale_y_continuous(limits = c(0, 0.7)) +
  scale_linetype_manual(values = c(5, 2, 1)) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(margin = margin(0.3, 0, 0, 0, unit = "cm")),
    legend.key.size = unit(0.75, "in"),
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10)
  )

## Effect of D on p_colonize ----

df2 <- df %>%
  filter(S %in% c(10, 100, 1000)) %>%
  mutate(H = glue("H = {H}", H = H), S = factor(round(S), levels = c("10", "100", "1000")))

df2_label <- df2 %>%
  group_by(H, S) %>%
  summarize(D = max(D), p_colonize = max(p_colonize)) %>%
  mutate(label = glue("italic(S)=={S}", S = S))

fS1b <- ggplot(df2, aes(x = D, y = p_colonize, linetype = S)) +
  facet_wrap(H ~ ., strip.position = "left", ncol = 1) +
  geom_line(size = 1.1, lineend = "round") +
  geom_text(
    data = df2_label, 
    mapping = aes(label = label), 
    vjust = 0, hjust = 1, parse = TRUE
  ) +
  xlab(expression(paste("Stomatal density [m", m^-2, "]"))) +
  ylab(expression(paste("Probability of colonization (", italic(p)[colonize], ")"))) +
  scale_linetype_manual(values = c(5, 2, 1)) +
  scale_x_log10() +
  scale_y_continuous(limits = c(0, 0.7), position = "right") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10)
  )

plot_grid(fS1a, fS1b, labels = "auto", align = "hv", axis = "lr")

# ggsave("figures/figS1.jpeg", width = 6.5, height = 5.5, units = "in")
ggsave("figures/figS1.pdf", width = 6.5, height = 5.5, units = "in")
