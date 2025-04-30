# === Load data ===
df <- read.csv("shoot dry weight.csv")

# === Load required packages ===
library(ggplot2)
library(dplyr)
library(ggpubr)
library(multcompView)

# === Data preprocessing ===
df$richness <- factor(df$richness, levels = unique(df$richness))

# === Define custom fill colors ===
fill_colors <- c(
  "CK" = "#E1E6ED",  # steel light
  "1"  = "#C2CAD9",  # cool gray
  "2"  = "#A0ACC4",  # slate blue
  "3"  = "#7A8FAE",  # denim
  "4"  = "#5E7298"   # navy steel
)

# === Perform ANOVA and Tukey HSD test ===
aov_res <- aov(shoot_dry_weight ~ richness, data = df)
tukey_res <- TukeyHSD(aov_res)

# Extract significance letters
tukey_letters <- multcompView::multcompLetters4(aov_res, tukey_res)$richness
letters_df <- data.frame(
  richness = names(tukey_letters$Letters),
  Letters = tukey_letters$Letters
)

# Automatically set position for significance letters (slightly above max values)
max_y_df <- df %>%
  group_by(richness) %>%
  summarize(max_y = max(shoot_dry_weight, na.rm = TRUE) + 0.3)
letters_df <- merge(letters_df, max_y_df, by = "richness")

# Automatically set y-axis upper limit to avoid overlap
y_max <- max(df$shoot_dry_weight, na.rm = TRUE) + 0.8

# Extract overall ANOVA p-value and format it
anova_p <- summary(aov_res)[[1]][["Pr(>F)"]][1]
formatted_p <- formatC(anova_p, format = "e", digits = 2)

# === Plot ===
p <- ggplot(df, aes(x = richness, y = shoot_dry_weight, fill = richness)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, color = "black") +
  geom_jitter(size = 3, alpha = 0.7, color = "black", width = 0.3) +
  geom_text(data = letters_df, aes(x = richness, y = max_y, label = Letters),
            inherit.aes = FALSE, size = 10, fontface = "bold", vjust = 0) +
  annotate("text", x = 4.6, y = 0.3,
           label = paste0("P == ", formatted_p),
           parse = TRUE, size = 8) +
  scale_fill_manual(values = fill_colors) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 26, face = "plain"),
    axis.text = element_text(size = 22),
    panel.border = element_rect(color = "black", linewidth = 1.2, fill = NA),
    axis.ticks = element_line(linewidth = 1.2),
    axis.ticks.length = unit(0.3, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(y = expression("Shoot dry weight (g " * plant^{-1} * ")")) +
  ylim(0, y_max)

# === Save as PDF file ===
ggsave("shoot dry weight.pdf", plot = p, device = "pdf", width = 8, height = 6, units = "in")
