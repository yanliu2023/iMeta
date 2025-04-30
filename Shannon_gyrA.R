# === Load required packages ===
library(ggplot2)
library(dplyr)
library(ggpubr)

# === Import data ===
df <- read.csv("Shannon_gyrA.csv")

# Preserve the order of factor levels
df$richness <- factor(df$richness, levels = unique(df$richness))


# Color palette option 2: 99CFED tones
my_colors <- c("#ffffff", "#e5f3fa", "#cce7f6", "#b2dbf1", "#99cfed")

# === Plot boxplot without significance annotations ===
p <- ggplot(df, aes(x = richness, y = Shannon)) +
  geom_boxplot(aes(fill = richness), width = 1, outlier.shape = NA, color = "black") +
  geom_jitter(size = 3, alpha = 0.7, color = "black", width = 0.3) +
  scale_fill_manual(values = my_colors) +
  scale_color_manual(values = my_colors) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20, face = "plain"),
    axis.text = element_text(size = 18),
    panel.border = element_rect(color = "black", linewidth = 1.2, fill = NA),
    axis.ticks = element_line(linewidth = 1.2),
    axis.ticks.length = unit(0.3, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(y = "Shannon diversity") +
  ylim(4, 7)

# === Save figure as PDF ===
ggsave("Shannon_gyrA.pdf", plot = p, device = "pdf", width = 6, height = 5, units = "in")
