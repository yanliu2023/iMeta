
# === Clear environment ===
rm(list = ls())

# === Load packages ===
library(ggplot2)
library(gghalves)
library(patchwork)

# === Load data ===
df <- read.csv("8-HR.csv")

# === Preprocess data ===
# Treat Richness as factor for x-axis label control (e.g., 1,2,4,8), and as numeric for regression (1:4)
df$Richness <- as.factor(df$Richness)
df$Richness_num <- as.numeric(factor(df$Richness, levels = c("1", "2", "4", "8")))

# === Define colors ===
group_colors <- c(
  "1" = "#CFF1DF",
  "2" = "#A7E1CB",
  "4" = "#4DB89D",
  "8" = "#287C6F"
)

# === GLM fit and prediction ===
fit_glm_and_predict <- function(df, yvar) {
  df$y <- df[[yvar]]
  glm_model <- glm(y ~ Richness_num, data = df, family = gaussian())
  newdata <- data.frame(Richness_num = seq(1, 4, length.out = 100))
  pred <- predict(glm_model, newdata = newdata, se.fit = TRUE)
  newdata$fit <- pred$fit
  newdata$se <- pred$se.fit
  newdata$lower <- newdata$fit - 1.96 * newdata$se
  newdata$upper <- newdata$fit + 1.96 * newdata$se
  stats <- summary(glm_model)
  r2 <- round(1 - stats$deviance / stats$null.deviance, 4)
  p <- signif(coef(stats)[2, 4], 4)
  list(data = newdata, r2 = r2, p = p)
}

# === Plotting function ===
plot_one <- function(df, y_var, y_label, y_range = NULL) {
  df$y <- df[[y_var]]
  reg <- fit_glm_and_predict(df, y_var)

  ggplot(df, aes(x = Richness, y = y, fill = Richness)) +
    gghalves::geom_half_violin(side = "r", trim = FALSE, width = 1,
                                color = "black", linewidth = 0.3, alpha = 0.6) +
    geom_boxplot(aes(group = Richness), width = 0.1, outlier.shape = NA,
                 fill = "white", color = "black", linewidth = 0.4) +
    gghalves::geom_half_point(aes(x = Richness),
                              color = "black", size = 1.6, alpha = 0.6,
                              position = position_nudge(x = -0.1), side = "l") +
    geom_ribbon(data = reg$data, aes(x = Richness_num, ymin = lower, ymax = upper),
                inherit.aes = FALSE, fill = "#CFF1DF", alpha = 0.4) +
    geom_line(data = reg$data, aes(x = Richness_num, y = fit),
              inherit.aes = FALSE, color = "black", linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = Inf, y = Inf,
             label = paste0("R² = ", reg$r2, ", p = ", reg$p),
             hjust = 1.1, vjust = 2.5, size = 6, color = "black") +
    scale_fill_manual(values = group_colors) +
    theme_bw(base_size = 14) +
    theme(
      axis.title = element_text(size = 18, face = "plain"),
      axis.text = element_text(size = 16),
      axis.ticks = element_line(linewidth = 1),
      axis.ticks.length = unit(0.25, "cm"),
      panel.border = element_rect(color = "black", linewidth = 1, fill = NA),
      panel.grid = element_blank()
    ) +
    labs(x = "Bacillus consortia richness", y = y_label) +
    coord_cartesian(ylim = y_range)
}

# === Generate subplots ===
p1 <- plot_one(df, "Abundance",
               expression(italic(Bacillus)~abundance~(log[10]~g^{-1}~roots)),
               y_range = c(5, 9))

p2 <- plot_one(df, "IAA", "IAA production (ng/mL)", y_range = c(0, 30))

p3 <- plot_one(df, "Siderophore", "Siderophore production", y_range = c(0, 1.5))

# === Combine plots ===
final_plot <- (p1 + p2 + p3) +
  plot_layout(nrow = 1, guides = "collect", widths = c(1, 1, 1)) &
  theme(legend.position = "none") &
  plot_annotation(title = "Highly related consortia",
                  theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))

# === Save plot ===
ggsave("8-HR.pdf", plot = final_plot, width = 18, height = 6)
