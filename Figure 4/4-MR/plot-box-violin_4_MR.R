# === Clear the environment ===
rm(list = ls())

# === Load data ===
df <- read.csv("4-MR.csv")

# === Check column names ===
colnames(df)

# === Preprocess data ===
df$Richness <- as.character(df$Richness)       # Ensure Richness is character (for matching fill color)
df$Richness_num <- as.numeric(df$Richness)     # Numeric version for fitting

# === Define custom green color gradient (from light to dark) ===
group_colors <- c(
   "1" = "#D4F0F9",  
   "2" = "#A2D8F2",  
   "3" = "#5DB9E4",  
   "4" = "#247BA0"   
 )


# === Load required packages ===
library(ggplot2)
library(gghalves)
library(patchwork)

# === GLM fitting function ===
fit_glm_and_predict <- function(df, yvar) {
  df$y <- df[[yvar]]
  
  # Fit GLM
  glm_model <- glm(y ~ Richness_num, data = df, family = gaussian())
  
  # Generate prediction data
  newdata <- data.frame(Richness_num = seq(1, 4, length.out = 100))
  pred <- predict(glm_model, newdata = newdata, se.fit = TRUE)
  
  # Extract predictions
  newdata$fit <- pred$fit
  newdata$se <- pred$se.fit
  newdata$lower <- newdata$fit - 1.96 * newdata$se
  newdata$upper <- newdata$fit + 1.96 * newdata$se
  
  # Extract R² and p-value
  stats <- summary(glm_model)
  r2 <- round(1 - stats$deviance / stats$null.deviance, 3)
  p <- signif(coef(stats)[2, 4], 3)
  
  list(data = newdata, r2 = r2, p = p)
}

# === Custom plotting function ===
plot_one <- function(df, y_var, y_label, y_range = NULL) {
  y_expr <- substitute(y_var)
  df$y <- df[[y_var]]
  
  reg <- fit_glm_and_predict(df, y_var)
  
  ggplot(df, aes(x = Richness_num, y = y, fill = Richness)) +
    gghalves::geom_half_violin(
      aes(x = Richness_num),
      side = "r", trim = FALSE, width = 1,
      color = "black", linewidth = 0.3, alpha = 0.6
    ) +
    geom_boxplot(
      aes(x = Richness_num, group = Richness_num),
      width = 0.1, outlier.shape = NA, fill = "white",
      color = "black", linewidth = 0.4
    ) +
    gghalves::geom_half_point(
      aes(x = Richness_num),
      color = "black", size = 1.8, alpha = 0.6,
      position = position_nudge(x = -0.1), side = "l"
    ) +
    geom_ribbon(data = reg$data, aes(x = Richness_num, ymin = lower, ymax = upper),
                inherit.aes = FALSE, fill = "#D4F0F9", alpha = 0.4) +
    geom_line(data = reg$data, aes(x = Richness_num, y = fit),
              inherit.aes = FALSE, color = "black", linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = Inf, y = Inf,
             label = paste0("R² = ", reg$r2, ", P = ", reg$p),
             hjust = 1.1, vjust = 2.8, size = 8, color = "black") +
    scale_fill_manual(values = group_colors) +
    scale_color_manual(values = group_colors) +
    theme_bw(base_size = 12) +
    theme(
      axis.title = element_text(size = 24, face = "plain"),
      axis.text = element_text(size = 22),
      axis.ticks = element_line(linewidth = 1),
      axis.ticks.length = unit(0.25, "cm"),
      panel.border = element_rect(color = "black", linewidth = 1, fill = NA),
      panel.grid = element_blank()
    ) +
    labs(x = "Bacillus consortia richness", y = y_label) +
    coord_cartesian(ylim = y_range)
}

# === Generate three subplots ===
p1 <- plot_one(df, "Abundance",
               expression(Bacillus~abundance~(log[10]~g^{-1}~roots)),
               y_range = c(5, 8.5))

p2 <- plot_one(df, "IAA", "IAA production (ng/mL)", y_range = c(-2, 30))

p3 <- plot_one(df, "Siderophore", "Siderophore production", y_range = c(-0.2, 1.5))

# === Combine plots ===
final_plot <- (p1 + p2 + p3) / NULL +
  plot_layout(nrow = 1, guides = "collect", widths = c(1, 1, 1)) &
  theme(legend.position = "none")

# === Save final plot as square-proportioned PDF ===
ggsave("4-MR1.pdf", plot = final_plot, width = 18, height = 6)
