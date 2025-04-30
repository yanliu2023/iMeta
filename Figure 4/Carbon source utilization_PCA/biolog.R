library(vegan)
library(ggpubr)

# Read the Biolog data table
x = read.table(file="biolog.txt", sep="\t", header=TRUE, row.names=1)

# Perform redundancy analysis (RDA), or optionally principal components analysis (PCA)
x.pca = rda(x)
x.pca
outputpca = summary(x.pca)

# Display the structure of the PCA result list
str(outputpca)

# Extract species scores
a = outputpca$species ; a

# Extract explained variance (importance of components)
b = outputpca$cont$importance ; b

# Save species scores and explained variance to files
write.table(a, file="PCA_site.txt", sep="\t", col.names = NA)
write.table(b, file="PCA_evale.txt", sep="\t", col.names = NA)

# Save the full PCA summary output to a text file
sink("PCA.txt")
outputpca
sink()

# Manually assign groups for different treatments to enable grouping and coloring in plots
# The grouped file should be prepared in advance
y = read.table("PCA_site_group.txt", header = TRUE, row.names = 1)
head(y)

# Load plotting libraries
library(ggplot2)
library(ggpubr)

# Extract and round variance explained by PC1 and PC2
pc1 = round(b[2,1], 2); pc1
pc2 = round(b[2,2], 2); pc2

# Set up plot theme
plot.theme = theme(
  plot.title = element_text(size=20, color="black", family="serif", face="bold", vjust=0.5, hjust=0.5),
  axis.line = element_line(size=0.5, colour="black"),
  axis.ticks = element_line(color="black"),
  axis.text = element_text(size=20, color="black", family="serif", face="bold", vjust=0.5, hjust=0.5),
  axis.title = element_text(size=20, color="black", family="serif", face="bold", vjust=0.5, hjust=0.5),
  legend.position = "right",
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.text = element_text(colour='black', size=20, family="serif", face='bold'),
  legend.title = element_blank(),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

# Plot PCA result
ggplot(y, aes(PC1, PC2, colour = group1)) +
  geom_point(aes(shape = group2), size = 5) +
  scale_shape_manual(values = c(15, 16, 17, 3, 4, 18, 25, 7, 8, 9, 10, 11)) + # Customize point shapes
  scale_color_manual(values = c("#9932CC", "#CCFF99", "#BDB76B", "#CD5C5C", "#669933", 
                                "#996600", "#6495ED", "#A52A2A")) +
  geom_hline(yintercept = 0.08, linetype = 2, color = "grey", size =_
             