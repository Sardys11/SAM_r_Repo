

# Module 9 - Module # 9 Visualization in R
# Student: Sardys Avile-Martinez


# ###NOTES FROM THE PROFESSOR:
# Module # 9:
# In this module, we reviewed three types of visualization in R: basic visualization without any package, lettuce and ggplot2.
# Choose any data set for your visualization from Vincent Arel Bundock dataset list: https://vincentarelbundock.github.io/Rdatasets/datasets.htmlLinks to an external site.
# Using this data, generate three types of visualization on the data set you have chosen. In your blog, discuss and present your three visualizations you will create and express your opinion on these three different types of graphics output.
# 
# Yours
# Dr. Friedman


# Using the suggested link to get the data set. I have chosen the quartets of 'Anscombe's (1973) Quartets' because this dataset is easy to work
# and it is perfect for scatterplots and fun to stylize.

# Data Load
quartets <- read.csv("Data/quartets.csv")

# 1. Base R - Scatterplots with regression lines no extra packages...just basic
# Here showing off the quartets as quartets.

unique_groups <- unique(quartets$group)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

for (grp in unique_groups) {
  data_grp <- subset(quartets, group == grp)
  plot(data_grp$x, data_grp$y, main = grp,
       xlab = "X", ylab = "Y", pch = 19, col = "blue")
  abline(lm(y ~ x, data = data_grp), col = "red", lwd = 2)
}


# 2. Lattice (With a 3D Flair!)
# Now let's make it interesting. The professor suggested to use it a 3D visualization.
# However, this data is two-dimensional, but let's see if this lattice will do the job for us with cloud() plots.

library(lattice)

# Convert set to numeric for z-axis
quartets$group_id <- as.numeric(as.factor(quartets$group))  # Create z-axis group ID

cloud(y ~ x * group_id, data = quartets,
      screen = list(z = 30, x = -60),
      col = "purple", pch = 16,
      xlab = "X", ylab = "Y", zlab = "Quartet",
      main = "Anscombe's Quartet in 3D (Lattice)")


# 3. ggplot2 (Polished & Pretty)
# This script creates individual scatterplots for each dataset, complete with regression lines and distinct colors.

library(ggplot2)

ggplot(quartets, aes(x = x, y = y, color = group)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~group) +
  theme_minimal() +
  labs(title = "Anscombe's Quartet with ggplot2",
       x = "X", y = "Y")

# Beautiful!!!!

