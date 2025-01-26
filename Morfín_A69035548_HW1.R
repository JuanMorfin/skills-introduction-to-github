##########3.1 Preliminaries#########
library(tidyverse)
library(readxl)
library(stargazer)
library(ggplot2)
#a. Load dataset and set all variables to numeric

setwd("~/Documents/GPS 2nd Quarter/QMII/tareas/tarea 1")
getwd()
library(dplyr)
data <- read.csv("HW1_wildfire_exposure.csv")
datanum <- data %>% mutate (across(everything(),~ as.numeric(as.character(.)) ))
str(datanum)

#b. Set dim() and evaluate
dim(data)

#c. Count NAs
sum(is.na(data))

#d. Replace NAs (in case there are blank spaces)
datanum[datanum == ""] <- NA
#Check again -> Result: No NAs
sum(is.na(datanum))

########3.2 Data and Descriptive Statistics#########
table(datanum$mindist5000)

#a. How many Census Block Groups had wildfires in 2010?
## first check if all counts unique
uniquebgs <- length(unique(datanum$bg))
## then count occurrences of wildfire1y (assumming this year is 2010, of the dataset subset we are working on)
wildfoccurrences<-datanum[datanum$wildfire1yr == 1, 0]
print(wildfoccurrences)

#b. Create new variable
datanum <- datanum %>% 
  mutate(mindistkm = mindist5000/1000)

#c. Compute summary statistics for dependent and independent variables
mean_envbi <- mean(datanum$envbi, na.rm = TRUE)
mean_mindistkm <- mean(datanum$mindistkm, na.rm = TRUE)
var_envbi <- var(datanum$envbi, na.rm = TRUE)
var_mindistkm <- var(datanum$mindistkm, na.rm = TRUE)
cov_envbi_mindistkm <- cov(datanum$envbi, datanum$mindistkm, use = "complete.obs")

print(mean_envbi)
print(mean_mindistkm)
print(var_envbi)
print(var_mindistkm)
print(cov_envbi_mindistkm)

########3.3 Visualizing the Data.############
##a. histogram and kernel density curve for envbi variable
library(tidyverse)
plot_envbi <- ggplot(datanum, aes(x = envbi)) +
  geom_histogram(
    aes(y = after_stat(count)), 
    bins = 10, 
    fill = "gold", 
    alpha = 0.5, 
    color = "black"
  ) +
  geom_density(
    aes(y = after_stat(density) * nrow(datanum) * (0.85 / 10)), 
    color = "red", 
    linewidth = 1,
  ) +
  labs(
    title = "Figure 1. Distribution of support for environmental policies,
    reflected in the Environmental Ballot Index",
    x = "Environmental Ballot Index",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )
print(plot_envbi)
ggsave("distribution_envbi.png", plot = plot_envbi, width = 8, height = 6)

#counting for proportions
sum(datanum$envbi>= 0.6)
sum(datanum$envbi<= 0.6)
proportionenvbihigh <- 12355/20990
mean(datanum$envbi)

sum(datanum$mindistkm <= 50)
sum(datanum$mindistkm > 50)
proportionmindistkm <- 11607/20990
mean(datanum$mindistkm)

#b. histogram and kernel density curve for mindistkm variable
d <- density(datanum$mindistkm)
density_df <- data.frame(x = d$x, y = d$y)
print(d$bw)
scaling_factor <- diff(range(datanum$mindistkm)) / diff(range(density_df$x))
plot_mindistkm <- ggplot(datanum, aes(x = mindistkm)) +
  geom_histogram(
    aes(y = after_stat(count)), 
    bins = 20, 
    fill = "grey", 
    alpha = 0.5, 
    color = "black"
  ) +
  geom_density(
    aes(y = ..density.. * nrow(datanum) * (diff(range(datanum$mindistkm)) / 20)),  # Scale to match histogram counts
    color = "red", 
    size = 1
  ) +
  labs(
    title = "Figure 2. Distribution of distances of Census Block Groups,
    to wildfires (kilometers)",
    x = "Distance to wildfires",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

print(plot_mindistkm)
ggsave("distribution_mindistkm.png", plot = plot_mindistkm, width = 8, height = 6)

#c. Create scatterplot for envbi and mindistkm
plot_scatter <- ggplot(datanum, aes(x = mindistkm, y = envbi)) +
  geom_point(shape = 19, color = "coral3", size = 0.75, alpha = 0.7) +
  labs(
    title = "Figure 3. Scatter Plot of Environmental Ballot Index vs. Distance from wildfires (km)",
    x = "Distance of Census Block Group from wildfires (km)",
    y = "Environmental Ballot Index"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(plot_scatter)
ggsave("scatter_mindistkm_envbi.png", plot = plot_scatter, width = 8, height = 6)

#c.2 Generate regression line for mindistkm and envbi
library(grid)
ggplot(data = datanum, aes(x = mindistkm, y = envbi)) +
  geom_point(shape = 19, color = "black", size = 0.25, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Figure 4. Scatter plot of distance of Census Block Groups from wildfires 
       vs. Environmental Ballot Index with regression line",
       x = "Distance from wildfire (km)",
       y = "Environmental Ballot Index") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("scatter_mindistkm_envbi_regression.png")

#######3.4 Regression Analysis#########
#a.
gb_km_model <- lm(envbi ~ mindistkm, data = datanum)
summary(gb_km_model)
print(gb_km_model)

#b. Save regression results with stargazer.
stargazer(
  gb_km_model, 
  type = "text", 
  title = "Bivariate Regression Results: Distance from wildfires predicting Environmental Ballot Index",
  dep.var.labels = "Environmental Ballot Index",
  covariate.labels = c("Distance from wildfires (km)", "Intercept"),
  out = "regression_results.txt"
)

#c. Manually calculate regression coefficients b0 and b1
b1 <- (cov(datanum$mindistkm, datanum$envbi)/var(datanum$mindistkm))
print(b1)
b0 <- mean_envbi-b1*mean_mindistkm
print(b0)

