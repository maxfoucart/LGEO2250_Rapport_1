setwd("C:/Users/fouca/Documents/DocumentsDATA/GEO Master 2/Q2 Master 2/LGEO2250 Mesures de terrain en Géographie")

lab1 <- read.csv2("Lab1_LGEO2250.csv")
library(ggpubr)
library(esquisse)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(dplyr)

##### Mise en forme ####

lab1$Time..min. <- as.factor(lab1$Time..min.)

lab1$Measure <- as.factor(lab1$Measure)
lab1$Height = lab1$Height * -1
lab1$Height = lab1$Height + 34.27
#esquisser()

ggplot(lab1) +
  aes(x = Distance, y = Height, colour = Measure) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_bw() +
  facet_wrap(vars(Time..min.))


lab1_aggregated <- lab1 %>%
  group_by( Distance, Time..min.) %>%
  summarise(mean_height = mean(Height))

##### Figure 2 ####
ggplot(lab1_aggregated) +
  aes(x = Distance, y = mean_height, colour = Time..min.) +
  geom_line() +
  scale_color_hue(direction = 1) +
  labs(
    x = "Distance (cm)",
    y = "Hauteur (cm)",
    color = "Temps (min)"
  ) +
  theme_bw()# + facet_wrap(vars(Time..min.))

##### Création animation GIF ####

# Set up the ggplot with the aggregated data
p <- ggplot(lab1_aggregated, aes(x = Distance, y = mean_height)) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_bw() +
  labs(title = "Evolution of Height over Time")


# Assuming 'p' is your ggplot object
animated_plot <- p +
  transition_time(as.numeric(Time..min.)) +
  enter_fade() +
  exit_fade() +
  shadow_mark() +
  labs(title = "Mesure numéro: {round(frame_time)}") +
  theme_bw() +
  labs(
    x = "Distance (cm)",
    y = "Hauteur (cm)",
    color = "Temps (min)"
  )

# Save the animated plot as a GIF
#animate(animated_plot,duration = 5,fps = 20,width = 500,height = 400,renderer = gifski_renderer())


#Graphique pour chaque point, calculer la pente


####### Dérivée ####

finite.differences <- function(x,Z) {
  n <- length(x)
  fdZ <- NULL
  for (i in 2:n-1) {
    fdZ[i] <- (Z[i-1] - Z[i+1]) / (x[i-1] - x[i+1])}
  fdZ[n] <- (Z[n] - Z[n - 1]) / (x[n] - x[n - 1])
  fdZ[1] <- (Z[1] - Z[2]) / (x[1] - x[2])
  return (fdZ)
}
#lab1_aggregated$diff = finite.differences(lab1_aggregated$mean_height, lab1_aggregated$Distance)

##### Calcul des dérivées premières et seconde ####
#T0
labT0 <- lab1[lab1$Time..min. == 0, ]

labT0_agg <- labT0 %>%
  group_by( Distance, Time..min.) %>%
  summarise(mean_height = mean(Height))

labT0_agg$diff = finite.differences(labT0_agg$Distance, labT0_agg$mean_height)
labT0_agg$d2 = finite.differences(labT0_agg$Distance, labT0_agg$diff)
#T1
labT15 <- lab1[lab1$Time..min. == 15, ]

labT15_agg <- labT15 %>%
  group_by( Distance, Time..min.) %>%
  summarise(mean_height = mean(Height))

labT15_agg$diff = finite.differences(labT15_agg$Distance, labT15_agg$mean_height)
labT15_agg$d2 = finite.differences(labT15_agg$Distance, labT15_agg$diff)
#T2
labT45 <- lab1[lab1$Time..min. == 45, ]

labT45_agg <- labT45 %>%
  group_by( Distance, Time..min.) %>%
  summarise(mean_height = mean(Height))

labT45_agg$diff = finite.differences(labT45_agg$Distance, labT45_agg$mean_height)
labT45_agg$d2 = finite.differences(labT45_agg$Distance, labT45_agg$diff)
#T3

labT75 <- lab1[lab1$Time..min. == 75, ]

labT75_agg <- labT75 %>%
  group_by( Distance, Time..min.) %>%
  summarise(mean_height = mean(Height))

labT75_agg$diff = finite.differences(labT75_agg$Distance, labT75_agg$mean_height)
labT75_agg$d2 = finite.differences(labT75_agg$Distance, labT75_agg$diff)
# Now, you can use the lab1_aggregated$Diff vector without encountering the error

##### Fichier comprenant toutes les données
lab_all <- rbind(labT0_agg, labT15_agg, labT45_agg, labT75_agg)

#esquisser(lab_all)

##### Figure 3 ####

ggplot(lab_all) +
  aes(x = Distance, y = diff, colour = Time..min.) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_bw()+
  labs(
    x = "Distance (cm)",
    y = "Pente",
    color = "Temps (min)"
  )

##### Figure 4 ####

ggplot(lab_all) +
  aes(x = Distance, y = d2, colour = Time..min.) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_bw()+
  labs(
    x = "Distance (cm)",
    y = "Courbure",
    color = "Temps (min)"
  )

#####Graphique de pente erosion vs dépot####

# Calculate erosion values directly
lab_all <- lab_all %>%
  mutate(
    eros1 = mean_height[Time..min. == 15] - mean_height[Time..min. == 0],
    eros2 = mean_height[Time..min. == 45] - mean_height[Time..min. == 15],
    eros3 = mean_height[Time..min. == 75] - mean_height[Time..min. == 45],
    eros4 = mean_height[Time..min. == 75] - mean_height[Time..min. == 0]
  )

lab_all$Time..min. = as.factor(lab_all$Time..min.)

lab_long = pivot_longer(lab_all, cols = c(eros1, eros2, eros3, eros4), names_to = "Colonnes")

lm(lab_all$eros1~lab_all$d2)
library(patchwork)
lab_all %>%
  filter(Time..min. %in% "0") %>%
  ggplot() +
  aes(x = d2, y = eros1) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  geom_smooth(method = lm,span = 1, se= FALSE, show.legend = TRUE) +
  theme_bw() +
  stat_regline_equation(label.y = 1, aes(label = paste(..eq.label.., sep = "~~~"))) +
  stat_cor(label.y = 0.75, aes(label = paste("p-value =", ..p.value.., sep = " ")))

lab_all %>%
  filter(Time..min. %in% "15") %>%
  ggplot() +
  aes(x = d2, y = eros2) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  geom_smooth(method = lm,span = 1, se= FALSE, show.legend = TRUE) +
  theme_bw() +
  stat_regline_equation(label.y = 1, aes(label = paste(..eq.label.., sep = "~~~"))) +
  stat_cor(label.y = 0.75, aes(label = paste("p-value =", ..p.value.., sep = " ")))

lab_all %>%
  filter(Time..min. %in% "45") %>%
  ggplot() +
  aes(x = d2, y = eros1) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  geom_smooth(method = lm,span = 1, se= FALSE, show.legend = TRUE) +
  theme_bw() +
  stat_regline_equation(label.y = 1, aes(label = paste(..eq.label.., sep = "~~~"))) +
  stat_cor(label.y = 0.75, aes(label = paste("p-value =", ..p.value.., sep = " ")))

lab_all %>%
  filter(Time..min. %in% "75") %>%
  ggplot() +
  aes(x = d2, y = eros1) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  geom_smooth(method = lm,span = 1, se= FALSE, show.legend = TRUE) +
  theme_bw() +
  stat_regline_equation(label.y = 1, aes(label = paste(..eq.label.., sep = "~~~"))) +
  stat_cor(label.y = 0.75, aes(label = paste("p-value =", ..p.value.., sep = " ")))

p1 <- p + geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)

lab_all %>%
  mutate(Time..min. = as.numeric(Time..min.)) %>%
  group_by(Time..min.) %>%
  ggplot() +
  aes(x = d2, y = eros1) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  geom_smooth(method = lm, se = FALSE, show.legend = TRUE, formula = y ~ x + 0) +
  theme_bw() +
  stat_regline_equation(label.y = 1, aes(label = paste(..eq.label.., sep = "~~~"))) +
  stat_cor(label.y = 0.75, aes(label = paste("p-value =", ..p.value.., sep = " "))) +
  facet_wrap(~Time..min., scales = "free") +
  labs(title = "Faceted Regression", x = "d2", y = "eros1")

##### Figure 5 ####

Plot1 = lab_all %>%
  filter(Time..min. %in% "0") %>%
  ggplot() +
  aes(x = d2, y = eros1) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  geom_smooth(method = lm, se = FALSE, show.legend = TRUE, formula = y ~ 0 + x) +
  theme_bw() +
  labs(title = "Regression pour T 0min à T 15min", x = "Courbure", y = "Erosion/Dépot")+
  stat_regline_equation(label.y = 0.65, aes(label = paste(..eq.label.., sep = "~~~"))) +
  stat_cor(label.y = 0.5, aes(label = paste("p-value =", ..p.value.., sep = " ")))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")


Plot2 = lab_all %>%
  filter(Time..min. %in% "15") %>%
  ggplot() +
  aes(x = d2, y = eros2) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  geom_smooth(method = lm, se = FALSE, show.legend = TRUE, formula = y ~ 0 + x) +
  theme_bw() +
  labs(title = "Regression pour T 15min à T 45min", x = "Courbure", y = "Erosion/Dépot")+
  stat_regline_equation(label.y = 0.65, aes(label = paste(..eq.label.., sep = "~~~"))) +
  stat_cor(label.y = 0.5, aes(label = paste("p-value =", ..p.value.., sep = " ")))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")


Plot3 = lab_all %>%
  filter(Time..min. %in% "45") %>%
  ggplot() +
  aes(x = d2, y = eros3) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  geom_smooth(method = lm, se = FALSE, show.legend = TRUE, formula = y ~ 0 + x) +
  theme_bw() +
  labs(title = "Regression pour T 45min à T 75min", x = "Courbure", y = "Erosion/Dépot")+
  stat_regline_equation(label.y = 0.65, aes(label = paste(..eq.label.., sep = "~~~"))) +
  stat_cor(label.y = 0.5, aes(label = paste("p-value =", ..p.value.., sep = " ")))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")


Plot4 = lab_all %>%
  filter(Time..min. %in% "75") %>%
  ggplot() +
  aes(x = d2, y = eros4) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  geom_smooth(method = lm, se = FALSE, show.legend = TRUE, formula = y ~ 0 + x) +
  theme_bw() +
  labs(title = "Regression pour T 0min à T 75min", x = "Courbure", y = "Erosion/Dépot")+
  stat_regline_equation(label.y = 0.65, aes(label = paste(..eq.label.., sep = "~~~"))) +
  stat_cor(label.y = 0.5, aes(label = paste("p-value =", ..p.value.., sep = " ")))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")

combined_plot <- Plot1 + Plot2 + Plot3 + Plot4
combined_plot
ggsave("combined_plot.png", combined_plot, width = 30, height = 20, units = "cm")

##### Calcul de k ####
# E/D = (-k/rho)*courb
# -k = (E/D*rho)/courb
rho = 0.008 #kg/cm^3

lab_all$K1 <- lab_all$eros1 * rho / lab_all$d2
lab_all$K2 <- lab_all$eros2 * (rho / lab_all$d2)
lab_all$K3 <- lab_all$eros3 * (rho / lab_all$d2)
lab_all$K4 <- lab_all$eros4 * (rho / lab_all$d2)
mean(lab_all$K1)
mean(lab_all$K2)
mean(lab_all$K3)
mean(lab_all$K4)

