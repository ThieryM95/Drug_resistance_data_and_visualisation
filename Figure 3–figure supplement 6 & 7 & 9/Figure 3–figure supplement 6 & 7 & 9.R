#################################################################################
# Code to visualizes the difference in the distribution of selection coefficient#
# across each arm of the constrain analysis for each drug archetype             #
#                                                                               #
# Input: Data that summarise for each simulation the input parameter, and       #
#        estimated selection coefficient for each drug archetype                #
#        (Data_summary_CSA_all.csv)                                             #
#                                                                               #                    #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                         #
#################################################################################

# Load package
library("ggh4x")
library("ggplot2")

# Load the data
Scenario_liste <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3-figure supplement 6&7&9-Source data 1.csv", header  = TRUE)

# Change arm information into factor
Scenario_liste$drug <- factor(Scenario_liste$drug,
                              levels = c("Drug A", "Drug B", "Drug A + Drug B"))

Scenario_liste$eir <- factor(Scenario_liste$eir,
                             levels = c("5", "10", "500"))

Scenario_liste$Access <- factor(Scenario_liste$Access,
                                levels = c("0.04", "0.5"))

Scenario_liste$Resistance_Level <- factor(Scenario_liste$Resistance_Level,
                                          levels = c("2.5", "7", "10", "18"))

Scenario_liste$Dosage <- factor(Scenario_liste$Dosage,
                                levels = c("1", "0"))

Scenario_liste$seasonality <- factor(Scenario_liste$seasonality,
                                     levels = c("sesonality1", "sesonality2"))


#---- Plot at high level of access to treatment (Figure_3_supplement_5) ----

# Select arm that have a high level of treatment access
Scenario_liste_2 <- Scenario_liste[Scenario_liste$Access == 0.5, ]
Scenario_liste_2$Resistance_Level[Scenario_liste_2$Resistance_Level == 2.5] <- 7
Scenario_liste_2$Resistance_Level[Scenario_liste_2$Resistance_Level == 10] <- 18
Scenario_liste_2 <- Scenario_liste_2[Scenario_liste_2$Resistance_Level == 7, ]

# Define the label
R.labs <-  c(
  "Low resistance\n level",
  "Low resistance\n level",
  "High resistance\n level",
  "High resistance\n level")
names(R.labs) <- c("7", "2.5", "18", "10")

Dr.labs <- c("Short-acting drug", "Long-acting drug", "Short-acting +\nLong-acting drugs")
names(Dr.labs) <- c("Drug A", "Drug B", "Drug A + Drug B")


constant<-2
# Plot by level of adherence to treatment
pd <- position_dodge(0.9)
PP1 <- ggplot(Scenario_liste_2, aes(x = Dosage, y = Indicator, fill = Dosage)) +
  geom_boxplot(position = pd) +
  labs(title = "", x = "Adherence to treatment (%)", y = "Selection coefficient") +
  scale_x_discrete(labels = c("1" = "100%", "0" = "60%")) +
  facet_grid( ~ drug,labeller = labeller(drug = Dr.labs)) +
  scale_fill_manual(
    name = "Adherence to\n  treatment",
    values = c("lightblue", "blue"),
    breaks = c(1, 0),
    labels = c("100 %", "60 %")) +
  scale_color_manual(
    name = "Resistance level",
    values = c("black", "grey"),
    breaks = c(2.5, 10),
    labels = c("Low (2.5)", "High (10)")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 16/constant),
    legend.title = element_text(size = 16/constant)) +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(legend.position = "none")

# Plot by level of seasonality
pd <- position_dodge(0.9)
PP2 <-ggplot(Scenario_liste_2,
             aes(x = seasonality, y = Indicator,  fill = seasonality)) +
  geom_boxplot(position = pd) +
  facet_grid( ~ drug,labeller = labeller(drug = Dr.labs)) +
  labs(title = "", x = "Seasonality", y = "Selection coefficient") +
  scale_x_discrete(labels = c("sesonality1" = "No", "sesonality2" = "Yes")) +
  scale_fill_manual(
    name = "Seasonality",
    values = c("lightblue", "blue"),
    breaks = c("sesonality1", "sesonality2"),
    labels = c("No", "Yes")) +
  scale_color_manual(
    name = "Resistance level",
    values = c("black", "grey"),
    breaks = c(2.5, 10),
    labels = c("Low (2.5)", "High (10)")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15/constant),
        axis.text.y = element_text(size = 15/constant),
        axis.title.y = element_text(size = 16/constant, face = "bold"),
        axis.title.x = element_text(size = 16/constant, face = "bold"),
        plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 16/constant),
        legend.title = element_text(size = 16/constant)) +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(legend.position = "none")


# Plot by level of transmission
pd <- position_dodge(0.9)
PP3 <- ggplot(Scenario_liste_2, aes(x = eir, y = Indicator, fill = eir)) +
  geom_boxplot(position = pd) +
  labs(title = "", x = "EIR (inoculations per person per year)", y = "Selection coefficient") +
  facet_grid( ~ drug,labeller = labeller(drug = Dr.labs)) +
  scale_x_discrete(labels = c("5" = "5", "10" = "10", "500" = "500")) +
  scale_fill_manual(name = "EIR",
                    values = c("white", "lightblue", "blue")) +
  scale_color_manual(
    name = "Resistance level",
    values = c("black", "grey"),
    breaks = c(2.5, 10),
    labels = c("Low (2.5)", "High (10)")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 16/constant),
    legend.title = element_text(size = 16/constant)) +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(legend.position = "none")


# Merge all plot
PLOT<-plot_grid(
  PP3,
  PP2,
  PP1,
  ncol = 1,
  nrow = 3,
  rel_widths = c(1, 1),
  scale = 1,
  labels = c('A', 'B', "C"),
  label_size = 18/constant)

ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3-figure supplement 6.pdf",
       plot = PLOT, width = 11, height = 16, device="pdf", units = "cm", dpi = 300)

#---- Plot at high level of resistance (Figure_3_supplement_5) ----

# Select arm that have a high level of treatment access
Scenario_liste_2 <- Scenario_liste[Scenario_liste$Access == 0.5, ]
Scenario_liste_2$Resistance_Level[Scenario_liste_2$Resistance_Level == 2.5] <- 7
Scenario_liste_2$Resistance_Level[Scenario_liste_2$Resistance_Level == 10] <- 18
Scenario_liste_2 <- Scenario_liste_2[Scenario_liste_2$Resistance_Level == 18, ]

# Define the label
R.labs <-  c(
  "Low resistance\n level",
  "Low resistance\n level",
  "High resistance\n level",
  "High resistance\n level")
names(R.labs) <- c("7", "2.5", "18", "10")

Dr.labs <- c("Short-acting drug", "Long-acting drug", "Short-acting +\nLong-acting drugs")
names(Dr.labs) <- c("Drug A", "Drug B", "Drug A + Drug B")


constant<-2
# Plot by level of adherence to treatment
pd <- position_dodge(0.9)
PP1 <- ggplot(Scenario_liste_2, aes(x = Dosage, y = Indicator, fill = Dosage)) +
  geom_boxplot(position = pd) +
  labs(title = "", x = "Adherence to treatment (%)", y = "Selection coefficient") +
  scale_x_discrete(labels = c("1" = "100%", "0" = "60%")) +
  facet_grid( ~ drug,labeller = labeller(drug = Dr.labs)) +
  scale_fill_manual(
    name = "Adherence to\n  treatment",
    values = c("lightblue", "blue"),
    breaks = c(1, 0),
    labels = c("100 %", "60 %")) +
  scale_color_manual(
    name = "Resistance level",
    values = c("black", "grey"),
    breaks = c(2.5, 10),
    labels = c("Low (2.5)", "High (10)")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 16/constant),
    legend.title = element_text(size = 16/constant)) +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(legend.position = "none")

# Plot by level of seasonality
pd <- position_dodge(0.9)
PP2 <-ggplot(Scenario_liste_2,
             aes(x = seasonality, y = Indicator,  fill = seasonality)) +
  geom_boxplot(position = pd) +
  facet_grid( ~ drug,labeller = labeller(drug = Dr.labs)) +
  labs(title = "", x = "Seasonality", y = "Selection coefficient") +
  scale_x_discrete(labels = c("sesonality1" = "No", "sesonality2" = "Yes")) +
  scale_fill_manual(
    name = "Seasonality",
    values = c("lightblue", "blue"),
    breaks = c("sesonality1", "sesonality2"),
    labels = c("No", "Yes")) +
  scale_color_manual(
    name = "Resistance level",
    values = c("black", "grey"),
    breaks = c(2.5, 10),
    labels = c("Low (2.5)", "High (10)")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15/constant),
        axis.text.y = element_text(size = 15/constant),
        axis.title.y = element_text(size = 16/constant, face = "bold"),
        axis.title.x = element_text(size = 16/constant, face = "bold"),
        plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 16/constant),
        legend.title = element_text(size = 16/constant)) +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(legend.position = "none")


# Plot by level of transmission
pd <- position_dodge(0.9)
PP3 <- ggplot(Scenario_liste_2, aes(x = eir, y = Indicator, fill = eir)) +
  geom_boxplot(position = pd) +
  labs(title = "", x = "EIR (inoculations per person per year)", y = "Selection coefficient") +
  facet_grid( ~ drug,labeller = labeller(drug = Dr.labs)) +
  scale_x_discrete(labels = c("5" = "5", "10" = "10", "500" = "500")) +
  scale_fill_manual(name = "EIR",
                    values = c("white", "lightblue", "blue")) +
  scale_color_manual(
    name = "Resistance level",
    values = c("black", "grey"),
    breaks = c(2.5, 10),
    labels = c("Low (2.5)", "High (10)")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 16/constant),
    legend.title = element_text(size = 16/constant)) +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(legend.position = "none")


# Merge all plot
PLOT<-plot_grid(
  PP3,
  PP2,
  PP1,
  ncol = 1,
  nrow = 3,
  rel_widths = c(1, 1),
  scale = 1,
  labels = c('A', 'B', "C"),
  label_size = 18/constant)

ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3-figure supplement 7.pdf",
       plot = PLOT, width = 11, height = 16, device="pdf", units = "cm", dpi = 300)

#---- Plot at low level of access to treatment (Figure_3_supplement_7) ----

# Select arm that have a low level of treatment access
Scenario_liste_2 <- Scenario_liste[Scenario_liste$Access == 0.04, ]
Scenario_liste_2$Resistance_Level[Scenario_liste_2$Resistance_Level == 2.5] <- 7
Scenario_liste_2$Resistance_Level[Scenario_liste_2$Resistance_Level == 10] <- 18
Scenario_liste_2 <- Scenario_liste_2[Scenario_liste_2$Resistance_Level == 7, ]

# define the label
R.labs <- c(
  "Low resistance\n level",
  "Low resistance\n level",
  "High resistance\n level",
  "High resistance\n level")

names(R.labs) <- c("7", "2.5", "18", "10")

constant<-2
# plot by level of treatment adherence
pd <- position_dodge(0.9)
PP1 <- ggplot(Scenario_liste_2, aes(x = Dosage, y = Indicator, fill = Dosage)) +
  geom_boxplot(position = pd) +
  labs(title = "", x = "Adherence to treatment (%)", y = "Selection coefficient") +
  scale_x_discrete(labels = c("1" = "100%", "0" = "60%")) +
  facet_grid( ~ drug,labeller = labeller(drug = Dr.labs)) +
  scale_fill_manual(
    name = "Adherence to\n  treatment",
    values = c("lightblue", "blue"),
    breaks = c(1, 0),
    labels = c("100 %", "60 %")) +
  scale_color_manual(
    name = "Resistance level",
    values = c("black", "grey"),
    breaks = c(2.5, 10),
    labels = c("Low (2.5)", "High (10)")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 16/constant),
    legend.title = element_text(size = 16/constant)) +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(legend.position = "none")

# plot by level of seasonality
pd <- position_dodge(0.9)
PP2 <- ggplot(Scenario_liste_2,
              aes(x = seasonality, y = Indicator,  fill = seasonality)) +
  geom_boxplot(position = pd) +
  facet_grid( ~ drug,labeller = labeller(drug = Dr.labs)) +
  labs(title = "", x = "Seasonality", y = "Selection coefficient") +
  scale_x_discrete(labels = c("sesonality1" = "No", "sesonality2" = "Yes")) +
  scale_fill_manual(
    name = "Seasonality",
    values = c("lightblue", "blue"),
    breaks = c("sesonality1", "sesonality2"),
    labels = c("No", "Yes")) +
  scale_color_manual(
    name = "Resistance level",
    values = c("black", "grey"),
    breaks = c(2.5, 10),
    labels = c("Low (2.5)", "High (10)")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 16/constant),
    legend.title = element_text(size = 16/constant)) +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(legend.position = "none")

# plot by level of transmission
pd <- position_dodge(0.9)
PP3 <- ggplot(Scenario_liste_2, aes(x = eir, y = Indicator, fill = eir)) +
  geom_boxplot(position = pd) +
  labs(title = "", x = "EIR (inoculations per person per year)", y = "Selection coefficient") +
  facet_grid( ~ drug,labeller = labeller(drug = Dr.labs)) +
  scale_x_discrete(labels = c("5" = "5", "10" = "10", "500" = "500")) +
  scale_fill_manual(name = "EIR",
                    values = c("white", "lightblue", "blue")) +
  scale_color_manual(
    name = "Resistance level",
    values = c("black", "grey"),
    breaks = c(2.5, 10),
    labels = c("Low (2.5)", "High (10)")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 16/constant),
    legend.title = element_text(size = 16/constant)) +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(legend.position = "none")

# Merge all plot
PLOT<-plot_grid(
  PP3,
  PP2,
  PP1,
  ncol = 1,
  nrow = 3,
  rel_widths = c(1, 1),
  scale = 1,
  labels = c('A', 'B', "C"),
  label_size = 18/constant)

ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3-figure supplement 9.pdf",
       plot = PLOT, width = 11, height = 16, device="pdf", units = "cm", dpi = 300)
