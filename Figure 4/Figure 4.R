############################################################################################
# Code to visualize the relationship ship between the probability of establishment   and   #
# the selection coefficient for each drug archetype (figure 4)                             #
#                                                                                          #
# Input:  Table of selection coefficient and estimated probability of establishment        #
#         (Data_esthablishment.csv)                                                        #
#                                                                                          #
# authors: thiery.masserey@swisstph.ch                                                     #
############################################################################################

# load package
library("ggplot2")
library("ggh4x")

# Load the data
Scenario_liste <- read.csv(file = "/scicore/home/penny/masthi00/wf_esthablishment/Visualise_results/figure_paper/Data_esthablishment.csv", header  = TRUE)

# Define setting variable to be a factor
Scenario_liste$eir <- factor(Scenario_liste$eir,
                             levels = c("5", "10", "500"))

Scenario_liste$Dosage <- factor(Scenario_liste$Dosage,
                                levels = c("1", "0"))

Scenario_liste$drug <- factor(Scenario_liste$drug,
                              levels = c("A", "B", "A+B"))


# Define label for variable that are a factor
D.labs <- c("Adherence = 100 %", "Adherence = 60 %")
names(D.labs) <- c("1", "0")

E.labs <- c("EIR = 5", "EIR = 10", "EIR = 500")
names(E.labs) <- c("5", "10", "500")

Dr.labs <- c("Drug A", "Drug B", "Drug A + Drug B")
names(Dr.labs) <- c("A", "B", "A+B")

# Select the data from the setting that we want show on the plot
Scenario_liste_2 <- Scenario_liste[Scenario_liste$eir == 5 | Scenario_liste$eir == 500,]
Scenario_liste_2 <- Scenario_liste_2[Scenario_liste_2$Dosage == 1,]

# Define a constant use to adjust size of plot 
constant<-2.5
# Plot the data
PLOT<-ggplot(data = Scenario_liste_2) +
  geom_line(aes(x = Indicator, y = Pe, linetype = eir), size = 2 / constant) +
  facet_grid( ~ drug,
              scale = "free",
              labeller = labeller(eir = E.labs, drug = Dr.labs)) +
  theme_bw() +
  scale_linetype_manual(
    values = c("solid", "solid", "twodash"),
    name = "EIR:",
    breaks = c("5", "10", "500"),
    labels = c("5", "10", "500")  ) +
  scale_x_continuous(name = "Selection coefficient") +
  scale_y_continuous(name = "Probability of establishment") +
  theme(
    axis.text.x = element_text(size = 16 / constant),
    axis.text.y = element_text(size = 16 / constant),
    axis.title.x = element_text(size = 18 / constant, face = "bold"),
    axis.title.y = element_text(size = 18 / constant, face = "bold"),
    plot.title = element_text(
      size = 20 / constant,
      hjust = 0.5,
      face = "bold")) +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  ggtitle(label = "") +
  theme(
    strip.text.x = element_text(
      size = 18 / constant,
      color = "black",
      face = "bold"),
    strip.text.y = element_text(
      size = 18 / constant,
      color = "black",
      face = "bold")) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  guides(linetype = guide_legend(keywidth = 4 / constant, keyheight = 1 /constant)) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.spacing.y = unit(-5, "cm"),
    legend.margin = margin(-0.3, -0.3, -0.3, -0.3, unit = "cm"))

ggsave("/scicore/home/penny/masthi00/wf_esthablishment/Visualise_results/figure_paper/Figure 4.pdf",
       plot = PLOT, width = 12, height = 5, device="pdf", units = "cm", dpi = 300)