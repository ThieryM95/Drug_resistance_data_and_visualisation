###############################################################################
# Code to visuals the first order indices of each factors on the selection    #
# coefficient estimated during the constrained global sensitivity analysis    #
# of parasite resistance to drug B (figure 2 A)                               #
#                                                                             #
# Input: Table of first and total order indices for each factor estimated     #                                                                            #
#        during the constrained global sensitivity analysis of drug A         #
#        (Data_first_order_indices_CSA_long.csv)                              #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load package
library("ggh4x")
library("ggplot2")

# load the data
data <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3-figure supplement 4-Source data 1.csv", header  = TRUE)

# transform the constrained variable into factor
data$Resistance_level <- factor(data$Resistance_level, levels = c("2.5", "10"))
data$Dosage <- factor(data$Dosage, levels = c("1", "0"))
data$EIR <- factor(data$EIR, levels = c("5", "10", "500"))
data$Treatment_access <- factor(data$Treatment_access, levels = c("0.5","0.04"))

# creat a lavel for each constrained variable
T.labs <- c("High access to treatment", "Low access to treatment")
names(T.labs) <- c("0.5", "0.04")
R.labs <- c("High degree\n of resistance", "Low degree\n of resistance")
names(R.labs) <- c("10", "2.5") 
D.labs <- c("High adherence\n to treatment", "Low adherence\n to treatment")
names(D.labs) <- c("1", "0")
S.labs <- c("No seasonality", "Seasonality")
names(S.labs) <- c("sesonality1", "sesonality2")

# define the break for the y axis
break_y <- c(0, 0.25, 0.5, 0.75, 1)
Label_yy <- c(0, 0.25, 0.5, 0.75, 1)

# ---- visualize the first order indices of each factors -----

# select the data
data_2 <- data[data$Effect == "First", ]

# define the level of the variable factors
data_2$Factor <- factor(data_2$Factor, levels = c("MKR", "Diangostic", "Cmax/IC50", "half-life", "Fitness"))

constant<-2
# Visualize
PLOT<-
  ggplot(data_2, aes(x = EIR, y = First, fill = Factor)) +
  geom_col(color = "black", width = 0.6) +
  facet_nested(Treatment_access + Resistance_level ~ Seasonality+ Dosage, labeller = labeller(Resistance_level = R.labs, Treatment_access = T.labs, Dosage = D.labs, Seasonality = S.labs)) +
  scale_fill_manual(
    name = "Factors:",
    values = c(
      "#999933",
      "#AA4499",
      "#009E73",
      "#661100",
      "#888888"),
    breaks = c("Fitness", "half-life", "Cmax/IC50", "MKR", "Diangostic"),
    labels = c("\nFitness cost\n", "\nHalf-life (days) of the\nlong-acting drug", "\nCmax/EC50 of the\nlong-acting drug", "\nEmax (per day) of the\nlong-acting drug", "\nDiagnostic detection\nlimit (parasites/ul)")) +
  theme(axis.title = element_text(face = "bold")) +
  ylab("First-order indices") +
  xlab("EIR (inoculations per person per year)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 20/constant, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 16/constant)) +
  theme(legend.title = element_text(size = 16/constant, face = "bold")) +
  ggtitle(label = "Long-acting drug") +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  scale_y_continuous(breaks = break_y, labels = Label_yy)+
  theme(legend.position="top", legend.box="vertical")  +
  guides(fill=guide_legend(nrow=2))


ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3-figure supplement 4.pdf",
       plot = PLOT, width = 16, height = 16, device="pdf", units = "cm", dpi = 300)
