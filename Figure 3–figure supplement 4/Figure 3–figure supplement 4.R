###############################################################################
# Code to visuals the first order indices of each factors on the selection    #
# coefficient estimated during the constrained global sensitivity analysis    #
# of parasite resistance to drug A+B (figure 2 A)                               #
#                                                                             #
# Input: Table of first and total order indices for each factor estimated     #                                                                            #
#        during the constrained global sensitivity analysis of drug B         #
#        (Data_first_order_indices_CSA_ACT.csv)                              #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load package
library("ggh4x")
library("ggplot2")

# load the data
data <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Data_first_order_indices_CSA_ACT.csv", header  = TRUE)

# transform the constrained variable into factor
data$Resistance_level <- factor(data$Resistance_level, levels = c("7", "18"))
data$Dosage <- factor(data$Dosage, levels = c("1", "0"))
data$EIR <- factor(data$EIR, levels = c("5", "10", "500"))
data$Treatment_access <- factor(data$Treatment_access, levels = c("0.5","0.04"))

# creat a lavel for each constrained variable
T.labs <- c("High access to treatment", "Low access to treatment")
names(T.labs) <- c("0.5", "0.04")
R.labs <- c("High degree\n of resistance", "Low degree\n of resistance")
names(R.labs) <- c("18", "7") 
D.labs <- c("High adherence\n to treatment", "Low adherence\n to treatment")
names(D.labs) <- c("1", "0")
S.labs <- c("No seasonality", "Seasonality")
names(S.labs) <- c("sesonality1", "sesonality2")

# define the break for the y axis
break_y <- c(0, 0.25, 0.5, 0.75, 1)
Label_yy <- c(0, 0.25, 0.5, 0.75, 1)

# ---- visualise the first order indices of each factors -----

# select the data
data_2 <- data[data$Effect == "First", ]

# order the level of each factors
data_2$Factor <- factor(data_2$Factor, levels = c(
  "Diangostic",
  "MKR_short",
  "MKR_long",
  "IC50_S_short",
  "half-life_short",
  "half-life_long",
  "Cmax/IC50",
  "Fitness"))

constant<-2
# visualise
PLOT<-ggplot(data_2, aes(x = EIR, y = First, fill = Factor)) +
  geom_col(color = "black", width = 0.6) +
  facet_nested(Treatment_access + Resistance_level ~ Seasonality + Dosage, labeller = labeller(Resistance_level = R.labs, Treatment_access = T.labs, Dosage = D.labs, Seasonality = S.labs)) +
  scale_fill_manual(
    values = c(
      "#999933",
      "#E6959F", "#AA4499",
      "#88CCEE", "#009E73",
      "#882255", "#661100",
      "#888888"),
    name = "Factors:",
    breaks = c("Fitness", "half-life_short", "half-life_long", "IC50_S_short", "Cmax/IC50", "MKR_short", "MKR_long", "Diangostic"),
    labels = c("\nFitness cost\n", "\nHalf-life of drug A\n (days)", "\nHalf-life of drug B\n (days)", "\nCmax/EC50 of drug A\n", "\nCmax/EC50 of drug B\n", "\nEmax of drug A\n (per day)", "\nEmax of drug B\n (per day)", "\nDiagnostic detection\n limit (parasites/ul)")) +
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
  ggtitle(label = "Drug A + Drug B") +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
    scale_y_continuous(breaks = break_y, labels = Label_yy)

ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3–figure supplement 4.pdf",
       plot = PLOT, width = 17, height = 13, device="pdf", units = "cm", dpi = 300)
