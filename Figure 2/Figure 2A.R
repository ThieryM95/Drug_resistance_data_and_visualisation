###############################################################################
# Code to visuals the first order indices of each factors on the selection    #
# coefficient estimated during the global sensitivity analysis                #
# of parasite resistance to each drug archetype (figure 2 A)                  #
#                                                                             #
# Input: Table of first and total order indices for each factor estimated     #                                                                            #
#        during the global sensitivity analysis of each treatment profile     #
#        (Data_first_order_indices_GSA.csv)                                   #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load package
library("ggh4x")
library("ggplot2")

# load the data
data <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Data_first_order_indices_GSA.csv", header  = TRUE)


# Order the level of the factors (in the order of the highest sobol indices to lower one)
data$Factor <- factor(data$Factor, levels = c(
  "Diangostic",
  "Maximum killing rate of drug A",
  "Maximum killing rate of drug B",
  "EIR",
  "Cmax/IC50 of drug A",
  "Cmax/IC50 of drug B",
  "Half-life of drug A",
  "Half-life of drug B",
  "Fitness",
  "Resistance level of drug A",
  "Resistance level of drug B",
  "Access"
))

# Make drug variable as a factor for visualisation
data$drug <- factor(data$drug, levels = c("Drug A", "Drug B", "Drug A + Drug B"))

# Select only the non-seasonal setting
data_2 <- data[data$Setting == "Spread_sesonality1", ]

# define the break for the sobol indices on the y values
break_y <- c(0, 0.25, 0.5, 0.75, 1)
Label_yy <- c(0, 0.25, 0.5, 0.75, 1)

# Define the label for each drug archetype
Dr.labs <- c("Drug A", "Drug B", "Drug A + B")
names(Dr.labs) <- c("A", "B", "A+B")

# select only the first order indicies
data_3 <- data_2[data_2$Effect == "First", ]

constant<-2
# visualise as a columns
PA <- ggplot(data_3, aes(x = drug, y = First, fill = Factor)) +
  geom_col(color = "black", width = 0.6) +
  xlab("") +
  ylab("First-order indices") +
  scale_fill_manual(
    values = c(
      "#126429",
      "#5087C1", "#273871",
      "#999933",
      "#E6959F", "#AA4499",
      "#88CCEE", "#009E73",
      "#DDCC77",
      "#882255", "#661100",
      "#888888"),
    name = "Factors:",
    breaks = c("Access", "Resistance level of drug A", "Resistance level of drug B", "Fitness", "Half-life of drug A", "Half-life of drug B", "Cmax/IC50 of drug A", "Cmax/IC50 of drug B", "EIR", "Maximum killing rate of drug A", "Maximum killing rate of drug B", "Diangostic"),
    labels = c("Access to treatment (%)", "Degree of resistance to drug A", "Degree of resistance to drug B", "Fitness cost", "Half-life of drug A (days)", "Half-life of drug B (days)", "Cmax/EC50 of drug A", "Cmax/EC50 of drug B", "EIR (inoculations per person per year)", "Emax of drug A (per day)", "Emax of drug B (per day)", "Diagnostic detection limit\n (parasites/microliter)")) +
  scale_x_discrete(labels = c("Drug A" = "Drug A", "Drug B" = "Drug B", "Drug A + Drug B" = "Drug A\n + \nDrug B")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15/constant, colour = "black", face = "bold"),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 20/constant, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 16/constant)) +
  theme(legend.title = element_text(size = 16/constant, face = "bold")) +
  ggtitle(label = "")+
  theme(legend.key.size = unit(0.3, "cm"))


