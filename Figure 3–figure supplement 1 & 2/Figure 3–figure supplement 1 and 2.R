##################################################################################
# Code to visualize the effect of each factor during the constrained global      #
# sensitivity analyses of the spread of parasites resistant to each drug         #
# archetype  (figure_3_and_Figure_3_supplement_1)                                #
#                                                                                #
# Input: table of the estimated median and interquartile range of selection      #
#        coefficients estimated during the constrained global sensitivity        #
#        analyses over the parameter ranges (Data_factors_effect_CSA_all.csv)    #
#                                                                                #
# author: Thiery Masserey (thiery.masserey@swisstph.ch)                          #
##################################################################################

# Load the package
library("ggh4x")
library("ggplot2")

# Load the data
Quantil_final_final <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3-Source data 1.csv", header  = TRUE)

# Transform constrained variable into a factor
Quantil_final_final$drug <- factor(Quantil_final_final$drug, levels = c("A", "B", "A+B"))
Quantil_final_final$Dosage <- factor(Quantil_final_final$Dosage, levels = c("1", "0"))
Quantil_final_final$EIR <- factor(Quantil_final_final$EIR, levels = c("5", "10", "500"))

# Creat a label for each constrained variable
S.labs <- c("No seasonality", "Seasonality")
names(S.labs) <- c("sesonality1", "sesonality2")
T.labs <- c("Low access to treatment", "High access to treatment")
names(T.labs) <- c("0.04", "0.5")
D.labs <- c("High adherence\n to treatment", "Low adherence\n to treatment")
names(D.labs) <- c("1", "0")
Dr.labs <- c("Short-acting drug", "Long-acting drug", "Short-acting +\nLong-acting drugs")
names(Dr.labs) <- c("A", "B", "A+B")
Dr.labs_2 <- c("", "", "")
names(Dr.labs_2) <- c("A", "B", "A+B")
R.labs <- c("Low degree\n of resistance", "Low degree\n of resistance", "High degree\n of resistance", "High degree\n of resistance")
names(R.labs) <- c("7", "2.5", "18", "10")
R.labs_2 <- c("", "", "", "")
names(R.labs_2) <- c("7", "2.5", "18", "10")

# creat the label for the y axis
break_y <- c(1, 10, 20, 30, 39)
Label_yy <- c("Min", "", "", "", "Max")

# Select the data
Quantil_final_2 <- Quantil_final_final[Quantil_final_final$G == "Fitness" | Quantil_final_final$G == "half_life_short" | Quantil_final_final$G == "C_max_IC50_short" | Quantil_final_final$G == "half_life_long" | Quantil_final_final$G == "C_max_IC50_long", ]
Quantil_final_3 <- Quantil_final_2[Quantil_final_2$EIR == 500 | Quantil_final_2$EIR == 5, ]
Quantil_final_3 <- Quantil_final_3

# Transform IC50 and Fitness in the good direction
Quantil_final_3$x[Quantil_final_3$G == "Fitness"] <- (Quantil_final_3$x[Quantil_final_3$G == "Fitness"] - 40) * -1
Quantil_final_3$x[Quantil_final_3$G == "C_max_IC50_short" & Quantil_final_3$drug == "A"] <- (Quantil_final_3$x[Quantil_final_3$G == "C_max_IC50_short" & Quantil_final_3$drug == "A"] - 40) * -1
Quantil_final_3$x[Quantil_final_3$G == "C_max_IC50_short" & Quantil_final_3$drug == "A+B"] <- (Quantil_final_3$x[Quantil_final_3$G == "C_max_IC50_short" & Quantil_final_3$drug == "A+B"] - 40) * -1

#----- Plot for high level of access to treatment (Figure 3) ----
# plot all high acess to treatment
Quantil_final_4 <- Quantil_final_3[Quantil_final_3$Access == 0.5, ]
Quantil_final_4 <- Quantil_final_4[!(Quantil_final_4$drug == "A+B" & Quantil_final_4$G == "C_max_IC50_short"), ]
Quantil_final_4 <- Quantil_final_4[!(Quantil_final_4$drug == "A+B" & Quantil_final_4$G == "half_life_short"), ]

constant<-2
PLOT <- ggplot(data = Quantil_final_4) +
  geom_line(aes(x = x, y = M, color = G, linetype = EIR), size = 1.9/constant, alpha = 1) +
  # facet_nested(drug~Access+Seasonality+Dosage,labeller=labeller(drug= Dr.labs,Access=T.labs, Dosage=D.labs, Seasonality=S.labs,Resistance_level=R.labs)) +
  facet_nested(drug + Resistance_level ~  Seasonality + Dosage, labeller = labeller(drug = Dr.labs, Dosage = D.labs, Seasonality = S.labs, Resistance_level = R.labs)) +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Selection coefficient") +
  scale_x_continuous(name = "Factor values", breaks = break_y, labels = Label_yy) +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 20/constant, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 12/constant)) +
  theme(legend.title = element_text(size = 12/constant, face = "bold")) +
  scale_color_manual(
    name = "Factors:",
    values = c(
      "#999933",
      "#E6959F", "#AA4499",
      "#88CCEE", "#009E73"),
    breaks = c("Fitness", "half_life_short", "half_life_long", "C_max_IC50_short", "C_max_IC50_long"),
    labels = c("Fitness cost", "Half-life  (days) of the\nshort-acting drug", "Half-life (days) of the\nlong-acting drug", "Cmax/EC50 of the\nshort-acting drug", "Cmax/EC50 of the\nlong-acting drug")) +
  scale_linetype_manual(
    values = c("solid", "solid", "dashed"),
    name = "EIR (inoculations per person per year):",
    breaks = c("5", "10", "500"),
    labels = c("5", "10", "500")) +
  guides(
    linetype = guide_legend(keywidth = 5/constant, keyheight = 0.05/constant, override.aes = list(size = 2.5/constant)),
    colour = guide_legend(keywidth = 2/constant, keyheight = 0.05/constant, override.aes = list(size = 5/constant)),
    legend.spacing.y = unit(150, "cm")) +
  ggtitle(label = "") +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(0.05, 0.05,0.05 , 0.05), "cm")) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#999999", size = 1.5/constant) +
  theme(legend.position="top", legend.box="vertical")  +
  guides(fill=guide_legend(nrow=2))

ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3-figure supplement 1.pdf",
       plot = PLOT, width = 15, height = 18.5, device="pdf", units = "cm", dpi = 300)

#----- Plot for high level of access to treatment (Figure 3) ----

# plot all high acess to treatment
Quantil_final_4 <- Quantil_final_3[Quantil_final_3$Access == 0.04, ]
Quantil_final_4 <- Quantil_final_4[!(Quantil_final_4$drug == "A+B" & Quantil_final_4$G == "C_max_IC50_short"), ]
Quantil_final_4 <- Quantil_final_4[!(Quantil_final_4$drug == "A+B" & Quantil_final_4$G == "half_life_short"), ]

constant<-2
PLOT <- ggplot(data = Quantil_final_4) +
  geom_line(aes(x = x, y = M, color = G, linetype = EIR), size = 1.9/constant, alpha = 1) +
  # facet_nested(drug~Access+Seasonality+Dosage,labeller=labeller(drug= Dr.labs,Access=T.labs, Dosage=D.labs, Seasonality=S.labs,Resistance_level=R.labs)) +
  facet_nested(drug + Resistance_level ~  Seasonality + Dosage, labeller = labeller(drug = Dr.labs, Dosage = D.labs, Seasonality = S.labs, Resistance_level = R.labs)) +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Selection coefficient") +
  scale_x_continuous(name = "Factor values", breaks = break_y, labels = Label_yy) +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 20/constant, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 12/constant)) +
  theme(legend.title = element_text(size = 12/constant, face = "bold")) +
  scale_color_manual(
    name = "Factors:",
    values = c(
      "#999933",
      "#E6959F", "#AA4499",
      "#88CCEE", "#009E73"),
    breaks = c("Fitness", "half_life_short", "half_life_long", "C_max_IC50_short", "C_max_IC50_long"),
    labels = c("Fitness cost", "Half-life  (days) of the\nshort-acting drug", "Half-life (days) of the\nlong-acting drug", "Cmax/EC50 of the\nshort-acting drug", "Cmax/EC50 of the\nlong-acting drug")) +
  scale_linetype_manual(
    values = c("solid", "solid", "dashed"),
    name = "EIR (inoculations per person per year):",
    breaks = c("5", "10", "500"),
    labels = c("5", "10", "500")) +
  guides(
    linetype = guide_legend(keywidth = 5/constant, keyheight = 0.05/constant, override.aes = list(size = 2.5/constant)),
    colour = guide_legend(keywidth = 2/constant, keyheight = 0.05/constant, override.aes = list(size = 5/constant)),
    legend.spacing.y = unit(150, "cm")) +
  ggtitle(label = "") +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(0.05, 0.05,0.05, 0.05), "cm")) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#999999", size = 1.5/constant) +
  theme(legend.position="top", legend.box="vertical")  +
  guides(fill=guide_legend(nrow=2))

ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3-figure supplement 2.pdf",
       plot = PLOT, width = 15, height = 18.5, device="pdf", units = "cm", dpi = 300)
