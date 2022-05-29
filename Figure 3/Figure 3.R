###############################################################################
# Code to visualize the effect of each factors during the constrain global    #
# sensitivity analysis of each drug archetype                                 #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load the package
library("ggh4x")
library("ggplot2")
library("cowplot")

# load the data and add the drug name
Quantil_final_final<-read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3-Source data 1.csv", sep = ",", header = T)


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
Dr.labs <- c("Short-acting\ndrug", "Long-acting\ndrug", "Short-acting +\n Long-acting drugs")
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

# select the data
Quantil_final_2 <- Quantil_final_final[Quantil_final_final$G == "Fitness" | Quantil_final_final$G == "half_life_short" | Quantil_final_final$G == "C_max_IC50_short" | Quantil_final_final$G == "half_life_long" | Quantil_final_final$G == "C_max_IC50_long", ]
Quantil_final_3 <- Quantil_final_2[Quantil_final_2$EIR == 500 | Quantil_final_2$EIR == 5, ]
Quantil_final_3 <- Quantil_final_3
#Quantil_final_3 <- Quantil_final_3[Quantil_final_3$Access == 0.5 & Quantil_final_3$Seasonality == "sesonality1" | Quantil_final_3$Access == 0.04 & Quantil_final_3$Dosage == 1 & Quantil_final_3$Seasonality == "sesonality1" | Quantil_final_3$Access == 0.5 & Quantil_final_3$Dosage == 1 & Quantil_final_3$Seasonality == "sesonality2", ]
#Quantil_final_3 <- Quantil_final_3[Quantil_final_3$Resistance_level == 2.5 | Quantil_final_3$Resistance_level == 7, ]

Quantil_final_3$x[Quantil_final_3$G == "Fitness"] <- (Quantil_final_3$x[Quantil_final_3$G == "Fitness"] - 40) * -1
Quantil_final_3$x[Quantil_final_3$G == "C_max_IC50_short" & Quantil_final_3$drug == "A"] <- (Quantil_final_3$x[Quantil_final_3$G == "C_max_IC50_short" & Quantil_final_3$drug == "A"] - 40) * -1
Quantil_final_3$x[Quantil_final_3$G == "C_max_IC50_short" & Quantil_final_3$drug == "A+B"] <- (Quantil_final_3$x[Quantil_final_3$G == "C_max_IC50_short" & Quantil_final_3$drug == "A+B"] - 40) * -1



Quantil_final_4 <- Quantil_final_3[Quantil_final_3$Seasonality == "sesonality2", ]
Quantil_final_4 <- Quantil_final_4[Quantil_final_4$Dosage == 1, ]
Quantil_final_4<-Quantil_final_4[Quantil_final_4$G=="half_life_short" | Quantil_final_4$G=="Fitness" |Quantil_final_4$G=="C_max_IC50_long" |Quantil_final_4$G=="half_life_long",]
Quantil_final_4$G<-as.factor(Quantil_final_4$G)

# Do the plot for drug A
Quantil_final_4_1 <- Quantil_final_4[Quantil_final_4$drug == "A", ]
Quantil_final_4_1<-Quantil_final_4_1[Quantil_final_4_1$G=="half_life_short" & Quantil_final_4_1$Access==0.5 | Quantil_final_4_1$G=="Fitness" & Quantil_final_4_1$Access==0.04,]

break_y <- c(1, 10, 20, 30, 39)
Label_y <- c("Min", "", "", "", "Max")

Label_yy_1 <- c("", "", "", "", "")
constant<-2.5
pl11 <- ggplot(data = Quantil_final_4_1) +
  geom_line(aes(x = x, y = M, color = G, linetype = EIR), size = 1.9/constant, alpha = 1) +
  # facet_nested(drug~Access+Seasonality+Dosage,labeller=labeller(drug= Dr.labs,Access=T.labs, Dosage=D.labs, Seasonality=S.labs,Resistance_level=R.labs)) +
  facet_nested(drug ~ Access + Resistance_level, labeller = labeller(drug = Dr.labs, Access = T.labs, Dosage = D.labs, Seasonality = S.labs, Resistance_level = R.labs), scales = "free", independent = "x") +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Selection coefficient",limits=c(-0.125,0.6)) +
  scale_x_continuous(name = "",breaks = break_y, labels = Label_y) +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x =element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 12/constant)) +
  theme(legend.title = element_text(size = 12/constant, face = "bold")) +
  theme(legend.margin = margin(0,0,0,0, unit="cm"))+
  guides(
    linetype = guide_legend(keywidth = 5/constant, keyheight = 0.05/constant, override.aes = list(size = 2.5/constant)),
    colour = guide_legend(keywidth = 2/constant, keyheight = 0.05/constant, override.aes = list(size = 5/constant)),
    legend.spacing.y = unit(-1500, "cm")) +
  scale_color_manual(
    name = "Factors:",
    values = c(
      "#999933",
      "#CC6677", "#AA4499",
      "#88CCEE", "#44AA99","#44AA99","#44AA99","#44AA99"),
    breaks = c("Fitness", "half_life_short", "half_life_long", "C_max_IC50_long"),
    labels = c("Fitness cost", "Half-life (days) of the\nshort-acting drug", "Half-life (days) of the\nlong-acting drug", "Cmax/EC50 of the\nlong-acting drug"), 
    drop = FALSE) +
  scale_linetype_manual(
    values = c("solid", "solid", "dashed"),
    name = "EIR:",
    breaks = c("5", "10", "500"),
    labels = c("5", "10", "500")) +
  ggtitle(label = "") +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(-1, 0.2, 1, 0.2)/constant, "cm")) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#999999", size = 1.5) +
  theme(legend.position="top", legend.box="vertical")  +
  guides(fill=guide_legend(nrow=2))

pl11

# do the plot for drug B
Quantil_final_4_2 <- Quantil_final_4[Quantil_final_4$drug == "B", ]
Quantil_final_4_2<-Quantil_final_4_2[Quantil_final_4_2$G=="half_life_long" & Quantil_final_4_2$Access==0.5  & Quantil_final_4_2$Resistance_level==2.5 | Quantil_final_4_2$G=="C_max_IC50_long" & Quantil_final_4_2$Access==0.5  & Quantil_final_4_2$Resistance_level==10| Quantil_final_4_2$G=="Fitness" & Quantil_final_4_2$Access==0.04,]

# adjust the label
Label_yy_1 <- c("", "", "", "", "")
S.labs2 <- c("", "")
names(S.labs2) <- c("sesonality1", "sesonality2")
T.labs2 <- c("", "")
names(T.labs2) <- c("0.04", "0.5")
D.labs2 <- c("", "")
names(D.labs2) <- c("1", "0")

pl12 <- ggplot(data = Quantil_final_4_2) +
  geom_line(aes(x = x, y = M, color = G, linetype = EIR), size = 1.9/constant, alpha = 1) +
  # facet_nested(drug~Access+Seasonality+Dosage,labeller=labeller(drug= Dr.labs,Access=T.labs, Dosage=D.labs, Seasonality=S.labs,Resistance_level=R.labs)) +
  facet_nested(drug  ~ Access  + Resistance_level, labeller = labeller(drug = Dr.labs, Access = T.labs2, Dosage = D.labs, Seasonality = S.labs2, Resistance_level = R.labs)) +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Selection coefficient", limits=c(-0.125,0.6)) +
  scale_x_continuous(name = "", breaks = break_y, labels = Label_yy) +
  theme(
    axis.text.x =  element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold"),) +
  scale_color_manual(
    name = "Factors:",
    values = c(
      "#999933",
      "#CC6677", "#AA4499",
      "#88CCEE", "#44AA99"),
    breaks = c("Fitness", "half_life_short", "half_life_long", "C_max_IC50_short", "C_max_IC50_long"),
    labels = c("Fitness cost", "Half-life drug A (days)", "Half-life drug B (day)", "Cmax/EC50 drug A", "Cmax/EC50 drug B")) +
  scale_linetype_manual(
    values = c("solid", "solid", "dashed"),
    name = "EIR:",
    breaks = c("5", "10", "500"),
    labels = c("5", "10", "500")) +
  ggtitle(label = "") +
  theme(
    strip.text.x = element_blank(),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(-1, 0.2, 1, 0.2)/constant, "cm")) +
  theme(strip.background.x = element_rect(fill = "white", colour = "white")) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#999999", size = 1.5) +
  theme(legend.position = "none")

pl12

# do the plot for drug A and B
Quantil_final_4_3 <- Quantil_final_4[Quantil_final_4$drug == "A+B", ]
Quantil_final_4_3<-Quantil_final_4_3[Quantil_final_4_3$G=="C_max_IC50_long" & Quantil_final_4_3$Access==0.5 | Quantil_final_4_3$G=="Fitness" & Quantil_final_4_3$Access==0.04,]

# adjust the label
Label_yy_1 <- c("", "", "", "", "")
S.labs2 <- c("", "")
names(S.labs2) <- c("sesonality1", "sesonality2")
T.labs2 <- c("", "")
names(T.labs2) <- c("0.04", "0.5")
D.labs2 <- c("", "")
names(D.labs2) <- c("1", "0")

pl13 <- ggplot(data = Quantil_final_4_3) +
  geom_line(aes(x = x, y = M, color = G, linetype = EIR), size = 1.9/constant, alpha = 1) +
  # facet_nested(drug~Access+Seasonality+Dosage,labeller=labeller(drug= Dr.labs,Access=T.labs, Dosage=D.labs, Seasonality=S.labs,Resistance_level=R.labs)) +
  facet_nested(drug  ~ Access + Seasonality + Resistance_level, labeller = labeller(drug = Dr.labs, Access = T.labs2, Dosage = D.labs, Seasonality = S.labs2, Resistance_level = R.labs)) +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Selection coefficient",limits=c(-0.125,0.6)) +
  scale_x_continuous(name = "", breaks = break_y, labels = Label_yy) +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x = element_text(size = 16/constant, face = "bold", hjust = 0.3),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18, hjust = 0.75, face = "bold")) +
  scale_color_manual(
    name = "Factors:",
    values = c(
      "#999933",
      "#CC6677", "#AA4499",
      "#88CCEE", "#44AA99"),
    breaks = c("Fitness", "half_life_short", "half_life_long", "C_max_IC50_short", "C_max_IC50_long"),
    labels = c("Fitness cost", "Half-life drug A (day)", "Half-life drug B (day)", "Cmax/EC50 drug A", "Cmax/EC50 drug B")) +
  scale_linetype_manual(
    values = c("solid", "solid", "dashed"),
    name = "EIR:",
    breaks = c("5", "10", "500"),
    labels = c("5", "10", "500")) +
  ggtitle(label = "") +
  theme(
    strip.text.x = element_blank(),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(-2, 0.2, 1, 0.2)/constant, "cm")) +
  theme(strip.background.x = element_rect(fill = "white", colour = "white")) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#999999", size = 1.5) +
  theme(legend.position = "none")

pl13
# merge all the plot
Test_1 <- plot_grid(pl11, pl12, pl13,
                    ncol = 1, nrow = 3, rel_heights = c(1.55, 1, 1), scale = c(1, 1, 1))

Test_1<-Test_1+ 
  draw_label("Fitness cost", x = 0.12, y = 0.0308,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Fitness cost", x = 0.12, y = 0.3125,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Fitness cost", x = 0.12, y = 0.5942,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  
  draw_label("Fitness cost", x = 0.3475, y = 0.0308,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Fitness cost", x = 0.3475, y = 0.3125,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Fitness cost", x = 0.3475, y = 0.5942,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  
  draw_label("Cmax/EC50 of the\nlong-acting drug", x = 0.554, y = 0.0165,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Half-life (days) of the\nlong-acting drug", x = 0.541, y =  0.2982,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Half-life (days) of the\nshort-acting drug", x = 0.541, y = 0.5799,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  
  draw_label("Cmax/EC50 of the\nlong-acting drug", x = 0.7825, y = 0.0165,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Cmax/EC50 of the\nlong-acting drug", x = 0.7825, y =  0.2982,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Half-life (days) of the\nshort-acting drug", x = 0.769, y = 0.5799,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")

Test_1 <- plot_grid(pl11, pl12, pl13,
                    ncol = 1, nrow = 3, rel_heights = c(1.8, 1, 1), scale = c(1, 1, 1))
Test_1<-Test_1+ 
  draw_label("Fitness cost", x = 0.118, y = 0.04275,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Fitness cost", x = 0.118, y = 0.305,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Fitness cost", x = 0.118, y = 0.5673,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  
  draw_label("Fitness cost", x = 0.336, y = 0.04275,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Fitness cost", x = 0.336, y = 0.305,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Fitness cost", x = 0.336, y = 0.5673,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  
  draw_label("Cmax/EC50 of the\nlong-acting drug", x = 0.528, y = 0.024,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Half-life (days) of the\nlong-acting drug", x = 0.5125, y =  0.28625,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Half-life (days) of the\nshort-acting drug", x = 0.5125, y = 0.549,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  
  draw_label("Cmax/EC50 of the\nlong-acting drug", x = 0.7465, y = 0.024,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Cmax/EC50 of the\nlong-acting drug", x = 0.7465, y =  0.28625,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")+
  draw_label("Half-life (days) of the\nshort-acting drug", x = 0.73, y = 0.549,size=16/2.5, hjust = 0, vjust = 0, fontface ="bold")

#Test_1

ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3.pdf",
       plot = Test_1, width = 11, height = 13, device="pdf", units = "cm", dpi = 300)