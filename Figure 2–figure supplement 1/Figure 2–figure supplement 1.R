##################################################################################
# Code to visualize the effect of each factor during the global sensitivity      #
# analyses of the spread of parasites resistant to drug A or drug B when used    #
# in montherapy  (figure_2_supplement_1)                                         #
#                                                                                #
# Input: table of the estimated median and interquartile range of selection      #
#        coefficients estimated during the global sensitivity analyses over the  #
#        parameter ranges (Data_factors_effect_GSA_short_long.csv)               #
#                                                                                #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                          #
##################################################################################

# Load package
library("ggh4x")
library("ggplot2")

# Load the data
Quantil_final_final <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 2-figure supplement 1-Source data 1.csv", sep = ",", header = T)

# select non seasonal setting
Quantil_final_2 <- Quantil_final_final[Quantil_final_final$Seasonality == "sesonality1",]

# select factor of interest
Quantil_final_2 <-  Quantil_final_2[Quantil_final_2$G == "Access" |
                                      Quantil_final_2$G == "Resistance level of drug B" |
                                      Quantil_final_2$G == "Resistance level of drug A",]

# Define a label for lower value of each factor and one for highest value
break_y <- c(1, 10, 20, 30, 39)
Label_yy <- c("Min", "", "", "", "Max")

# Define label for each drug type
Dr.labs <- c("Short-acting drug", "Long-acting drug")
names(Dr.labs) <- c("Drug A", "Drug B")

constant<-2
# Visualise the results
PLOT<-
  ggplot(data = Quantil_final_2) +
  geom_line(aes(x = x, y = M, color = G), size = 2/constant) +
  geom_ribbon(aes(
    x = x,
    ymin = L,
    ymax = U,
    fill = G)
    , alpha = 0.15) +
  facet_grid(. ~ drug,  labeller = labeller(drug = Dr.labs)) +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Selection coefficient") +
  scale_x_continuous(name = "Factor values",
                     breaks = break_y,
                     labels = Label_yy) +
  theme(
    axis.text.x = element_text(size = 16/constant),
    axis.text.y = element_text(size = 16/constant),
    axis.title.x = element_text(size = 18/constant, face = "bold"),
    axis.title.y = element_text(size = 18/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 18/constant)) +
  theme(legend.title = element_text(size = 18/constant, face = "bold")) +
  scale_color_manual(
    name = "Factors:",
    values = c("#126429", "#709FCD", "#273871"),
    breaks = c(
      "Access",
      "Resistance level of drug A",
      "Resistance level of drug B",
      "half_life",
      "Fitness",
      "eir",
      "C_max_IC50"),
    labels = c(
      "Access to treatment (%)",
      "Degree of resistance to the short-acting drug",
      "Degree of resistance to the long-acting drug",
      "Half-life",
      "Fitness cost",
      "EIR",
      "Cmax/IC50")) +
  scale_fill_manual(
    name = "Factors:",
    values = c("#117733", "#6699CC", "#332288"),
    breaks = c(
      "Access",
      "Resistance level of drug A",
      "Resistance level of drug B",
      "half_life",
      "Fitness",
      "eir",
      "C_max_IC50"),
    labels = c(
      "Access to treatment (%)",
      "Degree of resistance to the short-acting drug",
      "Degree of resistance to the long-acting drug",
      "Half-life",
      "Fitness cost",
      "EIR",
      "Cmax/IC50")) +
  ggtitle(label = "") +
  theme(
    strip.text.x = element_text(size = 18/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 18/constant, color = "black", face = "bold"))+
  theme(legend.position = "bottom", legend.direction = "vertical")+
  theme(legend.key.size = unit(1/constant, "cm")) 

ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 2-figure supplement 1.pdf",
       plot = PLOT, width = 10, height = 9, device="pdf", units = "cm", dpi = 300)