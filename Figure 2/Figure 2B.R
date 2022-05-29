##################################################################################
# Code to visualize the effect of each factor during the global                  #
# sensitivity analyses of the spread of parasites resistant to drug A+ B         #
#                                                                                #
# Input: table of the estimated median and interquartile range of selection      #
#        coefficients estimated during the global sensitivity                    #
#        analyses over the parameter ranges (Data_factors_effect_GSA_ACT.csv)    #
#                                                                                #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                          #
##################################################################################


# Load the package
library("ggh4x")
library("ggplot2")

# Load the data
Quantil_final_final <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 2-Source data 2.csv", header  = TRUE)

Quantil_final_final <- Quantil_final_final[, c(1:5, 10)]

# add data about the drug archeytpe
Quantil_final_final$Drug <- "Short-acting + Long-acting drugs"

# Define the break for the y axis
break_y <- c(1, 10, 20, 30, 39)
Label_yy <- c("Min", "", "", "", "Max")

#---- visualise three most important factor in the non seasonal setting ----

# select the data
Quantil_final_2 <- Quantil_final_final[Quantil_final_final$Seasonality == "sesonality1", ]
Quantil_final_2 <- Quantil_final_2[Quantil_final_2$G == "Access" | Quantil_final_2$G == "Resistance_Level_long" | Quantil_final_2$G == "Resistance_Level", ]


constant<-2
# visualise
PB <- ggplot(data = Quantil_final_2) +
  geom_line(aes(x = x, y = M, color = G), size = 2/constant) +
  geom_ribbon(aes(x = x, ymin = L, ymax = U, fill = G), alpha = 0.1) +
  facet_grid(. ~ Drug) +
  theme_bw() +
  scale_y_continuous(name = "Selection coefficient\n(resistance to the short-acting drug)") +
  scale_x_continuous(name = "Factor values", breaks = break_y, labels = Label_yy) +
  scale_color_manual(
    name = "Factors:",
    values = c(
      "#126429",
      "#709FCD", "#273871"),
    breaks = c("Access", "Resistance_Level", "Resistance_Level_long", "half_life_long", "Fitness", "eir", "half_life_short", "C_max_IC50"), 
    labels = c("Access to treatment (%)", "Degree of resistance to the short-acting drug", "Degree of resistance to the long-acting drug", "\nHalf-life\nof drug B", "Fitness cost", "EIR", "\nHalf-life\nof drug A", "Cmax/IC50\nof drug B")) +
  scale_fill_manual(
    name = "Factors:",
    values = c(
      "#117733",
      "#6699CC", "#332288"),
    breaks = c("Access", "Resistance_Level", "Resistance_Level_long", "half_life_long", "Fitness", "eir", "half_life_short", "C_max_IC50"), 
    labels = c("Access to treatment (%)", "Degree of resistance to the short-acting drug", "Degree of resistance to the long-acting drug", "\nHalf-life\nof drug B", "Fitness cost", "EIR", "\nHalf-life\nof drug A", "Cmax/IC50\nof drug B")) +
  theme(
    axis.text.x = element_text(size = 15/2),
    axis.text.y = element_text(size = 15/2),
    axis.title.x = element_text(size = 15/2, face = "bold"),
    axis.title.y = element_text(size = 15/2, face = "bold"),
    plot.title = element_text(size = 20/2, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 15/2)) +
  theme(legend.title = element_text(size = 15/2, face = "bold")) +
  theme(
    strip.text.x = element_text(size = 15/2, color = "black", face = "bold"),
    strip.text.y = element_text(size = 15/2, color = "black", face = "bold")) +
  # theme(legend.position = "bottom", legend.direction = "vertical")+
  theme(legend.key.size = unit(0.3, "cm"))

PB
PLOT<-plot_grid(PA, PB,
                ncol = 1, nrow = 2, rel_widths = c(1, 1), scale = 1, labels = c("A", "B"), label_size = 16/2)


ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 2.pdf",
       plot = PLOT, width = 14, height = 14, device="pdf", units = "cm", dpi = 300)
