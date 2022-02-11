##################################################################################
# Visualize the fit of the GP for the global sensitivity analysis (constrained)  #
# of  drug B                                                                     #
#                                                                                #
# Input: Data set of the selection coefficients of the test data set estimated   #
#        using OpenMalaria and estimated using the emulator during the final     #
#        round of adaptive sampling of the constrained GSA drug B                #                            #
#        (Data_precision_CSA_long.csv)                                           #
#                                                                                #
# Author: thiery Masserey (thiery.masserey@swisstph.ch)                          #
##################################################################################

# Load package
library("ggh4x")
library("ggplot2")
library("plyr")

# load the data
Precision_final <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Data_precision_CSA_long.csv", header  = TRUE)

# Define constrained variable as a factor
Precision_final$resistance_level <- factor(Precision_final$resistance_level, levels = c("2.5", "10"))
Precision_final$access <- factor(Precision_final$access, levels = c("0.04", "0.5"))
Precision_final$eir <- factor(Precision_final$eir, levels = c("5", "10", "500"))

# creat labels for each constrain factor
T.labs <- c("Treatment access = 0.04", "Treatment access = 0.5")
names(T.labs) <- c("0.04", "0.5")
R.labs <- c("Degree of resistance = 10", "Degree of resistance = 2.5")
names(R.labs) <- c("10", "2.5")
E.labs <- c("EIR = 5", "EIR = 10", "EIR = 500")
names(E.labs) <- c("5", "10", "500")
D.labs <- c("Adherence to treatment = 100%", "Adherence to treatment = 67%")
names(D.labs) <- c("0", "1")
S.labs <- c("No seasonality", "Seasonality")
names(S.labs) <- c("sesonality1", "sesonality2")
D2.labs <- c("", "")
names(D2.labs) <- c("0", "1")
S2.labs <- c("", "")
names(S2.labs) <- c("sesonality1", "sesonality2")

# Select the data of the last round of adaptative sampling 
Precision_final_2 <- Precision_final[Precision_final$iteration == 4, ]

# ---- Look at low level of treatment access (Supplementary figure 8)----

# select the data
Precision_final_3 <- Precision_final_2[Precision_final_2$access == 0.04, ]
Precision_final_4 <- Precision_final_3 

# estimate correlation and root mean squared error in each arm
cors <- ddply(Precision_final_4, c("seasonality", "dosage", "eir", "resistance_level"), summarise, cor = round(cor(Test_True, Test_predicted), 3))
RMSEE <- ddply(Precision_final_4, c("seasonality", "dosage", "eir", "resistance_level"), summarise, cor = round(RMSE(Test_True, Test_predicted), 3))

# visualise
constant<-2
PLOT<-ggplot(data = Precision_final_4, aes(x = Test_True, y = Test_predicted)) +
  facet_nested(resistance_level + eir ~ dosage + seasonality, labeller = labeller(seasonality = S.labs, dosage = D.labs, eir = E.labs, resistance_level = R.labs)) +
  geom_point(size = 2.5/constant) +
  theme_bw() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  geom_text(data = cors, aes(label = paste("Cor=", cor, sep = "")), x = -0.04, y = 0.05, size = 4.5/constant) +
  geom_text(data = RMSEE, aes(label = paste("RMSE=", cor, sep = "")), x = -0.04, y = 0.03, size = 4.5/constant) +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold")) +
  ylab("Predicted selection coefficient") +
  ylim(-0.08, 0.08) +
  xlab("Observed selection coefficient") +
  ggtitle("Drug B") +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold"))

# save
ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Supplementary file 1–figure 10.pdf",
       plot = PLOT, width = 15, height = 19, device="pdf", units = "cm", dpi = 300)


# ---- Look at low level of treatment access (Supplementary figure 9)----

# select the data
Precision_final_3 <- Precision_final_2[Precision_final_2$access == 0.5, ]

# start with estimation when the level of resistance was low
Precision_final_5 <- Precision_final_3[Precision_final_3$resistance_level == 2.5, ]

## estimate corelation and RMSE
cors <- ddply(Precision_final_5, c("seasonality", "dosage", "eir", "resistance_level"), summarise, cor = round(cor(Test_True, Test_predicted), 3))
RMSEE <- ddply(Precision_final_5, c("seasonality", "dosage", "eir", "resistance_level"), summarise, cor = round(RMSE(Test_True, Test_predicted), 3))

## visualise
pp1 <- ggplot(data = Precision_final_5, aes(x = Test_True, y = Test_predicted)) +
  facet_nested(resistance_level + eir ~ dosage + seasonality, labeller = labeller(seasonality = S.labs, dosage = D.labs, eir = E.labs, resistance_level = R.labs)) +
  geom_point(size = 2.5/constant) +
  theme_bw() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  geom_text(data = cors, aes(label = paste("Cor=", cor, sep = "")), x = 0.1, y = 0.30, size = 4.5/constant) +
  geom_text(data = RMSEE, aes(label = paste("RMSE=", cor, sep = "")), x = 0.1, y = 0.25, size = 4.5/constant) +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold")) +
  ylab("Predicted selection coefficient") +
  xlab("") +
  ggtitle("Drug B") +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(5.5, 5.5, -10.5, 5.5), "pt"))

# Then with estimation when the level of resistance was high 
Precision_final_6 <- Precision_final_3[Precision_final_3$resistance_level == 10, ]

## estimate corelaiton and RMSE
cors <- ddply(Precision_final_6, c("seasonality", "dosage", "eir", "resistance_level"), summarise, cor = round(cor(Test_True, Test_predicted), 3))
RMSEE <- ddply(Precision_final_6, c("seasonality", "dosage", "eir", "resistance_level"), summarise, cor = round(RMSE(Test_True, Test_predicted), 3))

## visualise
pp2 <- ggplot(data = Precision_final_6, aes(x = Test_True, y = Test_predicted)) +
  facet_nested(resistance_level + eir ~ dosage + seasonality, labeller = labeller(seasonality = S2.labs, dosage = D2.labs, eir = E.labs, resistance_level = R.labs)) +
  geom_point(size = 2.5/constant) +
  theme_bw() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  geom_text(data = cors, aes(label = paste("Cor=", cor, sep = "")), y = 0.64, x = 0.32, size = 4.5/constant) +
  geom_text(data = RMSEE, aes(label = paste("RMSE=", cor, sep = "")), y = 0.56, x = 0.32, size = 4.5/constant) +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold")) +
  ylab("Predicted selection coefficient") +
  xlab("Observed selection coefficient") +
  ggtitle("") +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(strip.background.x = element_rect(fill = "white", colour = "white")) +
  theme(plot.margin = unit(c(-10.5, 5.5, 5.5, 5.5), "pt"))

# visualise all plot
PLOT<-plot_grid(pp1, pp2, ncol = 1, nrow = 2, rel_widths = c(1, 1), scale = 1)

ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Supplementary file 1–figure 10.pdf",
       plot = PLOT, width = 15, height = 20, device="pdf", units = "cm", dpi = 300)
