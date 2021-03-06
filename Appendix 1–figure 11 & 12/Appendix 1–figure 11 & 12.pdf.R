##################################################################################
# Visualize the fit of the GP for the global sensitivity analysis (constrained)  #
# of  drug A+B                                                                   #
#                                                                                #
# Input: Data set of the selection coefficients of the test data set estimated   #
#        using OpenMalaria and estimated using the emulator during the final     #
#        round of adaptive sampling of the constrained GSA drug A+B              #                            #
#        (Data_precision_CSA_ACT.csv)                                            #
#                                                                                #
# Author: thiery Masserey (thiery.masserey@swisstph.ch)                          #
##################################################################################

# Load package
library("ggh4x")
library("ggplot2")
library("plyr")

# Load the data
Precision_final <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Appendix 1-figure 11&12-Source data 1.csv", header  = TRUE)

# Define constrained variable as a factor
Precision_final$resistance_level <- factor(Precision_final$resistance_level, levels = c("7", "18"))
Precision_final$access <- factor(Precision_final$access, levels = c("0.04", "0.5"))
Precision_final$eir <- factor(Precision_final$eir, levels = c("5", "10", "500"))
Precision_final$seasonality <- factor(Precision_final$seasonality, levels = c("sesonality1", "sesonality2"))
Precision_final$dosage <- factor(Precision_final$dosage, levels = c("0", "1"))

# Creat labels for each constrain factor
T.labs <- c("Treatment access = 0.04%", "Treatment access = 0.5%")
names(T.labs) <- c("0.04", "0.5")
R.labs <- c("Degree of resistance = 18", "Degree of resistance = 7")
names(R.labs) <- c("18", "7")
E.labs <- c("EIR = 5", "EIR = 10", "EIR = 500")
names(E.labs) <- c("5", "10", "500")
D.labs <- c("Adherence to treatment = 100%", "Adherence to treatment 67%")
names(D.labs) <- c("0", "1")
S.labs <- c("No seasonality", "Seasonality")
names(S.labs) <- c("sesonality1", "sesonality2")
D2.labs <- c("", "")
names(D2.labs) <- c("0", "1")
S2.labs <- c("", "")
names(S2.labs) <- c("sesonality1", "sesonality2")

# select the last round of adaptative sampling
Precision_final_2 <- Precision_final[Precision_final$iteration == 5, ]

# ---- Low acess to treatment (Supplementary figure 11) ----

# select the data
Precision_final_3 <- Precision_final_2[Precision_final_2$access == 0.04, ]
Precision_final_4 <- Precision_final_3

# estimate the corelaiton and root mean squared error in each arm
cors <- ddply(Precision_final_4, c("seasonality", "dosage", "eir", "resistance_level"), summarise, cor = round(cor(Test_True, Test_predicted), 3))
RMSEE <- ddply(Precision_final_4, c("seasonality", "dosage", "eir", "resistance_level"), summarise, cor = round(RMSE(Test_True, Test_predicted), 3))

# visualise
PLOT<-ggplot(data = Precision_final_4, aes(x = Test_True, y = Test_predicted)) +
  facet_nested(resistance_level + eir ~ dosage + seasonality, labeller = labeller(seasonality = S.labs, dosage = D.labs, eir = E.labs, resistance_level = R.labs)) +
  geom_point(size = 2.5/constant) +
  theme_bw() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  geom_text(data = cors, aes(label = paste("Cor=", cor, sep = "")), x = -0.055, y = 0.03, size = 4.5/constant) +
  geom_text(data = RMSEE, aes(label = paste("RMSE=", cor, sep = "")), x = -0.055, y = 0.01, size = 4.5/constant) +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold")) +
  ylab("Predicted selection coefficient") +
  xlab("Observed selection coefficient") +
  ggtitle("Short-acting + Long-acting drugs") +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold"))

# save
ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Appendix 1–figure 11.pdf",
       plot = PLOT, width = 14, height = 18, device="pdf", units = "cm", dpi = 300)

# ---- look at high level of treatment access (Supplementary figure 12) ----

# select the data 
Precision_final_3 <- Precision_final_2[Precision_final_2$access == 0.5, ]

# start by low level of resistance
Precision_final_5 <- Precision_final_3[Precision_final_3$resistance_level == 7, ]

# #estimate the corelation and the root mean squared error
cors <- ddply(Precision_final_5, c("seasonality", "dosage", "eir", "resistance_level"), summarise, cor = round(cor(Test_True, Test_predicted), 3))
RMSEE <- ddply(Precision_final_5, c("seasonality", "dosage", "eir", "resistance_level"), summarise, cor = round(RMSE(Test_True, Test_predicted), 3))

constant<-2
## visualise
pp1 <- ggplot(data = Precision_final_5, aes(x = Test_True, y = Test_predicted)) +
  facet_nested(resistance_level + eir ~ dosage + seasonality, labeller = labeller(seasonality = S.labs, dosage = D.labs, eir = E.labs, resistance_level = R.labs)) +
  geom_point(size = 2.5/constant) +
  theme_bw() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  geom_text(data = cors, aes(label = paste("Cor=", cor, sep = "")), x = 0.1, y = 0.4, size = 4.5/constant) +
  geom_text(data = RMSEE, aes(label = paste("RMSE=", cor, sep = "")), x = 0.1, y = 0.33, size = 4.5/constant) +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold")) +
  ylab("Predicted selection coefficient") +
  xlab("") +
  ggtitle("Short-acting + Long-acting drugs") +
  theme(
    strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(5.5, 5.5, -10.5, 5.5), "pt"))

# look at high level of treatment access and high level of resistance
Precision_final_6 <- Precision_final_3[Precision_final_3$resistance_level == 18, ]

## estimate corelation and RMSE
cors <- ddply(Precision_final_6, c("seasonality", "dosage", "eir", "resistance_level"), summarise, cor = round(cor(Test_True, Test_predicted), 3))
RMSEE <- ddply(Precision_final_6, c("seasonality", "dosage", "eir", "resistance_level"), summarise, cor = round(RMSE(Test_True, Test_predicted), 3))

## visualise
pp2 <- ggplot(data = Precision_final_6, aes(x = Test_True, y = Test_predicted)) +
  facet_nested(resistance_level + eir ~ dosage + seasonality, labeller = labeller(seasonality = S2.labs, dosage = D2.labs, eir = E.labs, resistance_level = R.labs)) +
  geom_point(size = 2.5/constant) +
  theme_bw() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  geom_text(data = cors, aes(label = paste("Cor=", cor, sep = "")), y = 0.45, x = 0.1, size = 4.5/constant) +
  geom_text(data = RMSEE, aes(label = paste("RMSE=", cor, sep = "")), y = 0.37, x = 0.1, size = 4.5/constant) +
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

PLOT<-plot_grid(pp1, pp2, ncol = 1, nrow = 2, rel_widths = c(1, 1), scale = 1)

ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Appendix 1-figure 12.pdf",
       plot = PLOT, width = 14, height = 19.5, device="pdf", units = "cm", dpi = 300)

#--------------------------------------------------
# Function to estimate the Root mean squared error.
# --------------------------------------------------
RMSE <- function(x, y) {
  # do a liner regression
  model_regression <- lm(y ~ x)
  
  # estimate the residuals
  model_summary <- summary(model_regression)
  
  # estimate the Root mean squared error
  RMSE <- sqrt(mean((model_summary$residuals)^2))
  
  # return RMSE
  return(RMSE)
}
