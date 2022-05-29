##################################################################################
# Visualize the fit of the GP for the global sensitivity analysis (unconstrained)#
# of each  drug archetype                                                        #
#                                                                                #
# Input: Data set of the selection coefficients of the test data set estimated   #
#        using OpenMalaria and estimated using the emulator during the final     #
#        round of adaptive sampling of the GSA  of each  drug archetype          #                            #
#        (Data_precision_GSA_all.csv)                                            #
#                                                                                #
# Author: thiery Masserey (thiery.masserey@swisstph.ch)                          #
##################################################################################

# Load package
library("ggh4x")
library("ggplot2")

# Load the data
Precision_final <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Appendix 1-figure 6-Source data 1.csv", header  = TRUE)

# Transform variable drug as a factor
Precision_final$drug <- factor(Precision_final$drug, levels = c("A", "B", "A+B"))

# Estimate the correlation between true value and predicted value for each drug archetype
cors <- ddply(Precision_final, c("drug"), summarise, cor = round(cor(Test_True, Test_predicted), 3))

# Estimate the Root mean squared error between true value and predicted value
RMSEE <- ddply(Precision_final, c("drug"), summarise, cor = round(RMSE(Test_True, Test_predicted), 3)) # see RMSE function bellow

# Prepare the label for the plot
D.labs <- c("Short-acting drug", "Long-acting drug", "Short-acting +\nLong-acting drugs")
names(D.labs) <- c("A", "B", "A+B")

constant<-2
# Visualize the results
PLOT<-ggplot(data = Precision_final, aes(x = Test_True, y = Test_predicted)) +
  facet_grid( ~ drug, labeller = labeller(drug = D.labs)) +
  geom_point(size = 2.5/constant) +
  geom_smooth(method = lm,
              formula = y ~ x,
              se = FALSE) +
  theme_bw() +
  geom_text(
    data = cors,
    aes(label = paste("Cor = ", cor, sep = "")),
    x = 0.09,
    y = 0.55,
    size = 5/constant) +
  geom_text(
    data = RMSEE,
    aes(label = paste("RMSE = ", cor, sep = "")),
    x = 0.09,
    y = 0.45,
    size = 5/constant) +
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
  theme(plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
PLOT
# save
ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Appendix 1-figure 6.pdf",
       plot = PLOT, width = 14, height = 6.5, device="pdf", units = "cm", dpi = 300)

#--- Estimate the root mean square error ----
RMSE <- function(x, y) {
  # do a regression
  model_regression <- lm(y ~ x)
  
  # estimate the residuals
  model_summary <- summary(model_regression)
  
  # estimate the RMSE
  RMSE <- sqrt(mean((model_summary$residuals) ^ 2))
  
  # return the RMSE
  return(RMSE)
}

