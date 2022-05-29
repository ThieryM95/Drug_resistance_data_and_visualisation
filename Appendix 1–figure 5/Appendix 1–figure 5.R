#####################################################################################
# Illustration of the moving average                                                #
#                                                                                   #
# Input: Output of one simulation from OpenMalaria                                  #
# Author: thiery Masserey (thiery.masserey@swisstph.ch)                             #
#####################################################################################


# Load the data
###############

Output_data <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Appendix 1-figure 5-Source data 1.csv", header  = TRUE)

# Estimate the moving average
#############################

# Define the number of time step
time_step <- 30

# Define the number of survey per generation
Number_survey_generation <- 60 / time_step # 1 generation equal 60 days

# Define the number of survey per years
Number_survey_years <- 365 / time_step

# define the time at which drug resistance is introduce
Time_HS_Change <- round(30 * Number_survey_years)

# Define time at which regression will start (when drug concentration of the previous drug is equal to zero + 1 parasite generations)
time_start <- Output_data$Survey[Output_data$nHostDrugConcNonZero_2 == 0]
time_start <- time_start[time_start >= (Time_HS_Change + Number_survey_generation)][1]

# Define time at which regression will stop (2 years after the beginning of the regression + 6 more for the moving average)
time_end <- time_start + 2 * Number_survey_years + 6

# select the data within this boundary
time_spread <- Output_data$Survey[Output_data$Survey > time_start & Output_data$Survey < time_end] / Number_survey_years # time spread is converted in years
Measurment_R <- Output_data$Inoculation_R[Output_data$Survey > time_start & Output_data$Survey < time_end]

# estimate the moving average (over 1 year period) of the frequency of the resistant genotype
Measurment_MA <- 0
time_spread_MA <- 0
for (i in (1 + 6):(24)) {
  Measurment_MA[i - 6] <- mean(Measurment_R[(i - 6):(i + 6)])
  time_spread_MA[i - 6] <- time_spread[i]
  
}


Data<-NULL
Data$time<-time_spread
Data$Measurment_R<-Measurment_R
Data<-as.data.frame(Data)

Data_2<-NULL
Data_2$time_spread_MA<-time_spread_MA
Data_2$Measurment_MA<-Measurment_MA
Data_2<-as.data.frame(Data_2)

# Plot the data
###############

constant<-2
PLOT<-ggplot(data = Data, aes(x = time_spread-30, y = Measurment_R)) +
  geom_point(size = 2.5/constant)+
  geom_point(data=Data_2, aes(x = time_spread_MA-30, y = Measurment_MA), color="blue", size = 2.5/constant)+
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15/constant),
    axis.text.y = element_text(size = 15/constant),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold")) +
  ylab("The logit of the relative frequency of\n the resistant genotype in inoculations") +
  ylim(0.5, 1) +
  xlab("Time since the end of the burn-in phase (years)") +
  theme(strip.text.x = element_text(size = 16/constant, color = "black", face = "bold"),
        strip.text.y = element_text(size = 16/constant, color = "black", face = "bold"))+
  theme(legend.position="none")


ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Appendix 1-figure 5.pdf",
       plot = PLOT, width = 10,height = 8, device="pdf", units = "cm", dpi = 300)

