#####################################################################################
# Visualize the logit of the relative frequency of the resistant genotype over time #                                           
# when the initial relative frequency of infected humans carrying the resistant     #
# genotype was 5%                                                                   #
#                                                                                   #
# Input: Monthly EIR value form Tanzania                                            #
# Author: thiery Masserey (thiery.masserey@swisstph.ch)                             #
#####################################################################################


#Output <- list.files("/scicore/home/penny/masthi00/OUT_Parameterisation_spread/Low_initial_resistance_frequency/2_sim_outputs/processed/sample_0/")
#Output_data_name <- Output[10]
#Output_data_file_path <- file.path("/scicore/home/penny/masthi00/OUT_Parameterisation_spread/Low_initial_resistance_frequency/2_sim_outputs/processed/sample_0/", Output_data_name)
#Output_data <- read.table(Output_data_file_path, sep = ";")

#Load the data
Output_data <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Appendix 1-figure 3-Source data 1.csv", header  = TRUE)

# Estimate logit
Output_data$P<-Output_data$nInfectByGenotype_2/(Output_data$nInfectByGenotype+Output_data$nInfectByGenotype_2)
Output_data$logit_P<-log(Output_data$P/(1-Output_data$P))

# Plot
constant<-2
PLOT<-ggplot(Output_data)+
  geom_line(aes(x=Survey/12.167-30, y=logit_P),size=2/constant)+
  scale_y_continuous(name = "The logit of the relative frequency of\n the resistant genotype in inoculations") +
  scale_x_continuous(name = "Time since the end of the burn-in phase (years)", lim=c(0,10)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size =  15/constant),
        axis.text.y = element_text(size =  15/constant),
        axis.title.x = element_text(size =  16/constant, face = "bold"),
        axis.title.y = element_text(size =  16/constant, face = "bold"))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(legend.position = "none")+
  expand_limits(x = 0, y = 0)

# Save
ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Appendix 1-figure 3.pdf",
       plot = PLOT, width = 10, height = 8, device="pdf", units = "cm", dpi = 300)
