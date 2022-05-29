##################################################################################
# Visualize the seasonality pattern                                              #
#                                                                                #
# Input: Monthly EIR value form Tanzania                                         #
# Author: thiery Masserey (thiery.masserey@swisstph.ch)                          #
##################################################################################



# Load the data
Data<-NULL
Data$time<-1:24/12
sesonality2 <- c(5.9, 2, 8.8, 26.5, 63.8, 133.1, 230.5, 335.4, 411.3, 346.6, 189.1, 45.4)
Data$sesonality2<-c(sesonality2,sesonality2)/5
Data<-as.data.frame(Data)

# Plot
PLOT<-ggplot(Data)+
  geom_line(aes(x=time, y=sesonality2),size=2/constant)+
  scale_y_continuous(name = "EIR \n(inoculations per person per year)", lim=c(0,100),expand = c(0, 0)) +
  scale_x_continuous(name = "Time", lim=c(1/12,13/12), breaks = c(1/12,2/12,3/12,4/12,5/12,6/12,7/12,8/12,9/12,10/12,11/12,12/12,13/12),labels=c("Jan.","","Mar.","","May","","Jul.","","Sept.","","Nov.","","Jan."),expand = c(0, 0)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size =  15/constant),
        axis.text.y = element_text(size =  15/constant),
        axis.title.x = element_text(size =  16/constant, face = "bold"),
        axis.title.y = element_text(size =  16/constant, face = "bold"))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(legend.position = "none")+
  expand_limits(x = 0, y = 0)

# Save
ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Appendix 1-figure 2.pdf",
       plot = PLOT, width = 10, height = 8, device="pdf", units = "cm", dpi = 300)
