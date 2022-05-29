##################################################################################
# Visualize the relation betwen EIR and the proportion of treated infection      #
# and treated individual                                                         #
#                                                                                #
#                                                                                #
# Author: thiery Masserey (thiery.masserey@swisstph.ch)                          #
##################################################################################



# Load the data
Scenario_liste<-read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3-figure supplement 8-Source data 1.csv", header=T)
Scenario_liste_2<-Scenario_liste[Scenario_liste$Access==0.5 & Scenario_liste$seed==2 & Scenario_liste$eir<=400,]

Scenario_liste_3<-NULL
Scenario_liste_3$eir<-c(Scenario_liste_2$eir,Scenario_liste_2$eir)
Scenario_liste_3$Percentage<-c(Scenario_liste_2$Treated_Total,Scenario_liste_2$Treated_infectTotal)
Scenario_liste_3$Total<-c(rep("Person", length(Scenario_liste_2$eir)),rep("infection", length(Scenario_liste_2$eir)))
Scenario_liste_3<-as.data.frame(Scenario_liste_3)

#Plot
constant<-2
PLOT<-ggplot(Scenario_liste_3,aes(x=eir, y=Percentage*100, col=Total))+
  geom_point(size=3/constant)+
  ylim(0,30)+
  ylab("Percentage (%)")+
  xlab("EIR (inoculation per person per year)")+
  scale_color_manual(name  ="Percentage of:",
                     values=c(SteppedSequential5Steps[9],SteppedSequential5Steps[17]),
                     breaks=c("person", "infection"),
                     labels=c("People that received treatment", "Infected people that received treatment"))+
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 16/constant),
    axis.text.y = element_text(size = 16/constant),
    axis.title.x = element_text(size = 18/constant, face = "bold"),
    axis.title.y = element_text(size = 18/constant, face = "bold")) +
  theme(legend.text = element_text(size = 18/constant)) +
  theme(legend.title = element_text(size = 18/constant, face = "bold")) +
  theme(legend.key.size = unit(0.9, "cm")) +
  theme(legend.key.width = unit(1.0, "cm"))+ 
  theme(legend.position = c(0.5, 0.8))


ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Figure 3-figure supplement 8.pdf",
       plot = PLOT, width = 10, height = 9, device="pdf", units = "cm", dpi = 300)