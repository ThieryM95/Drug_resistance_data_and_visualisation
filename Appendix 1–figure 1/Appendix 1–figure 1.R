#############################################################################
# Predict the time for the genotype resistant the artemisinie when used in  #
# ACT to reach a specific frequency                                         #                                                                          #
#                                                                           #
# author: thiery.masserey@swisstph.ch                                       #
#############################################################################

# Load package
library("ggh4x")
library("ggplot2")

# Load data
X <- read.csv(file = "/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Appendix 1-figure 1-Source data 1.csv", header  = TRUE)

# Define the label
EIR_LAB <- c("Low transmission intensity", "Low transmission intensity", "High transmission intensity")
names(EIR_LAB) <- c("5", "100", "500")

# Define the color scale
Resistance_Level_long <- seq(1.5, 13.5, length.out = 7)
colours <- scales::seq_gradient_pal(low = "#6CC4D9", high = "#0A237C", space = "Lab")(1:7 / 7)
colours <- scales::seq_gradient_pal(low = "lightblue", high = "darkblue", space = "Lab")(1:7 / 7)
values_2 <- setNames(colours, Resistance_Level_long)
Resistance_Level_short <- seq(1.5, 13.5, length.out = 7)
values <- setNames(colours, Resistance_Level_short)


# ---- Visualise for setting with a low EIR ----

# select the data
X2 <- X[X$eir == 5, ]

# Define the variable that need to be a factor
X2$Resistance_Level_short <- as.factor(X2$Resistance_Level_short)
X2$eir <- as.factor(X2$eir)

# Define that if needed more time than 60 year, we fixed the maximum to 60 year (for plot visualisation pupuse) 
X2 <- X[X$eir == 5, ]
X2$Resistance_Level_short <- as.factor(X2$Resistance_Level_short)
X2$eir <- as.factor(X2$eir)
X2$time_0.1[X2$time_0.1 >= 60] <- 60
X2$time_0.1[X2$time_0.1 <= 0] <- 60
constant<-2
PA <- ggplot(data = X2, aes(
  x = Resistance_Level_long,
  y = time_0.1,
  color = (Resistance_Level_short))) +
  geom_line(size = 1.5/constant) +
  facet_grid(~eir, labeller = labeller(eir = EIR_LAB)) +
  xlab("") +
  theme_bw() +
  #scale_y_continuous("T25 (years)", lim = c(0, 30), sec.axis = sec_axis(~ exp(a + . * b + d * .), breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1), name = "Probability of establishment")) +
  #scale_y_continuous("T25 (years)", lim = c(0, 60), sec.axis = sec_axis(~ (log(25/(100-25))-log(1/(100-1)))/./6*0.6639313+0.02354293, breaks =  c(0.037,0.05,0.1,0.5), name = "Probability of establishment")) +
  scale_y_continuous("T25 (years)", lim = c(0, 60), sec.axis = sec_axis(~ (log(25/(100-25))-log(1/(100-1)))/./6*0.9674891+0.002346184, breaks =  c(0.0125,0.025,0.05,0.1,0.5), name = "Probability of establishment")) +
  theme(
    legend.text = element_text(size = 16/constant),
    legend.title = element_text(size = 16/constant, color = "black", face = "bold"),
    axis.text.x = element_text(size = 16/constant, color = "black"),
    axis.text.y = element_text(size = 16/constant, color = "black", margin = margin(t = 0, r = 0, b = 0, l = 4)),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    axis.ticks.y.right = element_line(color = "gray30"),
    axis.title.y.right = element_text(size = 16/constant, color = "gray30", face = "bold"),
    axis.text.y.right = element_text(size = 14/constant, color = "gray30", margin = margin(t = 0, r = 4, b = 0, l = 0))) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5)/constant, "cm")) +
  theme(strip.text.x = element_text(size = 16/constant, color = "black", face = "bold")) +
  theme(legend.position = "none") +
  scale_colour_manual(values = values)

X2$PE<-(log(25/(100-25))-log(1/(100-1)))/X2$time_0.1/6*0.9674891+0.002346184

PA
# Visualize for setting with a high EIR
X3 <- X[X$eir == 500, ]
X3$Resistance_Level_short <- as.factor(X3$Resistance_Level_short)
X3$eir <- as.factor(X3$eir)
X3$time_0.1[X3$time_0.1 >= 60] <- 60
X3$time_0.1[X3$time_0.1 <= 0] <- 60
PB <- ggplot(data = X3, aes(
  x = Resistance_Level_long,
  y = time_0.1,
  col = (Resistance_Level_short))) +
  geom_line(size = 1.5/constant) +
  facet_grid(~eir, labeller = labeller(eir = EIR_LAB)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  #scale_y_continuous("T25 (years)", lim = c(0, 30), sec.axis = sec_axis(~ exp(a_2 + . * b_2 + d_2 * .), breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1), name = "Probability of establishment")) +
  #scale_y_continuous("T25 (years)", lim = c(0, 60), sec.axis = sec_axis(~ (log(25/(100-25))-log(1/(100-1)))/./6*0.6027698 +0.01987749 ,breaks = c(0.032,0.05,0.1,0.5), name = "Probability of establishment")) +
  scale_y_continuous("T25 (years)", lim = c(0, 60), sec.axis = sec_axis(~ (log(25/(100-25))-log(1/(100-1)))/./6*0.874511+0.00154079  ,breaks = c(0.0125,0.025,0.05,0.1,0.5), name = "Probability of establishment")) +
  theme(
    legend.text = element_text(size = 16/constant),
    legend.title = element_text(size = 16/constant, color = "black", face = "bold"),
    axis.text.x = element_text(size = 16/constant, color = "black"),
    axis.text.y = element_text(size = 16/constant, color = "black", margin = margin(t = 0, r = 0, b = 0, l = 4)),
    axis.title.x = element_text(size = 16/constant, face = "bold"),
    axis.title.y = element_text(size = 16/constant, face = "bold"),
    # axis.line.y.right = element_line(color = "red"),
    axis.ticks.y.right = element_line(color = "gray30"),
    axis.title.y.right = element_text(size = 16/constant, color = "gray30", face = "bold"),
    axis.text.y.right = element_text(size = 14/constant, color = "gray30", margin = margin(t = 0, r = 4, b = 0, l = 0))) +
  theme(strip.text.x = element_text(size = 16/constant, color = "black", face = "bold")) +
  labs(col = "Degree of resistance to the\n short-acting drug:") +
  theme(plot.margin = unit(c(0.5, 0, 0.5, 0.5)/constant, "cm")) +
  theme(legend.position = "right", legend.direction = "vertical") +
  guides(fill = guide_legend(ncol = 2)) +
  scale_colour_manual(values = values)

PB

X3$PE<-(log(25/(100-25))-log(1/(100-1)))/X3$time_0.1/6*0.874511+0.00154079 
# Merge the plot
PLOT <- plot_grid(PA, PB, ncol = 2, nrow = 1, rel_widths = c(1, 1.7), scale = 1)
PLOT <- ggdraw(add_sub(PLOT, "Degree of resistance to the long-acting drug", vpadding = grid::unit(0, "lines"), y = 6, x = 0.4, vjust = 4.5, size = 16/constant, fontface = "bold"))


ggsave("/scicore/home/penny/masthi00/WF_spread/Visulaise_results/Paper/Appendix 1-figure 1.pdf",
       plot = PLOT, width = 18, height = 6, device="pdf", units = "cm", dpi = 300)

