
# load libraries
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(grid)

# SET UP DATA FRAME -------------------------------------------------------

psyc = read.csv("Psych_ALL_YEARS.csv")
colnames(psyc)[2] <- "year"
psyc$year <- as.factor(as.character(psyc$year))# convert year column to factor

#labels for VALUE criteria
criteria <- c("Explanation\nof Issues",  "Influence of\nContext and\nAssumptions", "Student's\nPosition", "Conclusions", "Define\nProblem", "Identify\nStrategies","Propose\nSolutions", "Evaluate\nPotential\nSolutions", "Context and\nPurpose for\nWriting", "Content\nDevelopment", "Genre and\nDisciplinary\nConventions",  "Syntax and\nMechanics")

# create column to define colours based on value criteria
define_colour <- data.frame(define_colour = c('a','b','c','a','b','c', 'a','b','c','a','b','c','a','b','c','d','e','f','d','e','f','d','e','f','d','e','f', 'g','h','j', 'g','h','j', 'g','h','j', 'g','h','j', 'g','h','j')) # a,b,c = greens; d,e,f = oranges ; g,h,j = blues

psyc <- bind_cols(psyc, define_colour)

psyc$define_colour <- factor(psyc$define_colour, levels = c("a", "d", "g", "b","e","h","c","f", "j"))

# LEGEND LABELS -----------------------------------------------------------

a_1 <- "First Year  "
a_2 <- paste0("n = ", psyc[1, "N"], "  ")

b_1 <- "Second Year  "
b_2 <- paste0("n = ", psyc[2, "N"], "  ")

c_1 <- "Fourth Year  "
c_2 <- paste0("n = ", psyc[3, "N"], "  ")


legend_labels <- c(a_1, a_2,"", b_1, b_2, "", c_1, c_2, "")

# CREATE GRAPH --------------------------------------------
dodge <- position_dodge(width=0.5) # because bars and error bars will be different widths

ggplot(
  data = psyc, 
  aes(x = X, y = Mean, fill = define_colour)
)+
  geom_bar(stat = "identity",position = dodge, width = 0.5) + 
  geom_errorbar(
    aes(ymax = Mean + Std..Error, ymin = Mean - Std..Error),
    position = dodge, width = 0.25
  ) + 
  coord_cartesian(ylim = c(0, 4.4)) +
  scale_x_discrete(labels = criteria) +
  theme(
    axis.line = element_line("grey"), 
    panel.grid.major.y = element_line("grey"),
    panel.grid.major.x = element_blank(), # remove vertical white lines
    panel.background = element_rect("white"),
    plot.title = element_text(size = 15),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12), #size of x axis labels
    legend.position = "bottom",#c(0.8,0.88), # position legend on graph in top right corner
    legend.background = element_rect(size=0.5, linetype="solid", colour ="grey"), #border around legend
    legend.key.height = unit(0.15, "inches"),
    legend.text.align = 0.5, # center text
    legend.title.align = 0.5 # center title
    ) +
  guides(fill=guide_legend(ncol=3)) +# multiple columns in legend
  labs(title = "Summary of VALUE Learning Outcomes", 
    x = "Learning Outcome", y = "Average Rubric Level"
    ) +
  scale_fill_manual(
    values = c('#99e6a2', '#ffc499','#99c4f6', '#33cc44', '#ff8833','#3388ee', '#1f7a29','#cc5200' ,'#1f528f'),
    name = "Legend",
    labels = legend_labels
  ) +
  annotate( # add labels for CLA mastery levels
    "text", 
    fontface = "bold", 
    size = 5,
    x = c(3, 7.5, 12), y = 4.2, 
    label = c( "Critical Thinking","Problem Solving" ,"Written Communication"), 
    colour = c("#33CC44","#FF8833", "#3388EE")
  )


