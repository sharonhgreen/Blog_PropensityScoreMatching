################################################################################
# Propensity Score Methods for Causal Inference:
# Creating Data Visualizations to Assess Covariate Balance
# Berkeley D-Lab Blog Post
# Sharon H. Green
# 05/20/2024
################################################################################

#### Read in data and explore lalonde dataset ##################################
# Load the necessary libraries and inspect the data

library(MatchIt)
library(cobalt)

head(lalonde)

#### Prepare data ##############################################################

# Recode values to make figures easier to read later
lalonde$race <- recode(lalonde$race, black = "Black",  hispan = "Hispanic", 
                       white = "White")
table(lalonde$race)

lalonde$married <- recode(lalonde$married, "1" = "Married",  "0" = "Other")
table(lalonde$married)

var_names <- data.frame(old = c("age", "educ", "race_White", "race_Black",
                                "race_Hispanic", "married_Other", "nodegree",
                                "re74", "re75"),
                        new = c("Age (Years)", "Education (Years)", 
                                "Race (White)","Race (Black)","Race (Hispanic)", 
                                "Not Married", "No Degree","Earnings (1974)", 
                                "Earnings (1975)"))

#### Implement matching ########################################################

# Create a pre-matching object - can be used to assess initial imbalance in data
no_match <- matchit(treat ~ age + educ + race + married +
                      nodegree + re74 + re75, data = lalonde,
                    method = NULL, distance = "glm")

no_match

# Create a propensity score matching object - nearest neighbor without 
# replacement (1:1 ratio)
ps_match <- matchit(treat ~ age + educ + race + married +
                      nodegree + re74 + re75, data = lalonde,
                    method = "full", distance = "glm", link = "probit")

ps_match

#### Create data visualizations to assess covariate balance ####################

# Create covariate distribution plots

plot.age = bal.plot(ps_match, "age", which = "both",
                    sample.names = c("Unmatched", "Matched")) +
  scale_fill_discrete(labels = c("Control","Treatment"),
                      type = c("#003262","#FDB515")) +
  labs(title = "Age", x = "Age (Years)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 12))

plot.educ = bal.plot(ps_match, "educ", which = "both",
                     sample.names = c("Unmatched", "Matched")) +
  scale_fill_discrete(labels = c("Control","Treatment"),
                      type = c("#003262","#FDB515")) +
  labs(title = "Education", x = "Education (Years)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 12))

plot.race = bal.plot(ps_match, "race", which = "both",
                     sample.names = c("Unmatched", "Matched")) +
  scale_fill_discrete(labels = c("Control","Treatment"),
                      type = c("#003262","#FDB515")) +
  labs(title = "Race/Ethnicity", x = "Race/Ethnicity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 12))

plot.maritalstatus = bal.plot(ps_match, "married", which = "both",
                              sample.names = c("Unmatched", "Matched")) +
  scale_fill_discrete(labels = c("Control","Treatment"),
                      type = c("#003262","#FDB515")) +
  labs(title = "Marital Status", x = "Marital Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 12))

# Assemble distribution plots into one graphic

distributiongraphs <- (plot.age / plot.educ ) | (plot.race /plot.maritalstatus )

distributiongraphs <- distributiongraphs +
plot_annotation(title = "Distribution Balance in Unmatched and Matched Samples",
                  theme = theme(plot.title = element_text(size = 16, 
                                                          hjust = 0.5),
                                plot.title.position = "plot"))

distributiongraphs

# Create balance (Love) plots to assess quality of matches

loveplot.md <- love.plot(ps_match,
                         var.names = var_names,
                         drop.distance = TRUE,
                         var.order = "unadjusted",
                         abs = TRUE,  
                         line = TRUE,
                         stars = "raw",
                         shapes = c("circle filled", "circle"),
                         thresholds = c(m = .1),
                         stats = "mean.diffs",
                         sample.names = c("Unmatched", "Matched"),
                         title = NULL,
                         colors = c("#FDB515", "#003262")
)

loveplot.ks <- love.plot(ps_match,
                         var.names = var_names,
                         drop.distance = TRUE,
                         var.order = "unadjusted",
                         abs = TRUE,  
                         line = TRUE,
                         stars = "raw",
                         shapes = c("circle filled", "circle"),
                         thresholds = (ks = .05),
                         stats = "ks.statistics",
                         sample.names = c("Unmatched", "Matched"),
                         title = NULL,
                         colors = c("#FDB515", "#003262")
)  

# Assemble balance plots into one graphic

loveplot.md = loveplot.md + theme(
  legend.position = "bottom")

loveplot.ks = loveplot.ks + theme(
  legend.position = "none")

loveplots = loveplot.md + loveplot.ks +
  plot_annotation(title = "Covariate Balance in Matched and Unmatched Sample",
                  theme = theme(plot.title = element_text(size = 16, 
                                                          hjust = 0.5),
                                plot.title.position = "plot"))
loveplots
