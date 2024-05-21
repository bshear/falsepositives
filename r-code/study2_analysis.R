#----------------------------
## Analyze Study 2 Results ##
#----------------------------

#--------------
# To accompany:
# Shear, B. R., & Zumbo, B. D. (2013). False positives in multiple regression:
# Unanticipated consequences of measurement error in the predictor variables.
# Educational and Psychological Measurement, 73(5), 733–756. doi:10.1177/0013164413487738
#--------------

require(psych)
require(ggplot2)
source("r-code/regression_functions.R")

## Read in Data ---------------------------------------------------------------
study2.dat <- read.csv(file = "data/study2_simulation_results.csv", header = T)  ## Read in the raw data.
study2.dat$Type1Rate <- study2.dat$Type1_05/1000                                 ## Create a Type I error RATE variable

## Some descriptives ----------------------------------------------------------
describe(study2.dat)                                                            ## Descriptives (including of the IVs, for Table XXX in Paper)
hist(study2.dat$Type1Rate, col = "grey")                                        ## Interesting.
summary(study2.dat[ ,c(2:7,ncol(study2.dat))])                                  ## More descriptives for summary table.
apply(study2.dat[ ,c(2:7,ncol(study2.dat))], 2, sd)                             ## More descriptives.
sum(study2.dat$Type1Rate > 0.975)                                               ## How many cases of Type I error rates greater than 0.975?

## Nicer format table.
vars = c("RelX", "RelU", "RelY", "Corxu", "Rsq", "Sample.N", "Type1Rate", "mean_x.sr")
data.frame(vars,
           means = as.numeric(colMeans(study2.dat[ ,vars])),
           medians = (as.numeric(sapply(study2.dat[ ,vars], median))),
           sds = as.numeric(sapply(study2.dat[ ,vars], sd)),
           mins = as.numeric(sapply(study2.dat[ ,vars], min)),
           maxes = as.numeric(sapply(study2.dat[ ,vars], max))
)

sum(study2.dat$Type1Rate > 0.075) / 2000

## Linear Models -----------------------------------------------------
## Model 1 is main effects, Model 2 includes two-way interaction terms.
## All variables centered before fitting models.
rand.lm1 <- lm(Type1Rate ~ RelX + RelU + RelY + Corxu + Rsq + Sample.N,
               data = data.frame(cbind(Type1Rate = study2.dat$Type1Rate, scale(study2.dat[ ,2:7], center=T, scale=F))), x=T, y=T)
rand.lm2 <- lm(Type1Rate ~ (RelX + RelU + RelY + Corxu + Rsq + Sample.N)^2,
               data = data.frame(cbind(Type1Rate = study2.dat$Type1Rate, scale(study2.dat[ ,2:7], center=T, scale=F))), x=T, y=T)

## Model summaries:
summary(rand.lm1)
summary(rand.lm2)

## Using summary function that includes Pratt index values.
lmCustomSummary(rand.lm1)
lmCustomSummary(rand.lm2)

## Make plots ----------------------------------
## Create some factors, etc. for easier plotting.
study2.dat.gg <- study2.dat
study2.dat.gg$RelU <- cut(study2.dat.gg$RelU, breaks = 4)
study2.dat.gg$RelU <- factor(study2.dat.gg$RelU, labels = c("Rel(w) = 0.6 - 0.7", "Rel(w) = 0.7 - 0.8",
                                                            "Rel(w) = 0.8 - 0.9", "Rel(w) = 0.9 - 1.0"))
study2.dat.gg$CorxuFac <- cut(study2.dat.gg$Corxu, breaks = 3)
study2.dat.gg$CorxuFac <- factor(study2.dat.gg$CorxuFac, labels = c(1, 2, 3))

## Build the plot of the results with points and loess.
study2.gg <- ggplot(study2.dat.gg, aes(Corxu, Type1Rate)) +
  geom_point(size = 1.5, color = "black", alpha = 0.3) +
  geom_smooth(aes(lty = cut(Rsq, breaks = 3)), lwd = 1.5, alpha = 1, color = "black", se = F) +
  scale_linetype_discrete(name = "R-square", labels = c("0.0 - 0.25", "0.25 - 0.50", "0.50 - 0.75"),
                          guide = guide_legend(keywidth = 1.7)) +
  facet_wrap( ~ RelU, nrow = 1) +
  labs(x = "Correlation of Predictors", y = "Observed Type I Error Rate") +
  theme_bw() +
  theme(aspect.ratio = 1)

# plot of just loess slopes with no points; fit separately to each panel
# study2.gg.nopoints <- ggplot(study2.dat.gg, aes(Rsq, Type1Rate), coord_fixed()) +
#   geom_smooth(aes(lty = CorxuFac), lwd = 1.2, alpha = 1, color = "black", se = F, method = "loess") +
#   scale_linetype_manual(name = expression(bold(paste(Cor(tau[x], tau[w])))), labels = c("0.00-0.25   ", "0.25-0.50   ", "0.50-0.75  "),
#                         values = c("1" = 1, "2" = 2, "3" = 3),
#                         guide = guide_legend(label.position = "right", label.hjust = 0.5,
#                                              title.vjust = 0.5, title.position = "top", title.hjust = 0.5)) +
#   facet_wrap( ~ RelU, nrow = 1) +
#   labs(x = "True Score R-Squared", y = "Observed Type I Error Rate") +
#   theme_bw() + theme(aspect.ratio = 1) +
#   theme(legend.position = "bottom", legend.key.width = unit(1.3, "cm"), legend.background = element_rect(color = "black"))

study2.gg.nopoints <- ggplot(study2.dat.gg, aes(Rsq, Type1Rate), coord_fixed()) +
  geom_smooth(aes(lty = CorxuFac), lwd = 1.2, alpha = 1, color = "black", se = F, method = "loess") +
  scale_linetype_manual(name = expression(bold(paste(Cor(tau[x], tau[w]), ":  ", sep = ""))), labels = c(" 0.00 - 0.25  ", "  0.25 - 0.50  ", "  0.50 - 0.75 "),
                        values = c("1" = 1, "2" = 2, "3" = 3),
                        guide = guide_legend(label.position = "bottom", label.hjust = 0.5,
                                             title.vjust = 0.5, title.position = "left", title.hjust = 0.5)) +
  facet_wrap( ~ RelU, nrow = 1) +
  labs(x = "True Score R-Squared", y = "Observed Type I Error Rate") +
  theme_bw() + theme(aspect.ratio = 1) +
  theme(legend.position = "bottom", legend.background = element_rect(color = "black"))

## Save plots to PDF file.
ggsave(study2.gg.nopoints, filename = "results/study2_plot_nopoints.pdf",
       width = 12, height = 5, units = "in")
ggsave(study2.gg, filename = "results/study2_plot_withpoints.pdf",
       width = 12, height = 5, units = "in")



