##Code for Bayesian Analysis
library(dplyr)
library(optpart)
library(reshape2)
data(shoshveg)
## 'JUNCOM' - Common Juniper
## 'ABILAS' - Subalpine Fir
## 'ARTFRI' - Fringed Sagebrush
## 'ARTNOV' - Black Sagebrush
shosh.binary <- shoshveg %>% dplyr::select('JUNCOM', 'ABILAS', 'ARTFRI', 'ARTNOV') %>%
  melt() %>% mutate(present = as.numeric(value > 0), plot = rep(1:150,4)) %>%
  dplyr::select(species = variable, present, plot)

#calculates the mean coverage
shosh.binary %>% group_by(species) %>% summarise(plot = mean(present))%>% 
  ggplot(., aes(species, plot)) + geom_point(size = 4, color = "green3")  + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Species") + ylab("Mean Cover Proportion")+
  ggtitle("Coverage Proportions by Tree Type")

#If we were to go with just a beta-binomial model

#prior is beta distribution(alpha, beta) (if beta, alpha are 1,1 we have uniform prior)
alpha = 1
beta = 1
N <- length(shosh.binary$species)
y <- sum(shosh.binary$present)
set.seed(123)
posterior_sample <- rbeta(100, y + alpha, N + beta - y) #note we need a posterior predictive
qbeta(c(0.025, 0.975), y + alpha, N + beta - y) #these are our posterior intervals
posterior_predictive

hist(posterior_sample) #logit-scaled posterior sample

#prior beta distribution

#some plots to answer the question about covariates
shosh.binary



