library(tidyverse)
library(brms)

lepi <- read.table("Garben.txt", header = T)%>%tibble()
#mean fecundity (number of offspring) and body size (in grams) for species of butterflies
lepi

#visualize data
ggplot(lepi, aes(x = Size, Fecundity))+
  geom_point()+
  labs(title = "Lepidopteran fecundity data")

#define a formula:  the modeled relationship
form_lepi <- brmsformula(Fecundity ~ Size)
form_lepi <- bf(Fecundity ~ Size)
#What is the model behind this notation? (write down formula)


########################
# simulate the problem #
########################
#A bayesian model works like a kind of simulation model that (tries to) simulate how the data producing processes
#So we can learn a lot from making bayesian models on simulations where we know what is behind the data

#simulate a number of butterfly species that differ in body size with fecundity increasing in body size and quite some noise

sim <- tibble(species = factor(1:2000), 
              Size = rnorm(2000, mean = 20, sd = 4),                           #our 2000 species have their Size sampled from a Guassian with mean 20 and sd 4
              expected_Fecundity = Size*10+5,                                  #Size influences fecundity with slope = 10 and intercept = 5
              Fecundity = rnorm(2000, mean = expected_Fecundity, sd = 80))     #our each of the 2000 species' fecundity has a random offset from what is expected from their size
ggplot(sim, aes(x = Size, y = Fecundity))+
  geom_point(size = 1)+
  geom_line(aes(y = expected_Fecundity))+
  labs(title = "simulated fecundity")


#Somethings are wrong with the simulation, so let's try again
sim1 <- tibble(species = factor(1:2000),
              logsize = rnorm(2000, mean = 3, sd = 0.2),
              Size = exp(logsize), 
              expected_logfecundity = logsize*2+0.5,
              logfecundity = rnorm(2000, mean = expected_logfecundity, sd = 0.5),
              Fecundity = exp(logfecundity))
ggplot(sim1, aes(x = Size, y = Fecundity))+
  geom_point(size = 1)+
  geom_line(aes(y = exp(expected_logfecundity)))+
  labs(title = "simulated fecundity")

ggplot(sim1, aes(x = logsize, y = logfecundity))+
  geom_point(size = 1)+
  geom_line(aes(y = expected_logfecundity))+
  labs(title = "simulated fecundity on the log-scale")



#Let's fit a model to the simulated example
#on purpose with the original (wrong) model
form_lepi
get_prior(formula = form_lepi, data = sim1)

#Contruct a prior
prior_sim <- c(
  set_prior("normal(50,50)", class = "Intercept"),
  set_prior("normal(0,50)", class = "b"),
  set_prior("cauchy(0,2)", class = "sigma")
)

#run model
mod_sim1 <- brm(formula = form_lepi,
                data = sim1,
                family = "gaussian",
                prior = prior_sim,
                file = "mod_sim1")


#Inspect chains
plot(mod_sim1)
#Inspect fitted parameters
mod_sim1$fit
#Inspect pp: posterior prediction
pp_check(mod_sim1)


#Now try with a model in line with our simulated data
form_loglepi <- bf(logfecundity ~ logsize)
get_prior(formula = form_loglepi, data = sim1)

#Our previous priors are not sensible on the logscale
prior_logsim <- c(
  set_prior("normal(0,1)", class = "b"),
  set_prior("normal(0,1)", class = "Intercept"),
  set_prior("cauchy(0,1)", class = "sigma")
)

#run model
mod_logsim <- brm(formula = form_loglepi,
               data = sim1,
               family = "gaussian",
               prior = prior_logsim,
               iter = 2000,
               chains = 4,
               file = "mod_logsim")

#Inspect chains
plot(mod_logsim)
#Inspect fitted parameters
mod_logsim$fit
#Inspect pp: posterior prediction
pp_check(mod_logsim)

est2 <- fitted(mod_logsim)%>%as_tibble()
#the previous line makes an expected prediction on the log-scale. So we need to calculate back to the natural scale
est2 <- est2%>%mutate(Est_fec = exp(Estimate), 
                      Size_q2.5 = exp(Q2.5), 
                      Size_q97.5 = exp(Q97.5))
sim_est2 <- sim1%>%mutate(est2)
ggplot(sim_est2, aes(x = Size, y = Fecundity))+
  geom_point(size = 1, color = 'blue')+
  geom_line(aes(y = Est_fec))+
  geom_ribbon(aes(ymin = Size_q2.5, ymax = Size_q97.5), alpha = 0.2)+
  geom_line(aes(y = exp(expected_logfecundity)), lty = 2)+
  labs(title = "model expected prediction (marginal means) from simulation")

#Let's see what all of these visualizations are based on: the POSTERIOR
#let's make an object out of (the samples of) our posterior distribution
post_sim2 <- as_draws_df(mod_logsim)%>%as_tibble()
post_sim2
#compare a plot of 1 parameter with the plot function
ggplot(post_sim2, aes(x = b_logsize))+
  geom_density()+
  labs(title = "posterior of slope parameter")
plot(mod_logsim)

#plot two parameters, correlation
ggplot(post_sim2, aes(x = b_logsize, y = b_Intercept))+
  geom_point(alpha = 0.05)
ggplot(post_sim2, aes(x = b_logsize, y = b_Intercept))+
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = 0)
#Is correlation between parameters a problem or not? Depends...


#visualize sampled trendlines
sim2_trends <- slice_sample(post_sim2, n=10)%>%mutate(sample = factor(1:n()))%>%
  expand_grid(sim1%>%select(logsize))%>%
  mutate(logfecundity = b_Intercept + logsize * b_logsize,
         fecundity = exp(logfecundity),
         size = exp(logsize))

ggplot(sim2_trends, aes(x = size, y = fecundity))+
  geom_line(aes(group = sample), alpha = 0.3)+
  geom_point(data = sim1, aes(x = Size, y = Fecundity), size = 0.5)


##################
# Model the data #
##################

ggplot(lepi, aes(x = Size, Fecundity))+
  geom_point()+
  labs(title = "Lepidopteran fecundity data")

#If we are happy with the formula, we can proceed with it
form_loglepi 

#Define a prior. If you doubt what parameters the model want priors for, check them out using get_prior()
#My specific choice 
prior_loglepi<- c(
  set_prior("normal(0,2)", class = "b"),
  set_prior("normal(0,4)", class = "Intercept"),
  set_prior("cauchy(0,1)", class = "sigma")
)

lepi <- lepi%>%mutate(logsize = log(Size), 
                      logfecundity = log(Fecundity))

mod_loglepi <- brm(formula = form_loglepi,
                  data = lepi,
                  family = "gaussian",
                  prior = prior_loglepi,
                  iter = 2000,
                  chains = 4,
                  file = "mod_loglepi")

#Inspect chains
plot(mod_loglepi)
#Inspect fitted parameters
mod_loglepi$fit
#Inspect pp: posterior prediction
pp_check(mod_loglepi)

est3 <- fitted(mod_loglepi)%>%as_tibble()%>%mutate(Est_fec = exp(Estimate), 
                                                   Size_q2.5 = exp(Q2.5), 
                                                   Size_q97.5 = exp(Q97.5))
lepi_est <- lepi%>%mutate(est3)
ggplot(lepi_est, aes(x = Size, y = Fecundity))+
  geom_point(size = 1, color = 'blue')+
  geom_line(aes(y = Est_fec))+
  geom_ribbon(aes(ymin = Size_q2.5, ymax = Size_q97.5), alpha = 0.2)+
  labs(title = "Simulation Fecundity")

lepi_est <- lepi_est%>%mutate(r = logfecundity/logsize)
ggplot(lepi_est, aes(x = r))+
  geom_density()
lepi_est%>%filter(r > 2.3)

#############################################################################
# Consider more complexity: control for phylogenetic relatedness of species #
#############################################################################
#e.g. some high fecundity species can be related and are therefore not completely independent

library("ape")
library("ggtree")

#import phylogenetic data
lepi_tree <- read.tree("Garben_tree.txt")
plot(lepi_tree, cex = 0.3)
ggtree(lepi_tree, size=0.5, ladderize = F)+
  geom_tiplab(size = 1.5)

lepi_informedtree <- full_join(lepi_tree, lepi_est%>%mutate(high_fec = ifelse(r>2.3, 2, 1))%>%rename(label = Species), by = 'label')
ggtree(lepi_informedtree, size=0.5, ladderize = F)+
  geom_tiplab(aes(fontface = high_fec, color = high_fec), size = 1.5)+
  theme(legend.position = 'none')


form_lepiphylo <- bf(logfecundity ~ 0 + Intercept + logsize + (0+logsize|gr(Species, cov = vcv)))
#notice the gr(Species, cov = ...) that informs the model on the phylogenetic-based covariance
#also notice the 0 + Intercept + logsize, which is equivalent to logsize: fit an intercept and a slope. But, slightly different.

lepi_cov <- vcv.phylo(lepi_tree)
get_prior(form_lepiphylo, data = lepi, data2 = list(vcv = lepi_cov))

#define the prior. There is an extra parameter for which you can define a prior, the sd of the variable slope.
prior_lepiphylo<- c(
  set_prior("normal(0,2)", class = "b"),
  set_prior("normal(0,4)", class = "b", coef = "Intercept"),
  set_prior("cauchy(0,1)", class = "sigma"),
  set_prior("cauchy(0,1)", class = "sd")
)

#run the model
mod_lepiphylo <- brm(formula = form_lepiphylo,
                   data = lepi,
                   data2 = list(vcv = lepi_cov),
                   family = "gaussian",
                   prior = prior_lepiphylo,
                   iter = 2000,
                   chains = 4,
                   cores = 2,
                   file = "mod_lepiphylo")

plot(mod_lepiphylo)
mod_lepiphylo$fit
pp_check(mod_lepiphylo)

#check posterior prediction
est_phylo <- fitted(mod_lepiphylo)%>%as_tibble()%>%mutate(Est_fec = exp(Estimate), 
                                                           Size_q2.5 = exp(Q2.5), 
                                                           Size_q97.5 = exp(Q97.5))
est_phylo_lepi <- lepi%>%mutate(est_phylo)
ggplot(est_phylo_lepi, aes(x = Size, y = Fecundity))+
  geom_point(size = 1, color = 'blue')+
  geom_line(aes(y = Est_fec))+
  geom_ribbon(aes(ymin = Size_q2.5, ymax = Size_q97.5), alpha = 0.2)+
  labs(title = "Simulation Fecundity")

est_phylo <- fitted(mod_lepiphylo, re_formula = NA)%>%as_tibble()%>%mutate(Est_fec = exp(Estimate), 
                                                                           Size_q2.5 = exp(Q2.5), 
                                                                           Size_q97.5 = exp(Q97.5))
est_phylo_lepi <- lepi%>%mutate(est_phylo)

ggplot(est_phylo_lepi, aes(x = Size, y = Fecundity))+
  geom_point(size = 1, color = 'blue')+
  geom_line(aes(y = Est_fec))+
  geom_ribbon(aes(ymin = Size_q2.5, ymax = Size_q97.5), alpha = 0.2)+
  labs(title = "Simulation Fecundity")

###########################################
# Extra on priors: prior predictive check #
###########################################
# a good check to see what you imply with priors you defined
# we could do this by sampling random values from the distributions we defined as prior distributions, e.g. using rnorm(., mean = 0, sd = 1)
#But brms can do this for you using the argument sample_prior = "only"

#let's first test a prior for the logfecundity in the Lepidotera data of the most basic distributions
prior_loglepi<- c(
  set_prior("normal(0,1)", class = "b"),
  set_prior("normal(0,1)", class = "Intercept"),
  set_prior("cauchy(0,1)", class = "sigma")
)

mod_prior <- brm(formula = form_loglepi,
                 data = lepi,
                 family = "gaussian",
                 prior = prior_loglepi,
                 iter = 2000,
                 chains = 4,
                 sample_prior = "only",
                 file = "mod_prior")

#Inspect pp: but here it means prior prediction
#Priors shouldn't already match your data, that is what the data is for.  It should allow for a wide variety of "realistic" data

pp_check(mod_prior)

#what inferential trends are allowed under
priorexpected_d <- as_draws_df(mod_prior)%>%as_tibble()%>%slice_sample(n = 100)%>%
  expand_grid(logsize = lepi$logsize)%>%
  mutate(logfecundity = b_Intercept + b_logsize*logsize,
         size = exp(logsize),
         fecundity = exp(logfecundity))

#is this a good prior expectation for the kind of trends we want to explore with the data?
ggplot(priorexpected_d, aes(x = size, y = fecundity))+
  geom_line(aes(group = .draw))

ggplot(priorexpected_d, aes(x = logsize, y = logfecundity))+
  geom_line(aes(group = .draw))

prior_loglepi2<- c(
  set_prior("normal(0,2)", class = "b"),
  set_prior("normal(0,4)", class = "Intercept"),
  set_prior("cauchy(0,1)", class = "sigma"))

mod_prior2 <- brm(formula = form_loglepi,
                 data = lepi,
                 family = "gaussian",
                 prior = prior_loglepi2,
                 iter = 2000,
                 chains = 4,
                 sample_prior = "only",
                 file = "mod_prior2")

pp_check(mod_prior2)

#let's sample a bunch of intercepts and slopes: the inferential trends that are allowed under this kind of prior
priorexpected_d2 <- as_draws_df(mod_prior2)%>%as_tibble()%>%slice_sample(n = 100)%>%
  #the expand grid enables you to pair each intercept slope combination (posterior draw) with each x-value in the original dataset to enable us the expected at each x-value
  expand_grid(logsize = lepi$logsize)%>%
  mutate(logfecundity = b_Intercept + b_logsize*logsize,
         size = exp(logsize),
         fecundity = exp(logfecundity))

ggplot(priorexpected_d2, aes(x = size, y = fecundity))+
  geom_line(aes(group = .draw))

ggplot(priorexpected_d2, aes(x = logsize, y = logfecundity))+
  geom_line(aes(group = .draw))


priorpredictive <- predict(mod_prior2)%>%as_tibble()%>%mutate(Est_fec = exp(Estimate), Size_q2.5 = exp(Q2.5), Size_q97.5 = exp(Q97.5), logsize = lepi$logsize, size = lepi$Size)
ggplot(priorpredictive, aes(x = logsize, y = Estimate))+
  geom_line()+
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.2)
ggplot(priorpredictive, aes(x = logsize, y = Estimate))+
  geom_line()+
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.2)+
  geom_point(data = lepi, aes(x = logsize, y = logfecundity))

################################
# Extra on centering variables #
################################
#remember the correlated parameters
ggplot(post_sim2, aes(x = b_logsize, y = b_Intercept))+
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = 0)

#You can typically avoid (or at least ameliorate) this by centering the variables in your data around 0, by substracting each value of a variable with the mean of that variable
sim1 <- sim1%>%mutate(clogfecundity = logfecundity - mean(.$logfecundity),
                      clogsize = logsize - mean(.$logsize))

form_cloglepi <- bf(clogfecundity ~ clogsize)

#we try to fit similar parameters, so no difference in priors we need to set
get_prior(formula = form_cloglepi, data = sim1)


#run model with centered variables
mod_clogsim <- brm(formula = form_cloglepi,
                   data = sim1,
                   family = "gaussian",
                   prior = prior_logsim,
                   iter = 2000,
                   chains = 4,
                   file = "mod_clogsim")


estc2 <- fitted(mod_clogsim)%>%as_tibble()
#remember that we modelled not only log-transformed data, but that was also centered. So to calculate back we add the mean of the variable again and then exponentiate.
estc2 <- estc2%>%mutate(Est_fec = exp(Estimate+mean(sim1$logfecundity)), 
                        Size_q2.5 = exp(Q2.5+mean(sim1$logfecundity)), 
                        Size_q97.5 = exp(Q97.5+mean(sim1$logfecundity)))
sim_estc2 <- sim1%>%mutate(estc2)

#the expected prediction is still the same, so it seems that this model performs equally in terms of fitting the data
ggplot(sim_estc2, aes(x = Size, y = Fecundity))+
  geom_point(size = 1, color = 'blue')+
  geom_line(aes(y = Est_fec))+
  geom_ribbon(aes(ymin = Size_q2.5, ymax = Size_q97.5), alpha = 0.2)+
  geom_line(aes(y = exp(expected_logfecundity)), lty = 2)+
  labs(title = "model expected prediction (marginal means) from simulation")

#but there is no autocorrelation between slope and intercept
ggplot(post_csim2, aes(x = b_clogsize, y = b_Intercept))+
  geom_point(alpha = 0.3)
ggplot(post_csim2, aes(x = b_clogsize, y = b_Intercept))+
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = 0)

