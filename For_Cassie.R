library(brms)
library(rstan)

# in case it crashes
rstan_options(auto_write = TRUE)

# logistic regression of shared tools 
# dont forget to scale because the priors are standardised
t1 <- bf(shared ~ scale(distance)) + bernoulli()
know1 <- brm(t1, data = m.know,
      prior=c(prior("normal(0,2)", class="Intercept"),
              prior("normal(0,2)", class="b")), cores = 8, seed = 100,
      warmup=1000, iter=2000, chains=4, control=list(adapt_delta=0.90)) 
summary(know1)
posterior_summary(know1)
plot(know1)
mcmc_plot(know1, type = "acf_bar")
mcmc_plot(know1, type = "areas")

mcmc_plot(know1, 
          type = "areas",
          prob = 0.95,
          transformations = "exp") +
  geom_vline(xintercept = 1, color = "grey")

# with dyadId as random effect
know2 <- brm(shared ~ scale(distance) + (1|dyadID), 
             family = bernoulli(), data = m.know,
             prior=c(prior("normal(0,2)", class="Intercept"),
                     prior("normal(0,2)", class="b"),
                     prior("exponential(1)", class="sd")), 
             cores = 8, seed = 100, 
             warmup=1000, iter=2000, chains=4,
             control=list(adapt_delta =0.95))

summary(know2)
posterior_summary(know2)
plot(know2)
mcmc_plot(know2, type = "acf_bar")
mcmc_plot(know2, type = "areas")

# poisson models of complexity
table(m.complex$sharedsequence)
table(m.complex$sharedmod)

complex.prior <- c(prior(normal(0,1), class=Intercept),
  prior(normal(0,1), class=b),
  prior(exponential(1), class=sd))
complex1 <- brm(sharedmod ~ scale(IBDmean) + scale(distance) + (1 | dyadID), 
                family=poisson(), data=m.complex,
                prior=complex.prior,
                cores=8, chains=4, 
                control=list(adapt_delta =0.95))

summary(complex1)
posterior_summary(complex1)
plot(complex1)
mcmc_plot(complex1, type = "acf_bar")
mcmc_plot(complex1, type = "areas")

# Hurdle model
# hurdle coefficient: log of prob of 0 count, relative to any positive count
hist(m.complex$sharedsequence, breaks=seq(-1,10,1))

complex_hd.prior <- c(prior(normal(0,1), class=Intercept),
                   prior(normal(0,1), class=b),
                   prior(exponential(1), class=sd),
                   prior(normal(0,1), class=Intercept, dpar="hu"),
                   prior(normal(0,1), class=b, dpar="hu"),
                   prior(exponential(1), class=sd, dpar="hu"))
complex.hd <- brm(sharedmod ~ scale(distance) + (1 | dyadID), 
                hu ~ scale(distance) + (1 | dyadID),
                family=hurdle_poisson(), data=m.complex,
                prior=complex.prior,
                cores=8, chains=4, 
                control=list(adapt_delta =0.95))

summary(complex.hd)
posterior_summary(complex.hd)
plot(complex.hd)
mcmc_plot(complex.hd, type = "acf_bar")
mcmc_plot(complex.hd, type = "areas")
pp_check(complex.hd, ndraws=100)

              
