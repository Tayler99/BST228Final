library(tidyverse)
library(semico)
library(INLA)
library(survHE)
library(rstan)
setwd("~/Desktop/BST228/project")

data=read.csv("COSMOSCVD.csv")
data$arm01=ifelse(data$arm=="placebo",0,1)

# INLA installation not working
# y.surv=inla.surv(data$time,data$event) ~ data$arm01
# model=inla(y.surv, family ="coxph", data=data, verbose=T,
#            control.hazard=list(model="rw1", n.intervals=20))
# cox1=inla.coxph(~arm01,data=data, family="exponential.surv")

m_NIP = fit.models(formula = Surv(time,event)~arm01, data = data, 
                distr = "weibullPH",#c("exp","weibull"),
                method = "hmc",
                priors = list(
                  # exp = list(
                  #   mu_beta = c(0,-0.1), 
                  #   sigma_beta= c(1,1)),
                  # weibull = list(
                  #   a_alpha=0.1,
                  #   b_alpha=0.1,
                  #   mu_beta = c(0,-0.1), 
                  #   sigma_beta= c(1,1)),
                  weibullPH = list(
                      a_alpha=0.1,
                      b_alpha=0.1,
                      mu_beta = c(0,0), 
                      sigma_beta= c(10,10)
                    )
           ))


#informative prior
#https://link.springer.com/article/10.1007/s00394-019-01914-9
mu=log(0.92)
mu_se=((log(1)-mu)+(mu-log(0.85)))/2/1.96

m_IP = fit.models(formula = Surv(time,event)~arm01, data = data, 
                   distr = "weibullPH",#c("exp","weibull"),
                   method = "hmc",
                   priors = list(
                     weibullPH = list(
                       a_alpha=0.1,
                       b_alpha=0.1,
                       mu_beta = c(0,mu), 
                       sigma_beta= c(10,mu_se)
                     )
                   ))

#results
print(m_NIP)
exp(-0.09108809)
exp(-0.2371950)
exp(0.0485732)

print(m_IP)
exp(-0.08565226)
exp(-0.15474281)
exp(-0.0151697)


m3$models$`Weibull (PH)`
rstan::traceplot(m3$models$`Weibull (PH)`)
rstan::stan_ac(m3$models$`Weibull (PH)`)

m3$misc







