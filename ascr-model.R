library(R2admb)
library(testthat)
library(ascr)
test.ascr(quick = TRUE)

load("test-data.RData")

##capture history
cap_hist = function(region_area,D,detector_location,g0,sigma){
  
  centre_location_num = D*region_area
  centre_location=matrix(runif(centre_location_num*2,min = -500,max = 900),ncol=2)
  colnames(centre_location) = c("x", "y")
  
  cap_hist = matrix(rep(NA,nrow(centre_location)*nrow(detector_location)),nrow = nrow(centre_location))
  
  for (i in 1:nrow(centre_location)) {
    for (j in 1:nrow(detector_location)) {
      delta_x = centre_location[i,1] - detector_location[j,1]
      delta_y = centre_location[i,2] - detector_location[j,2]
      d = sqrt(delta_x^2 + delta_y^2)
      g_d = g0 * exp(-d^2/(2*sigma^2))
      cap_hist[i,j] = rbern(1,g_d)
    }
  }
  
  for (u in nrow(cap_hist):1){
    if (all(cap_hist[u,] == 0))
      cap_hist = cap_hist[-u,]
  }
  cap_hist
}

D = 2
sigma = 75
g0 = 0.9
region_area = (900-(-500))^2/10000
detector_location = cbind(rep(c(100,200,300),each=3),rep(c(100,200,300),3))
colnames(detector_location) = c("x","y")

bin_cap = cap_hist(region_area,D,detector_location,g0,sigma)

first = c(bin_cap)
second = rep(1:nrow(bin_cap),ncol(bin_cap))
third = rep(NA,nrow(bin_cap))
fourth = rep(1:ncol(bin_cap),each=nrow(bin_cap))

capt = data.frame(first, second, third, fourth)

captures = create.capt(capt, traps = test.data$traps)

##fit
fit = fit.ascr(capt = captures,
               traps = test.data$traps,
               mask = test.data$mask,
               detfn = "hn")

##check
##run function scr.nll first in scr-ll.r
par.start = c(log(0.1), qlogis(0.5), log(50))
check = optim(par.start, scr.nll, capt = bin_cap, traps = test.data$traps, mask = test.data$mask)

## D:
exp(check$par[1])
## g0:
plogis(check$par[2])
## sigma:
exp(check$par[3])

summary(fit)
coef(fit)
confint(fit)
show.detfn(fit)
show.detsurf(fit)
##show.capt(fit)
locations(fit)
boot.ascr(fit, N=10)
p.dot(fit)

##inhomogeneous density model 
cov.df = data.frame(a = test.data$mask[, 1]/1000, b = test.data$mask[, 2]/1000)
fit2 = fit.ascr(capt = captures,
                traps = test.data$traps,
                mask = test.data$mask,
                ihd.opts = list(model = ~ exp(x + y),covariates = cov.df))

summary(fit2)
coef(fit2)
confint(fit2)
show.detfn(fit2)
show.detsurf(fit2)
locations(fit2)
p.dot(fit2)
show.Dsurf(fit2)
