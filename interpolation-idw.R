library(gstat)
library(sp)
library(ggplot2)

#covariate df
load("test-data.RData")
mask.dt = as.data.frame(test.data$mask)
mask.dt = mask.dt/100

n1 = 1e5L
first.covariate.dt = data.frame(
  X = sort(rnorm(n1)), Y = rnorm(n1),
  var1 = rpois(n1, lambda = pi), var2 = sort(runif(n1)))

n2 = 1e3L
index = sample(1L:n1, n2)
second.covariate.dt = data.frame(
  X = c(first.covariate.dt$X[index], rnorm(n2)),
  Y = c(first.covariate.dt$Y[index], rnorm(n2)),
  var2 = c(first.covariate.dt$var2[index], runif(n2)),
  var3 = sample(x = LETTERS, size = n2*2L, replace = TRUE),
  var4 = sort(rnorm(n2*2L)))

n3 = 1e2L
third.covariate.dt = data.frame(
  X = rnorm(n3), Y = rnorm(n3))

###
cov.bind = function(cov.var, ...) {
  cov.list = list(...)
  names(cov.list) = 1:length(cov.list)
  col = c("X", "Y", cov.var)
  
  df = data.frame()
  for (i in 1:length(cov.list)) {
    cov.df = cov.list[[i]]
    if (cov.var %in% names(cov.df)) {
      df = rbind(df, cov.df[col])
    }
  }
  df
}

###
plot = function(mask, var, formula, nmax=1, maxdist=1, ...){
  df = mask
  coordinates(df) = ~ x + y
  gridded(df) = TRUE
  
  output.df = matrix(NA, nrow = nrow(mask), ncol = length(var))
  output.df = data.frame(mask,output.df)
  plot.list = list()
  
  for (i in 1:length(var)) {
    var.df = cov.bind(var[i],...)
    coordinates(var.df) = ~ X + Y
    
    # Inverse Distance Weighting
    idw = idw(
      formula = as.formula(formula[i]),
      locations = var.df,
      newdata = df,
      nmax = nmax,
      maxdist = maxdist
    )
    idw.output = as.data.frame(idw)
    names(idw.output)[1:3] = c("x","y","Prediction")
    output.df[i+2] = idw.output[3]
    
    plot.list[[i]] = ggplot(data = idw.output, aes(x = x, y = y)) +
      geom_tile(data = idw.output, aes(fill = Prediction)) +
      scale_fill_gradient(low = "#FEEBE2", high = "#7A0177") +
      coord_equal()+
      ggtitle(formula[i])
  }
  
  names(output.df)=c("x", "y", formula)
  names(plot.list) = formula
  multi.return = list(output.df, plot.list)
  
  return(multi.return)
}

plot(mask = mask.dt, 
     var = c("var1","var2","var4"),
     formula = c("var1 ~ 1","var2 ~ 1","var4 ~ 1"), 
     nmax = 5, maxdist = 5,
     first.covariate.dt,second.covariate.dt,third.covariate.dt)



