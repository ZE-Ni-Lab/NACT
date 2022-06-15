rm(list = ls())
options(stringsAsFactors = F)

## Pi
if(T){
  load("dat_for_pi.Rdata")
  pi_res = data.frame(pi = c(),
                      p.value = c())
  for(i in 1:(length(colnames(dat))-1)){
    line.model = lm(dat$final_change ~ dat[,i])
    r.squared = summary(line.model)$r.squared
    slope = (line.model$coefficients)[2]
    slope.symbol = ifelse(slope>0,1,-1)
    Regressionp <- function (modelobject) {
      if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
      f <- summary(modelobject)$fstatistic
      p <- pf(f[1],f[2],f[3],lower.tail=F)
      attributes(p) <- NULL
      return(p)
    }
    pi = -slope.symbol*r.squared*mean(dat[,i])
    pi_res_tmp = data.frame(pi = pi)
    pi_res = rbind(pi_res,pi_res_tmp)
  }
  rownames(pi_res) = colnames(dat)[1:(length(colnames(dat))-1)]
}

## Ti
if(T){
  load("dat_for_ti.Rdata")
  ti_res = data.frame(ti = c(),
                      p.value = c())
  for(i in 1:(length(colnames(dat))-1)){
    line.model = lm(dat$final_change ~ dat[,i])
    r.squared = summary(line.model)$r.squared
    slope = (line.model$coefficients)[2]
    slope.symbol = ifelse(slope>0,1,-1)
    Regressionp <- function (modelobject) {
      if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
      f <- summary(modelobject)$fstatistic
      p <- pf(f[1],f[2],f[3],lower.tail=F)
      attributes(p) <- NULL
      return(p)
    }
    ti = -slope.symbol*r.squared*mean(dat2[,i])
    ti_res_tmp = data.frame(ti = ti)
    ti_res = rbind(ti_res,ti_res_tmp)
  }
  rownames(ti_res) = colnames(dat)[1:(length(colnames(dat))-1)]
}