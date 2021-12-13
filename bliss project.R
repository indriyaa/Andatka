setwd("E:/PRAK ANMUL")
bliss <- read.csv("bliss.csv", header=TRUE, sep=";")
bliss
mod1 <- glm(cbind(dead, alive) ~ conc, family=binomial, bliss)
summary (mod1) $coef

y <- bliss$dead/30
mu <- y
install.packages("car")
library(car)
eta <- logit(mu)
z <- eta + (y-mu)/(mu*(1-mu))
w <- 30*mu*(1-mu)
lmod <- lm(z ~ conc, weights=w, bliss)
coef(lmod)

install.packages("faraway")
library(faraway)

for(i in 1:5){
  eta <- lmod$fit
  mu <- ilogit(eta)
  z <- eta + (y-mu)/(mu*(1-mu))
  w <- 30*mu*(1-mu)
  lmod <- lm(z~bliss$conc, weights=w)
  cat(i, coef(lmod), "\n")
}

summary(lmod)

xm <- model.matrix(lmod)
wm <- diag(w)
sqrt(diag(solve(t(xm)%*% wm %*% xm)))

summary(lmod)$coef[,2]/summary(lmod)$sigma
