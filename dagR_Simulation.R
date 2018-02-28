# Vulnerability = f(Competition in Category, X)
# Software Complexity → Competition (common cause)
# Software Complexity → Vulnerability (common cause)
# Competition → QA spend → Vulnerability (intermediate outcome)
# Competition →  Standardization ← Vulnerability (collider, common effect)
# Competition →  number of complementors ← Vulnerability   (collider, common effect)
# Use-case heterogeneity→ REV_lag → competition → vulnerability   (weak instruments)
# Single, widespread use-case → REV_lag → competition → vulnerability  (weak instruments)

library("dagR")
library(sem) 

dag1 <- dag.init(covs = c(1,1,1,1,1,1,1), arcs = c(0,1, 1,-1, 2,0, 2,-1, 0,3, -1,3, 0,4, -1,4, 5,0, 6,5, 7,5), cov.names = c("QA spend","Software Complexity","Standardization","Number of Complementors","Lagged revenue","Use-case heterogeneity","Single, widespread use-case"), x.name = "Competition", y.name = "Vulnerability")
dag1$x <- c(0.25,0.625,0.625,0.35,0.85,0,0,0,1)
dag1$y <- c(0.4,0.5,0.85,0,0,0.4,0,0.8,0.4)
dag1$symbols <- c("x","c1","c2","c3","c4","c5","c6","c7","y")
dag.draw(dag1, numbering = T)

sim1 <- dag.sim(dag1, b=c(0.4, 0.2, 1, 2, 0.3, 0.6, 1.3, 1.6, 0.5, 0.25, 1), stdev=rep(1, length(dag1$x)), seed=1, n=1000)

coef(lm(y~x, data=sim1))[2]
coef(lm(y~x+c1, data=sim1))[2]
coef(lm(y~x+c1+c2, data = sim1))[2]
coef(lm(y~x+c2, data = sim1))[2] 