
library("dagR")


## simulation 5 - DAG of heterogeneity Elwert & Winship paper (Figure 3)

dag5<-dag.init(covs=c(1,1,1), arcs=c(1,0, 1,-1, 2,0, 3,2, 3,-1), symbols=c("D","x1","x2","U","Y"), cov.names = c("Confounder1","Confounder2","Confounder2 - Unobeserved"));
dag5$x <- c(0.5, 0.5, 0, 0, 1.00)
dag5$y <- c(0.50, 1, 0.5, 0.00, 0.50)
dag.draw(dag5, numbering=TRUE);

sim5 <- dag.sim(dag5, b=c(1,0.8,1.5,1.2,2), bxy = 0.5, mu=c(10,20,30,10,10), stdev=c(2,3,2,2,4), seed=1, n=10000);
coef(lm(Y~D, data=sim4));
coef(lm(Y~D+x1+x2, data=sim5));
coef(lm(Y~D+x1+U, data=sim5));

## manual simulation 

trials <- 100000
df <- data.frame(row.names = 1:trials)

df$a <- rnorm(n = trials)
df$e1 <- rnorm(n = trials)
df$e2 <- rnorm(n = trials)
df$g <- rbinom(n = trials, size = 1, prob = 0.5)
df$b <- 0.8 * df$a + df$e1
df$b_g <- (0.4 + 0.8 * df$g) * df$a + df$e1 # hetero in alpha directly affect B
df$c0 <- df$a + 1.5 * df$b + df$e2 # no hetero - column 1
df$c1 <- df$a + 1.5 * df$b_g + df$e2 # hetero in alpha
df$c2 <- df$a + (0.5 + 2 * df$g) * df$b + df$e2 # beta
df$c3 <- (0.6 + 0.8 * df$g) * df$a + 1.5 * df$b + df$e2 # gamma
df$c4 <- df$a + (0.5 + 2 * df$g) * df$b_g + df$e2 # alpha & beta
df$c5 <- (0.6 + 0.8 * df$g) * df$a + 1.5 * df$b_g + df$e2 # alpha & gamma
df$c6 <- (0.6 + 0.8 * df$g) * df$a + (0.5 + 2 * df$g) * df$b + df$e2 # beta & gamma
df$c7 <- (0.6 + 0.8 * df$g) * df$a + (0.5 + 2 * df$g) * df$b_g + df$e2 # alpha, beta gamma

results <- data.frame(matrix(nrow=7, ncol=2))
reg0 <- lm(c0~a+b, data=df)
results[1,] <- coef(reg0)[2:3]
reg1 <- lm(c1~a+b, data=df)