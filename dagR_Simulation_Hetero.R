
library("dagR")


## simulation 5 - DAG of heterogeneity Elwert & Winship paper (Figure 3)



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
reg1 <- lm(c1~a+b_g, data=df)
results[2,] <- coef(reg1)[2:3]
reg2 <- lm(c2~a+b, data=df)
results[3,] <- coef(reg2)[2:3]
reg3 <- lm(c3~a+b, data=df)
results[4,] <- coef(reg3)[2:3]
reg4 <- lm(c4~a+b_g, data=df)
results[5,] <- coef(reg4)[2:3]
reg5 <- lm(c5~a+b_g, data=df)
results[6,] <- coef(reg5)[2:3]
reg6 <- lm(c6~a+b, data=df)
results[7,] <- coef(reg6)[2:3]
reg7 <- lm(c7~a+b_g, data=df)
results[8,] <- coef(reg7)[2:3]