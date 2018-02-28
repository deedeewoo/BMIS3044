# install.packages("dagR")
library("dagR")
library(sem) 

### built-in DAG examples ###

demo1<-demo.dag1()
dag.draw(demo1, numbering = T)

demo1a <- add.arc(demo1, arc=c(2,4))
dag.draw(demo1a, numbering = T)

demo1b <- add.node(demo1)
dag.draw(demo1b, numbering = T)

demo1c <- add.arc(demo1b, arc=c(5,4))
dag.draw(demo1c, numbering = T)

demo1<-demo.dag1()
dag.draw(demo1, numbering = T)

demo2<-demo.dag2()
dag.draw(demo2, numbering = T)

demo3<-demo.dag3()
dag.draw(demo3, numbering = T)

demo4<-demo.dag4()
dag.draw(demo4, numbering = T)

demo5<-demo.dag5()
dag.draw(demo5, numbering = T)

demo6<-demo.dag6()
dag.draw(demo6, numbering = T)

demo7<-demo.dag7()
dag.draw(demo7, numbering = T)

### function: dag.init(), add.node(), add.arc()

### let's start with the simpliest one

dag1 <- dag.init(x.name = "Treatment", y.name = "Outcome" )
dag.draw(dag1)


## confounding variable & backdoor path
## configure the 'arcs' argument

dag1_a <- dag.init(covs=c(1), arcs = c(1,0, 1,-1), cov.names = c("Confounding"), x.name = "Treatment", y.name = "Outcome" )
dag.draw(dag1_a)

## add node & arc
dag1_b <- add.node(dag1, name="Control1")
dag.draw(dag1_b)

dag1_c <- add.arc(dag1_b, c(2,1))
dag1_c <- add.arc(dag1_c, c(2,3))


### how to use add arc & add node together
dag<-dag.init(y.name="injury", x.name="warm-up exercises",
              covs=c(1,2,1,1,1,1,1,1,1,1,1),
              cov.names=c("coach", "genetics",
                          "fitness", "connective tissue disorder",
                          "pre-game proprioception", "motivation, aggression",
                          "fatigue", "contact sport",
                          "previous injury", "tissue weakness",
                          "intra-game proprioception"));
dag$x<-c(0.000, 0.261, 0.722, 0.494, 0.995, 0.257,
         0.002, 0.723, 0.505, 0.305, 0.998, 0.502, 1.000);
dag$y<-c(0.000, 0.852, 0.862, 0.761, 0.735, 0.595,
         0.527, 0.611, 0.449, 0.304, 0.401, 0.149, 0.000);

dag<-add.arc(dag, c(1,12));
dag<-add.arc(dag, c(2,7));
dag<-add.arc(dag, c(2,4));
dag<-add.arc(dag, c(3,4));
dag<-add.arc(dag, c(3,8));
dag<-add.arc(dag, c(3,5));
dag<-add.arc(dag, c(4,6));
dag<-add.arc(dag, c(4,8));
dag<-add.arc(dag, c(5,8));
dag<-add.arc(dag, c(5,11));
dag<-add.arc(dag, c(6,1));
dag<-add.arc(dag, c(7,1));
dag<-add.arc(dag, c(7,10));
dag<-add.arc(dag, c(8,12));
dag<-add.arc(dag, c(8,13));
dag<-add.arc(dag, c(9,12));
dag<-add.arc(dag, c(9,10));
dag<-add.arc(dag, c(11,13));
dag<-add.arc(dag, c(12,13));
dag.draw(dag)

## note: 
## the number of the nodes does not follow the settings in dag.init
## refer to the numbering in the draw


## our goal is to 'block' the backdoor path by conditioning on
## dagR package can find the backdoor path for us: find.paths(), eval.paths(), write,paths()
## replicate the dag in AEA Slide 

demo3_a <- dag.init(covs = c(1,1), symbols = c("D","X1","X2","Y"), arcs = c(1,0, 1,-1, 2,0, 2,-1), x.name = "Treatment", y.name = "Outcome")
dag.draw(demo3_a)

demo3_a_evaluated <- eval.paths(find.paths(demo3_a))
dag.draw(demo3_a_evaluated, numbering = T)
write.paths(demo3_a_evaluated)

demo3_b <- dag.init(covs = c(1,1,2), arcs = c())

demo3_c <- dag.init(covs = c(1,1), symbols = c("D","X1","X2","Y"), arcs = c(1,0, 1,-1, 0,2, -1,2), x.name = "Treatment", y.name = "Outcome")
demo3_c_evaluated <- eval.paths(find.paths(demo3_c))
demo3_c_evaluated$x <- c(0.00, 0.5, 0.5, 1.00)
demo3_c_evaluated$y <- c(0.50, 1, 0.00, 0.50)
dag.draw(demo3_c_evaluated, numbering = T)
write.paths(demo3_c_evaluated)

### dag.adjustment should don't be called!!! It's an internal function for dag.adjust.
### We should only use dag.adjust

'''
Week 7 Simulation for DAG - dag.sim

a few important arguments

b: the coefficient of all arcs; ; a vector with same length as arcs
bxy: the direct effect between treatment and outcome
mu: "intercept" term in the regression function; a vector with same length as nodes
stdev: standard deviation; a vector with same length as nodes

(From dagR documentation)
1. simulate data for nodes i without ancestors, drawing from Normal distribution with mean mu[i] and stdev[i] (continuous node), or drawing from Bernoulli events with probability mu[i] (binary node). 

2. simulate data for nodes i for which all ancestors already have been simulated by multiplying the ancestor values with the corresponding arc coefficients
and summing them up, shifting the resulting values to the mean mu[i] specified for the currently simulated node (logit-transformed if binary), then adding noise drawn from a Normal distribution with mean 0 and standard deviation stdev[i], finally using the inverse logit of the resulting values as success probabilities for simulating binary data if node is binary.

For binary node, the standard dev should be 0

'''

## simulation 1 - confounder

dag1<-dag.init(covs=c(1), arcs=c(1,0, 1,-1), symbols=c("D","X1","Y"));
dag.draw(dag1, numbering=TRUE);

sim1 <- dag.sim(dag1, b=c(0.4, 0.2), bxy = 1.5, mu=c(10,20,30), stdev=c(2,3,2), seed=1, n=10000)

## now we can see the impact of confounder

coef(lm(Y~D, data=sim1));
coef(lm(Y~D+X1, data=sim1));

## simulation 2 - intermedia outcome
dag2<-dag.init(covs=c(1), arcs=c(0,1, 1,-1), symbols=c("D","M","Y"));
dag.draw(dag2, numbering=TRUE);

sim2 <- dag.sim(dag2, b=c(2, 0.6), mu=c(10,20,30), stdev=c(2,3,2), seed=1, n=10000)
coef(lm(Y~D, data=sim2));
coef(lm(Y~D+M, data=sim2)); # wrong control!
summary(lm(Y~D+M, data=sim2))

## simulation 3 - common outcome & confounder
dag3<-dag.init(covs=c(1,1), arcs=c(1,0, 1,-1, 0,2, -1,2), symbols=c("D","x1","x2","Y"), cov.names = c("Confounder","Common outcome"));
dag3$x <- c(0.00, 0.5, 0.5, 1.00)
dag3$y <- c(0.50, 1, 0.00, 0.50)
dag.draw(dag3, numbering=TRUE);

sim3 <- dag.sim(dag3, b=c(1,0.8,1.5,1.2), bxy = 0.5, mu=c(10,20,30,10), stdev=c(2,3,2,2), seed=1, n=10000)
coef(lm(Y~D, data=sim3));
coef(lm(Y~D+x1, data=sim3));
coef(lm(Y~D+x1+x2, data=sim3)); # wrong control!

## simulation 4 - unobserved confounder
dag4<-dag.init(covs=c(1,1,1), arcs=c(1,0, 1,-1, 2,0, 3,2, 3,-1), symbols=c("D","x1","x2","U","Y"), cov.names = c("Confounder1","Confounder2","Confounder2 - Unobeserved"));
dag4$x <- c(0.5, 0.5, 0, 0, 1.00)
dag4$y <- c(0.50, 1, 0.5, 0.00, 0.50)
dag.draw(dag4, numbering=TRUE);

sim4 <- dag.sim(dag4, b=c(1,0.8,1.5,1.2,2), bxy = 0.5, mu=c(10,20,30,10,10), stdev=c(2,3,2,2,4), seed=1, n=10000);
coef(lm(Y~D, data=sim4));
coef(lm(Y~D+x1+x2, data=sim4));
coef(lm(Y~D+x1+U, data=sim4));

summary(lm(Y~D+x1+x2, data=sim4));
summary(lm(Y~D+x1+U, data=sim4));
summary(lm(Y~D+x1+U+x2, data=sim4));

## simulation 5 - IV

dag5<-dag.init(covs=c(1,1), arcs=c(1,0, 1,-1, 2,0), symbols=c("A","U","Z","Y"), cov.names = c("Unobserved Confounder","Instrument"));
dag5$x <- c(0.5, 0, 0, 1.00)
dag5$y <- c(0.50, 0, 0.5, 0.50)
dag.draw(dag5, numbering=TRUE);

sim5 <- dag.sim(dag5, b=c(0.6,0.5,0.8), bxy = 1, mu=c(10,20,30,10), stdev=c(5,10,5,5), seed=1, n=1000)

coef(lm(Y~A, data=sim5));
coef(lm(Y~A+U, data=sim5));
lm(Y~A+Z, data=sim5)
## if we control for Z (instrument), the estimator is even more biased

summary(tsls(Y ~ A, ~ Z, data = sim5))
summary(lm(A~Z, data=sim5));
summary(lm(Y~Z, data=sim5));


## wald estimator
first <- lm(A~Z, data=sim5)
reduced <- lm(Y~Z, data=sim5)
coef(reduced)[2]/coef(first)[2]

## let's look at a smaller sample
sim5a <- dag.sim(dag5, b=c(0.6,0.5,0.8), bxy = 1, mu=c(10,20,30,10), stdev=c(1,2,1,1), seed=1, n=50)
coef(lm(Y~A, data=sim5a));
coef(lm(Y~A+U, data=sim5a));
summary(tsls(Y ~ A, ~ Z, data = sim5a))

## weak instrument
sim5b <- dag.sim(dag5, b=c(0.6,0.5,0.2), bxy = 1, mu=c(0.4,20,0.5,10), stdev=c(0,2,0,1), binary=c(1,0,1,0), seed=1, n=10000)
coef(lm(Y~A, data=sim5b));
coef(lm(Y~A+Z, data=sim5b)); # more biasness when adding instrument when the sample size is small
summary(lm(Y~A+U, data=sim5b));
summary(lm(A~Z, data=sim5b));
summary(tsls(Y ~ A, ~ Z, data = sim5b))


## last notes - binary variable

dag.draw(demo4, numbering=TRUE);
sim2<-dag.sim(demo4, b=c(1, 2, 3, log(4), 5), mu=c(10, 20, 30, 0.4, 50), stdev=c(1,2,3,0,5),
              binary=c(0,0,0,1,0), seed=4, n=100, verbose=TRUE);

#############################
'''
Simulation (cont.)
simulate Bernoulli from uniform distribution on [0,1]

'''

# p = 0.6
set.seed(3044)
uniform <- runif(100)
bernoulli <- as.numeric(uniform<0.6)


#############################

## manual simulation of a DAG with binary variable

logit<-function(p) log(p/(1-p)); # helper function;
inv.logit<-function(l) exp(l)/(1+exp(l)); # helper function;

set.seed(4);
xx<-rnorm(m=10, sd=1, n=100); 
c1.noise<-rnorm(sd=2, n=100); 
c1<- 1*xx - mean(1*xx) + 20 + c1.noise; 
c2.noise<-rnorm(sd=3, n=100); 
c2<- 2*xx + 3*c1 - mean(2*xx+3*c1) + 30 + c2.noise; 
c3.raw<- log(4)*c1 - mean(log(4)*c1) + logit(0.4); 
# noise.dummy<-rnorm(m=0, sd=0, n=100);
c3<-sapply(c3.raw, FUN=function(x) rbinom(n=1,size=1,p=inv.logit(x))); 
yy.noise<-rnorm(sd=5, n=100); 
yy<- 0*xx + 5*c3 - mean(5*c3) + 50 + yy.noise; 

sim4_manual <- data.frame(xx,c1,c2,c3,yy);

glm(c3~c1, data=sim4_manual, family=binomial(link=logit))
