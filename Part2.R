##Question 1
#Part c
assort <- function(stock,capacity,cost,class){
  total_capacity <- sum(capacity*stock)
  total_cost <- sum(stock*cost)
  
  f <- which(class=='Furniture')
  d <- which(class=='Decor')
  b <- which(class=='Bedding and bath')
  r <- which(class=='Rugs')
  a <- which(class=='Appliances')
  
  deviation <- abs(sum(stock[f]*capacity[f])-12900)+
    abs(sum(stock[d]*capacity[d])-6000)+
    abs(sum(stock[b]*capacity[b])-3900)+
    abs(sum(stock[a]*capacity[a])-7000)+
    abs(sum(stock[r]*capacity[r])-2500)
  
  if(total_capacity > 32300|
     total_cost > 2300000|
     0.15*total_cost > sum(stock[f]*cost[f])|
     0.4*total_cost < sum(stock[f]*cost[f])|
     sum(stock < 0)>0|sum(stock)<=0){
    return(Inf)
  }else{
    return(deviation)
  }
  
}
BestHome <- read.csv(file = "BestHomePart2.csv",header = T, as.is = T)
Class <- BestHome$Class
Cost <- BestHome$Purchasing_cost
Capacity <- BestHome$Required_capacity
F <- which(Class=='Furniture')
D <- which(Class=="Decor")
B <- which(Class=='Bedding and bath')
R <- which(Class=='Rugs')
A <- which(Class=='Appliances') 
##Infeasible
Stock <- c()
for(i in 1:nrow(BestHome)){
  Stock[i] <- 2
}
infeasible <- assort(Stock,Capacity,Cost,Class)
infeasible
###Feasible
Stock <- c()
for(i in 1:nrow(BestHome)){
  Stock[i] <- 0
}
Stock[F][1:40]<-7
sum(Stock[F]*Capacity[F])
Stock[D][1:41]<-19
sum(Stock[D]*Capacity[D])
Stock[B][1:100]<-22.8
sum(Stock[B]*Capacity[B])
Stock[R][1:100]<-9
sum(Stock[R]*Capacity[R])
Stock[A][1:80]<-8
sum(Stock[A]*Capacity[A])
Feasible <- Stock
Feasible_result <- assort(Feasible,Capacity,Cost,Class)
Feasible_result
policy <- optim(Stock,assort,
                capacity=Capacity,
                cost=Cost,
                class=Class)
policy$par
policy$value
policy1 <- optim(policy$par,assort,
                capacity=Capacity,
                cost=Cost,
                class=Class)
round(policy1$par)
round(policy1$value)
policy2 <- optim(policy1$par,assort,
                 capacity=Capacity,
                 cost=Cost,
                 class=Class)
round(policy2$par)
round(policy2$value)
####Part d
Stock <- round(policy2$par)
sum(Stock>0)##products have positive stock quantity
####Part e
nF <- sum(Stock[F]>0)
nD <- sum(Stock[D]>0)
nB <- sum(Stock[B]>0)
nR <- sum(Stock[R]>0)
nA <- sum(Stock[A]>0)
df <- data.frame('Class'= c('Furniture','Decor','Bedding and bath','Rugs','Appliances'),'Number'= c(nF,nD,nB,nR,nA))
df
####Part f
#the maximum,min,mean and median of the stock quantities for products
#with positive stock quantity
row <- which(Stock>0)
max(Stock[row])
min(Stock[row])
mean(Stock[row])
median(Stock[row])

#Part2
##Question b
#simulate the cost of the optimal assortment policy
Stock <- policy2$par
set.seed(0)
nsim <- 1000
total_purchase_cost <- c()
pcost <- c()
pcost_with_c <- c()
for(i in 1: nsim){
  for(n in 1:nrow(BestHome)){
  pcost[n] <- rnorm(n=1,Cost[n],sd=100)
  pcost_with_c[n] <- max(pcost[n],0)}
  
 total_purchase_cost[i] <- sum(pcost_with_c*Stock)
  
}
total_purchase_cost
##Question c
#the sample mean of cost values 
avg_c <- mean(total_purchase_cost)
avg_c
sd_c <- sd(total_purchase_cost)
sd_c
MSE <- sd_c/sqrt(nsim)
##Question d
##the 95% confidence interval on the mean cost
UB <- avg_c + 1.96*MSE
UB
LB <- avg_c - 1.96*MSE
LB
##Question e
##using the initial fesible point
Stock <- Feasible
set.seed(0)
nsim <- 1000
total_purchase_cost <- c()
pcost <- c()
pcost_with_c <- c()
for(i in 1: nsim){
  for(n in 1:nrow(BestHome)){
    pcost[n] <- rnorm(n=1,Cost[n],sd=100)
    pcost_with_c[n] <- max(pcost[n],0)}
  
  total_purchase_cost[i] <- sum(pcost_with_c*Stock)
  
}
total_purchase_cost
avg_c <- mean(total_purchase_cost)
avg_c
sd_c <- sd(total_purchase_cost)
sd_c
MSE <- sd_c/sqrt(nsim)
UB <- avg_c + 1.96*MSE
UB
LB <- avg_c - 1.96*MSE
LB
##Compared to the optimal option, we can recognize the optimal policy is better than initial feasible point