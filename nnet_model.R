##### HIGHLIGHT BELOW AND PRESS CTRL+ENTER #######
######### MODIFY THIS #############
# example: First and second layer both have 1 neuron, feel free to increase or add new
# layers


######### DONT MODIFY THIS ########
#install.packages("MASS")
#install.packages("neuralnet")
minval <- Inf

set.seed(500)
library(MASS)
data <- Boston

index <- sample(1:nrow(data),round(0.75*nrow(data)))  # 75% of data will be used for training

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]  # splitting into training
test_ <- scaled[-index,]  # and test data sets

library(neuralnet)

for(numneu in seq(5, 20)) {
  for (numlay in seq(3,10)) {
    LAYERS <- c(numneu, numlay)
    
    n <- names(train_)
    f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
    nn <- neuralnet(f,data=train_,hidden=LAYERS,linear.output=T)
    pr.nn <- compute(nn,test_[,1:13])
    pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
    test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
    MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
    
    if (MSE.nn < minval) {
      minval <- MSE.nn
      print("New minimum found!")
    }
    
    print(paste(numlay, "layers with", numneu, "neurons each"))
    
    print(paste("MSE:", MSE.nn))
    print("")
    ###########################################
    
    
    #### HIGHLIGHT EACH LINE AND PRESS CTRL+ENTER TO RUN
    
    # PLOT REAL VS PREDICTED DATA
    #plot(test_$medv,pr.nn$net.result,col='red',main='Real vs predicted NN',pch=18,cex=0.7, xlab="Real Data", ylab="Predicted by NNet")
    #abline(0,1,lwd=2)
    
    # VIEW YOUR NEURAL NETWORK
    #plot(nn)
  }  # numlay
}  # numneu
print("Final minimum value:")
print(minval)
