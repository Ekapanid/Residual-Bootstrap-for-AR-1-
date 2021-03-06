


library(stats)

#Simulation of an AR(1) model with iid error distributed as U(-0.5,0.5) estimation of the model parameters and errors

x<-arima.sim(model=list(ar=.75), n=1000, rand.gen = runif, min = -0.5, max = 0.5)

y<-arima(x , order = c(1, 0, 0))

e<-y$residuals

init.pred<-predict(y,n.ahead = 2)

init.pred<-init.pred$pred[2]

init.pred




error<-rep(0,1000)


for (j in 1:1000){
#Bootstrap sample computation
boot_res<-sample(e,1002,replace=TRUE)



#new sample (Χ,Υ*)
simul_sample<-rep(0,1002)

simul_sample[1]<-x[1]

for (i in 2:1002){
  simul_sample[i]<- -0.0739+0.7525*simul_sample[i-1]+boot_res[i]
}


#Building the new model using the newly generated data.
simul_sample

simul_model<-arima(simul_sample[c(1,1000)] , order = c(1, 0, 0))


#Prediction with the new model for 2 steps ahead
pred<-predict(simul_model,n.ahead = 2)

pred$pred[2]

pred<-pred$pred[2]

pred


# Estimation of prediction error 
error[j]<-simul_sample[1002]-pred
}



hist(error,col="lightblue")
abline(v=-0.7314485,col="red",lwd=3,lty=3)
abline(v=0.6722310,col="red",lwd=3,lty=3)


quant<-quantile(error, probs = c(0.05, 0.95))

init.pred+quant[1]
init.pred+quant[2]




