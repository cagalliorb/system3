library(scatterplot3d)
library(numDeriv)
library(MASS)

deltaT <- 0.001 # 時間ステップ
g <- 9.80       # 重力加速度[m/s^2]
kappa <- 1      # 抵抗係数
m <- 1          # 物体の質量[m/kg]
dimension <- 2  # 位置と速度の2次元ベクトル 

f1 <- function(x,v){
  x + v*deltaT
}
f2 <- function(x,v){
  v + deltaT*(-g + kappa/m*v*v)
}
f <- function(x){
  c(f1(x = x[1],v = x[2]),
    f2(x = x[1],v = x[2]))
}

C <- diag(dimension) # 観測方程式のC

stateVariance <- 0.0001 # 状態ノイズの分散
obsVariance <- 0.01     # 観測ノイズの分散
nObs <- 500             # 時間ステップの繰り返し数
itr_num <-  10          # この回数分,違った乱数を生成

act_obs <- data.frame(X=rep(0, nObs),
                      Y=rep(0, nObs)) # 真値と観測値の誤差
act_est <- data.frame(X=rep(0, nObs),
                      Y=rep(0, nObs)) #真値と推定値の誤差


Q <- diag(c(stateVariance, stateVariance)) # State eq. noise Covariance
R <- diag(c(obsVariance,obsVariance))      # Observation eq. noise Covariance

"EKF main part"
for (j in 1:itr_num){
  x_est <- c(0, 0) # 初期値
  x_actual <- c(0, 0) # 初期値
  V <-  diag(c(0, 0))
  state_noise <- mvrnorm(nObs, c(0,0),Q)
  obs_noise <- mvrnorm(nObs, c(0,0),R)

  Time <- data.frame(Time=rep(NA, nObs))
  Actual   <- data.frame(X=rep(NA, nObs),
                       Y=rep(NA, nObs))
  Observed <- data.frame(X=rep(NA, nObs),
                       Y=rep(NA, nObs))
  Estimated <- data.frame(X=rep(NA, nObs),
                       Y=rep(NA, nObs))
  for(i in 1:nObs){
    # Observe
    x_actual <- c(f1(x_actual[1],x_actual[2]),
                f2(x_actual[1],x_actual[2])) + state_noise[i,]
    y <- C %*% x_actual + obs_noise[i,]
    # Predict
    x_prime <- f(x = x_est)
    A <- jacobian(f, x=x_est) 
    V_prime <- A %*% V %*% t(A) + Q
    # Update
    K <- V_prime %*% t(C) %*% solve(C %*% V_prime %*% t(C) + R)
    x_est <- x_prime + K %*% (y - C %*% x_prime)
    V <- (diag(dimension) - K %*% C) %*% V_prime
    
    Time[i,] <- deltaT*i
    Estimated[i,] <- x_est
    Actual[i,] <- x_actual
    Observed[i,] <- y
  }
  
  sum_obs <- 0.0
  sum_est <- 0.0
  for (k in 1:nObs){
    sum_obs <- sum_obs + abs(Actual[k,2] - Observed[k,2])
    sum_est <- sum_est + abs(Actual[k,2] - Estimated[k,2])
    act_obs[k,2] <- act_obs[k,2] + abs(Actual[k,2] - Observed[k,2])
    act_est[k,2] <- act_est[k,2] + abs(Actual[k,2] - Estimated[k,2])
  }
}

"plot output"
pdf("velocity1.pdf")
plot(Time[,1],Actual[,2],xlim = c(0,deltaT*nObs),ylim = c(Actual[nObs,2]-1,Observed[1,2]+1),
     xlab="time[s]", ylab="velocity[m/s]",col = "black",type="l")
par(new=T)
plot(Time[,1],Observed[,2],xlim = c(0,deltaT*nObs),ylim = c(Actual[nObs,2]-1,Observed[1,2]+1),
     xlab="", ylab="",col = "blue",type="l")
legend("topleft",legend=c("Actual","Observed"),col=c("black","blue"),lty=c(1,1))
dev.off()

pdf("velocity2.pdf")
plot(Time[,1],Actual[,2],xlim = c(0,deltaT*nObs),ylim = c(Actual[nObs,2]-1,Observed[1,2]+1),
     xlab="time[s]", ylab="velocity[m/s]",col = "black",type="l")
par(new=T)
plot(Time[,1],Estimated[,2],xlim =c(0,deltaT*nObs),ylim = c(Actual[nObs,2]-1,Observed[1,2]+1),
     xlab="", ylab="",col = "red",type="l")
legend("topleft",legend=c("Actual","Estimated"),col=c("black","red"),lty=c(1,1))
dev.off()

pdf("height1.pdf")
plot(Time[,1],Actual[,1],xlim = c(0,deltaT*nObs),ylim = c(Actual[nObs,1]-1,Observed[1,1]+1),
     xlab="time[s]", ylab="height[m]",col = "black",type="l")
par(new=T)
plot(Time[,1],Observed[,1],xlim = c(0,deltaT*nObs),ylim = c(Actual[nObs,1]-1,Observed[1,1]+1),
     xlab="", ylab="",col = "blue",type="l")
par(new=T)
legend("topleft",legend=c("Actual","Observed"),col=c("black","blue"),lty=c(1,1))
dev.off()

pdf("height2.pdf")
plot(Time[,1],Actual[,1],xlim = c(0,deltaT*nObs),ylim = c(Actual[nObs,1]-1,Observed[1,1]+1),
     xlab="time[s]", ylab="height[m]",col = "black",type="l")
par(new=T)
plot(Time[,1],Estimated[,1],xlim =c(0,deltaT*nObs),ylim = c(Actual[nObs,1]-1,Observed[1,1]+1),
     xlab="", ylab="",col = "red",type="l")
legend("topleft",legend=c("Actual","Estimated"),col=c("black","red"),lty=c(1,1))
dev.off()

pdf("error_velocity.pdf")
plot(Time[,1],act_obs[,2]/itr_num,xlim = c(0,deltaT*nObs),ylim=c(0,sum_obs/nObs*2),
     xlab="time[s]", ylab="error of velocity[m/s]",col = "blue",type="l")
par(new=T)
plot(Time[,1],act_est[,2]/itr_num,xlim = c(0,deltaT*nObs),ylim=c(0,sum_obs/nObs*2),
     xlab="", ylab="",col = "red",type="l")
legend("topleft",legend=c("Observed","Estimated"),col=c("blue","red"),lty=c(1,1))
dev.off()