"与えられたデータをcsvファイルに取りこみ,
1列目に記録のデータを保存した.
(ただし,ヘッダー付き)"
data = read.csv("data.csv")
x <- data[,1]

"生データのヒストグラムを表示"
hist(x,breaks=12,main="Histogram of Record",xlab="distance[m]")

"logを取った確率密度関数"
log_likelihood <- function(x, mu, sigma, alpha) {
  sum(log(alpha[1] * dnorm(x, mu[1], sqrt(sigma[1])) +
            alpha[2] * dnorm(x, mu[2], sqrt(sigma[2]))))
}

"平均,分散,割合の
初期パラメータを大体で予想して代入"
mu <- c(30, 15)
sigma <- c(15, 15)
alpha <- c(0.5, 0.5)
n_k <- c()
N <- length(x)
epsilon <- 0.00001
itr_times <- 10000
log_likelihood_history <- c()

"EMアルゴリズムメイン部分"
for(step in 1:itr_times) {
  old_log_likelihood <- log_likelihood(x, mu, sigma, alpha)
  log_likelihood_history <- c(log_likelihood_history, old_log_likelihood)
  
  # E-step
  gamma_1 <- alpha[1] * dnorm(x, mu[1], sqrt(sigma[1])) /
    (alpha[1] * dnorm(x, mu[1], sqrt(sigma[1])) + alpha[2] * dnorm(x, mu[2], sqrt(sigma[2])))
  gamma_2 <- 1 - gamma_1
  
  # M-step
  n_k[1] <- sum(gamma_1)
  n_k[2] <- sum(gamma_2)
  mu[1] <-  sum(gamma_1 * x) / n_k[1]
  mu[2] <-  sum(gamma_2 * x) / n_k[2]
  sigma[1] <- sum(gamma_1 * (x - mu[1])^2) / n_k[1]
  sigma[2] <- sum(gamma_2 * (x - mu[2])^2) / n_k[2]
  alpha[1] <- n_k[1] / N
  alpha[2] <- 1 - alpha[1]
  if(abs(log_likelihood(x, mu, sigma, alpha) - old_log_likelihood) < epsilon){
    break    
  }
}

alpha
mu
sigma
std <- c(sigma[1]**0.5,sigma[2]**0.5)

plot.new()
hist(x,breaks=12,main="Histogram of Record",xlab="distance[m]",prob=T)
curve(alpha[1]*dnorm(x,mu[1],std[1]),add=TRUE, col="blue")
curve(alpha[2]*dnorm(x,mu[2],std[2]),add=TRUE, col="red")
curve(alpha[1]*dnorm(x,mu[1],std[1])+alpha[2]*dnorm(x,mu[2],std[2]),add=TRUE, col="black")
legend("topright",legend=c("female","male","all"),col=c("red","blue","black"),lty=c(1,1,1,1))

"対数尤度の推移グラフ"
plot(log_likelihood_history, pch=1, type="b", ylab="Log Likelihood")