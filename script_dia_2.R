## exemplo das mulheres grávidas
x <- 0:4
px <- dbinom(x, size = 4, prob = 1/2)
plot(x, px, type = "h")


## versossimilhança Poisson, X=4, n=12
teta <- seq(0.01, 3, 0.02)
# sem a cte normalizadora
l.teta <- exp(-12*teta) * teta^4
plot(teta, l.teta, type = "l")
ll.teta <- -12*teta + 4*log(teta)
plot(teta, ll.teta, type = "l")
par(mfrow=c(1,2))
plot(teta, l.teta, type = "l")
plot(teta, ll.teta, type = "l")
par(mfrow=c(1,1))
# estimador de maxima verossimilhança
teta[l.teta == max(l.teta)]
teta[ll.teta == max(ll.teta)]
# nesse caso eh igual a media da Poisson x/n
4/12

## simulação normal-gama, exemplo dos leitões
# simula priori p(\tau) ~ gama
m <- 1000 # numero de simulacoes
mu1 <- 31.36
alpha1 <- 12
beta1 <- 1254.54
lambda1 <- 22

tau <- rgamma(m, shape = alpha1, rate = beta1)
hist(tau)

# simula priori p(\mu|\tau) ~ normal
mu <- rep(0, m)
for(i in 1:m){
    sigma <- 1/sqrt(22*tau[i]) # 1/sqrt(\tau) = sigma (desvio-padrao)
    mu[i] <- rnorm(1, mu1, sigma)
}

hist(mu, nclass = 20) # t-studemt nao central
plot(mu, 1/sqrt(tau)) # mu, desvio padrao

# intervalo de credibilidade
quantile(mu, c(.025, .975))
abline(v = quantile(mu, c(.025, .975)), lty = 2, col = 2)

## Exercício usando uma priori vaga
alpha0 <- beta0 <- 0.001
lambda0 = 0.0001
mu0 = 0
n <- 20
xbarra <- 30

(lambda0*mu0 + n*xbarra)/(lambda0+n)

# os parametros ficam
m <- 1000
mu1 <- 30
alpha1 <- 10
beta1 <- 950
lambda1 <- 20

tau <- rgamma(m, shape = alpha1, rate = beta1)
hist(tau)

# simula priori p(\mu|\tau) ~ normal
mu <- rep(0, m)
for(i in 1:m){
    sigma <- 1/sqrt(22*tau[i]) # 1/sqrt(\tau) = sigma (desvio-padrao)
    mu[i] <- rnorm(1, mu1, sigma)
}

hist(mu, nclass = 20) # t-studemt nao central
plot(mu, 1/sqrt(tau)) # mu, desvio padrao

# intervalo de credibilidade
quantile(mu, c(.025, .975))
abline(v = quantile(mu, c(.025, .975)), lty = 2, col = 2)
