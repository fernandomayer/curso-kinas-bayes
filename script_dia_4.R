## BUGS

## exemplo das mulhares grávidas
## modelo X ~ Bin(n, teta)
## teta ~ Unif(0,1)
# lista com os dados
lddados <- list(n = 4, x = 0)
# parametros do modelo
params <- list("teta")
inits <- list(teta = 0.5)
# modelo para OpenBUGS
sink("ru486_MCMC.txt")
cat("model{
teta ~ dunif(0, 1)
x ~ dbin(teta, n)
}")
sink()
# usando
require(R2OpenBUGS)
fit <- bugs(lddados, inits, params, model.file = "ru486_MCMC.txt",
            n.chains = 1, n.iter = 25000, n.burnin = 20000, n.thin = 5)
print(fit, digits = 3)
names(fit)
sim <- fit$sims.matrix
par(mfrow=c(1,2))
hist(sim[,1])
hist(sim[,2])
par(mfrow=c(1,1))

######################################################################

##----------------------------------------------------------------------
## Exemplo dos red oaks
##----------------------------------------------------------------------

## Usando amostragem por importancia
##----------------------------------------------------------------------
y <- c(6,0,1,2,1,7,1,5,2,0)
## amostragem por importancia com
## p(teta) ~ logN(0.7, 1.4)
g.teta <- rlnorm(100000, 0.7, 1.4) # simula de uma logN mesmo
vero <- dpois(sum(y), length(y)*g.teta)
w <- vero # pois g(teta) = p(teta) => imp = priori
w.norm <- w/sum(w)
hist(w.norm)
sir <- sample(g.teta, size = 1000, replace = TRUE, prob = w.norm)
hist(sir)
quantile(sir, c(.025, .5, .975))
mean(sir); sd(sir)

## Usando MCMC
##----------------------------------------------------------------------
# Y_i ~ Poisson(mu), i = 1, ..., 10
# mu ~ logN(mediana = 0.7, sigma = 1.4) => priori
# dados
lddados <- list(y = c(6,0,1,2,1,7,1,5,2,0))
# parametros
params <- list("mu")
# inicio
init <- list(mu = 5)
# modelo
sink("red_oaks.txt")
cat("model{
# define a verossimilhança
for(i in 1:10){
y[i] ~ dpois(mu)
}
# no BUGS precisa especificar a variancia pela precisao: tau = 1/sigma^2
tau <- 1/(1.4*1.4)
mu ~ dlnorm(0.7, tau)
}")
sink()
# ajuste
fit.red <- bugs(lddados, inits, params, model.file = "red_oaks.txt",
            n.chains = 1, n.iter = 25000, n.burnin = 20000, n.thin = 5)
print(fit.red, digits = 3)
sim <- fit.red$sims.matrix
par(mfrow=c(1,2))
hist(sim[,1])
hist(sim[,2])
par(mfrow=c(1,1))
# media por hectare
mph1 <- sim[,1]*25
hist(mph1, nclass = 20)

# comparando SIR e MCMC
quantile(sir, c(.025, .5, .975))
quantile(sim[,1], c(.025, .5, .975))
c(mean(sir), sd(sir))
c(mean(sim[,1]), sd(sim[,1]))

## Diagnosticos de convergencia
require(coda)
diagn <- mcmc(sim)
plot(diagn)
cumuplot(diagn)
# analisa se o thinning funcionou
autocorr.plot(diagn)
crosscorr.plot(diagn)
# Geweke diagnostics - teste de diff de duas medias
geweke.diag(diagn)
heidel.diag(diagn)

######################################################################

## Inferência/Análise de decisão bayesiana

## Exemplo da mauracao do peixe galo
# dados
y <- c(0,1,3,5)
n <- c(3,5,4,5)
x <- c(12.5,22.5,32.5,62.5)
xb <- mean(x)
lddados <- list(y=y, n=n, x=x, xb=xb)
# parametros
params <- list("B0", "B1", "L50")
# iniciais: o L50 nao precisa porque ele eh calculado em fincao dos
# outros
inits <- list(c(B0 = 0, B1 = 0))
# y_i ~ Bin(n_i,p_i)
# logit(p_i) = beta0 + beta1*(x_i - xbarra)
# prioris::
# beta0 ~ N(0, 0.0001)
# beta1 ~ N(0, 0.0001)
# modelo
sink("peixe_galo.txt")
cat("model{
for(i in 1:4){
y[i] ~ dbin(p[i], n[i])
logit(p[i]) <- B0 + B1 * (x[i] - xb)
}
B0 ~ dnorm(0, 0.0001)
B1 ~ dnorm(0, 0.0001)
L50 <- -(B0/B1) + xb
}")
sink()
# ajuste
fit.galo <- bugs(lddados, inits, params, model.file = "peixe_galo.txt",
            n.chains = 1, n.iter = 25000, n.burnin = 20000, n.thin = 5)
print(fit.galo, digits = 3)
sim <- fit.galo$sims.matrix
par(mfrow=c(1,3))
hist(sim[,1])
hist(sim[,2])
hist(sim[,3])
par(mfrow=c(1,1))

# distribuicao conjunta de beta0 e beta1
plot(sim[,1], sim[,2])
## contourplot(~sim[,1] + sim[,2])
## contour(x = seq(min(sim[,1]), max(sim[,1]), length.out = nrow(sim)),
##         y = seq(min(sim[,2]), max(sim[,2]), length.out = nrow(sim)),
##         z = matrix(c(sim[,c(1,2)]), nrow = nrow(sim), ncol = ncol(sim)))

######################################################################

## Teste de hipotese

L50 <- sim[,"L50"]
hist(L50)
# H0: LT50 <= 30
# H1: LT50 > 30
(p0 <- sum(L50 <= 30)/length(L50))
(p1 <- sum(L50 > 30)/length(L50)) # p1 = 1 - p0
# se os pesos sao w0 = 1 e w1 = 2
w0 <- 1
w1 <- 2
w1/(w0+w1)
# portanto p0 > [w1/(w0+w1)] => nao rejeita H0
