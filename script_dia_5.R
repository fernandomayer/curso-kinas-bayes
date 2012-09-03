## Modelos lineares

## Exemplo de rolos compressores
peso <- c(1.9,3.1,3.3,4.8,5.3,6.1,6.4,7.6,9.8,12.4)
dep <- c(2,1,5,5,20,20,23,10,30,25) # depressao
plot(peso, dep)

## ajuste bayesiano
lddados <- list(n = 10, y = dep, x = (peso - mean(peso)))
## parametros
params <- list("b0", "b1", "sigma2")
## iniciais
inits <- list(c(b0 = mean(dep), b1 = 0, tau = 1))
## modelo
sink("roller.txt")
cat("model{
# verossimilhança: m = media
for(i in 1:n){
m[i] <- b0 + b1*x[i]
y[i] ~ dnorm(m[i], tau)
}
# prioris
b0 ~ dnorm(0, 1.0E-6)I(-50,50) # o I coloca uma restricao nos params
b1 ~ dnorm(0, 1.0E-6)I(-50,50)
tau ~ dgamma(0.01, 0.01)
sigma2 <- 1/tau
}")
sink()
## ajuste
#require(R2OpenBUGS)
fit.roller <- bugs(lddados, inits, params, model.file = "roller.txt",
                   n.iter = 25000, n.burnin = 20000, n.chains = 1,
                   n.thin = 5)
print(fit.roller, digits = 3)

sim.roller <- fit.roller$sims.matrix
par(mfrow=c(2,2))
hist(sim.roller[,1])
hist(sim.roller[,2])
hist(sim.roller[,3])
hist(sim.roller[,4])
par(mfrow=c(1,1))
# intervalos
quantile(sim.roller[,1], c(.025, .5, .975))
quantile(sim.roller[,2], c(.025, .5, .975))
quantile(sim.roller[,3], c(.025, .5, .975))

## Diagnosticos de convergencia
#require(coda)
diagn <- mcmc(sim.roller[,1:3])
plot(diagn)
cumuplot(diagn)
# analisa se o thinning funcionou
autocorr.plot(diagn)
crosscorr.plot(diagn)
# Geweke diagnostics - teste de diff de duas medias
geweke.diag(diagn)
heidel.diag(diagn)

######################################################################

## GLM!

## Exemplo dos red oaks
