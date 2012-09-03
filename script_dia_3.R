# testando beta-binomial
# n = 24, esp. A = 19, esp. B = 25, Y = ind esp. B, teta = prop. esp. B
teta <- seq(.01, .99, .01)
# posterior
p.teta <- dbeta(teta, 26, 20)
plot(teta, p.teta, type = "l")
abline(v = .5)
# qual a probabilidade da especie ser B: a area da posterior > 0.5
pbeta(0.5, 26, 20, lower.tail = FALSE)
# razao de chances
pbeta(0.5, 26, 20, lower.tail = FALSE)/pbeta(0.5, 26, 20) # 4:1
# intervalo de credibilidade
qbeta(c(.025, .5, .975), 26, 20)

## Problema 4.9, pg. 144
## X = num avistagens = 10
## teta = num medio avistagens/10 MN
## n = um unidades amostradas = 15 unidades de 10 MN

## teta = gama(alpha, beta)
## teta|X = gama(alpha.star = alpha+X, beta.star = beta+n)

## A) priori nao informativa
## teta ~ gama(alpha = 0.001, beta = 0.001)
## teta|X ~ gama(alpha.star = 10, beta.star = 15)
alpha.star <- 10
beta.star <- 15
qgamma(c(.025, .5, .975), alpha.star, beta.star)
## E(X) = alpha.star/beta.star
alpha.star/beta.star
## V(X) = alpha.star/(beta.star)^2
alpha.star/(beta.star^2)
## DP(X) = sqrt(alpha.star/(beta.star)^2)
sqrt(alpha.star/(beta.star^2))
sqrt(alpha.star)/beta.star
## visualizacao da posterior
teta <- seq(0, 1.5, .01)
p.teta <- dgamma(teta, alpha.star, beta.star)
plot(teta, p.teta, type = "l")
abline(v = qgamma(c(.025, .5, .975), alpha.star, beta.star),
       lty = 2, col = 2)

## B) priori informativa
## E(X) = alpha/beta = 0.45
## Moda = alpha-1/beta = 0.4
## alpha = 20, beta = 9
## teta ~ gama(alpha = 20, beta = 9)
## teta|X ~ gama(alpha.star = 19, beta.star = 35)
alpha.star <- 19
beta.star <- 35
qgamma(c(.025, .5, .975), alpha.star, beta.star)
## E(X) = alpha.star/beta.star
alpha.star/beta.star
## V(X) = alpha.star/(beta.star)^2
alpha.star/(beta.star^2)
## DP(X) = sqrt(alpha.star/(beta.star)^2)
sqrt(alpha.star/(beta.star^2))
sqrt(alpha.star)/beta.star
## visualizacao da posterior
teta <- seq(0, 1.5, .01)
p.teta.inf <- dgamma(teta, alpha.star, beta.star)
plot(teta, p.teta.inf, type = "l")
abline(v = qgamma(c(.025, .5, .975), alpha.star, beta.star),
       lty = 2, col = 2)

## Plotando as duas posteriores
plot(teta, p.teta.inf, type = "l")
lines(teta, p.teta, type = "l", lty = 2)


## Problema 4.4, pg. 112
# alto vale
mu1A <- 17.32
lambda1A <- 5
alpha1A <- 2
beta1A <- 22
# baixo vale
mu1B <- 12.56
lambda1B <- 14
alpha1B <- 6.5
beta1B <- 160

## Define uma funcao para simular a posterior de uma normal gama
mu.NGpost <- function(m, mu1, lambda1, alpha1, beta1){
    tau <- rgamma(m, alpha1, beta1)
    mu <- rep(0, m)
    for(i in 1:m){
        sigma <- 1/sqrt(lambda1*tau[i])
        mu[i] <- rnorm(1, mu1, sigma)
    }
    return(mu)
}

# alto vale
muA <- mu.NGpost(m = 2000, mu = mu1A, lambda1 = lambda1A,
                 alpha1 = alpha1A, beta1 = beta1A)
hist(muA)
quantile(muA)
quantile(muA, c(.025, .5, .975))
c(mean(muA), sd(muA))

# baixo vale
muB <- mu.NGpost(m = 2000, mu = mu1B, lambda1 = lambda1B,
                 alpha1 = alpha1B, beta1 = beta1B)

hist(muB)
quantile(muB)
quantile(muB, c(.025, .5, .975))
c(mean(muB), sd(muB))

## distribuicoes comparadas
histogram(~ muA + muB, layout = c(1,2), table = FALSE)
par(mfrow=c(2,1))
hist(muA)
hist(muB)
par(mfrow=c(1,1))

# densidades comparadas
plot(density(muB), xlim = c(6,26))
lines(density(muA), lty = 2)

# distribuicao posterior para as diferenças
delta <- muA - muB
hist(delta)
abline(v = 0, lty = 2, col = 2)
quantile(delta, c(.025, .5, .975))
c(mean(delta), sd(delta))
# probabilidade de delta ser positivo P(delta>0)
length(delta[delta > 0])/length(delta)
sum(delta > 0)/length(delta)
# razao de chances
(sum(delta > 0)/length(delta))/(1-(sum(delta > 0)/length(delta))) # 30:1

######################################################################

## Amostragem por importancia

## exemplo das mulhers gravidas
g.teta <- runif(100000, 0, 1) # U(0,1)
vero <- dbinom(0, 4, g.teta)
priori <- dbeta(g.teta, 1, 1) # nao informativa Beta(1,1)
imp <- dunif(g.teta)
w <- vero*priori/imp
w.norm <- w/sum(w)
sum(g.teta*w.norm)
# sabe-se que essa posterior eh uma Beta(1,5), e E(X) =
# alpha/(alpha+beta)
1/6

# probabilidade de ser > 0.5 eh o somatorio dos pesos com g.teta > 0.5
sum(w.norm[g.teta > 0.5])
pbeta(0.5, 1, 5, lower.tail = FALSE)

# re-amostragem por importancia
g.teta.sir <- sample(g.teta, 1000, replace = TRUE, prob = w.norm)
hist(g.teta.sir)

# para verificar a distribuicao dos dados simulados
# criar um grafico QQ
# quantis teoricos
qteor <- (1:length(g.teta.sir))/(1+length(g.teta.sir)) # 1000/1001
teta.teo <- qbeta(qteor, 1, 5)
teta.emp <- sort(g.teta.sir)
plot(teta.teo, teta.emp)
abline(0, 1) # bissetriz
qqplot(teta.teo, teta.emp)

## Exercicio dos red oaks
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

######################################################################

## MCMC

# simulacao de um passaro se deslocando entre 4 ilhas
pos <- numeric()
# primeiro dia: ilha 2
pos[1] <- 2

# matriz de transição
M <- matrix(c(.5,.3,.2,0,.2,.4,.3,.1,.1,.1,.6,.2,.1,.2,.3,.4),
            byrow = TRUE, ncol = 4)

# altera as posicoes de acordo com M
for(i in 2:70000){
    pos[i] <- sample(1:4, size = 1, prob = M[pos[i-1], ])
}
plot(pos[1:100], type = "l")

# tira os 10000 primeiros valores para eliminar a influencia do valor
# inicial e calcula as frequencias relativas
table(pos[10001:70000])/60000


