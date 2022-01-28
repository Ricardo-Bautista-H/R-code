##########Sesión 3 - Tarea#############
#1
set.seed(1)
z = w = rnorm(100, sd=20)
for (t in 2:100) z[t] = 0.7*z[t-1] + w[t]
Time = 1:100
x = 30 + 3.5*Time+z

#2
x.simul = lm(x ~ Time)
coef(x.simul)
summary(x.simul)

#3
plot(x, xlab = "time", main= "Serie simulada y Regresionada", type = "l")
abline(36.31173, 3.531705, col = "red", lty=2)