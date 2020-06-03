library("ggplot2")
library("gridExtra")
library("latex2exp")

#########################################################
########### Limitations of whitening example ############
#########################################################

# Obtaining data
N = 5000
n_1 = rnorm(N, mean = 5, sd = sqrt(5)) 
n_2 = rnorm(N, mean = 5, sd = sqrt(5)) 
n = rbind(n_1,n_2)
s_1 = runif(N, min = -2, max = 2)
s_2 = runif(N, min = -2, max = 2)
s = rbind(s_1,s_2)
A = matrix(c(2,2,3,1), ncol = 2) #non-singular matrix
x = A %*% s # non-noisy data
xn = x #noisy
for (i in 1:N) {
  xn[,i] = x[,i] + n[,i] # noisy data
}
# run once (!) to obtain correct dimensions
x = t(x)
xn = t(xn)


# Whitening of non-noisy
cov = cov(x)
L = eigen(cov)$values
V = eigen(cov)$vectors
x_tilde = V %*% diag(L^(-1/2)) %*% t(V)  %*% t(x)
cov(t(x_tilde)) # Check the covariance is the identity

# Whitening of noisy without correction
cov = cov(xn)
L = eigen(cov)$values
V = eigen(cov)$vectors
xn_tilde = V %*% diag(L^(-1/2)) %*% t(V)  %*% t(x)
cov(t(xn_tilde)) #  The covariance NOT is the identity here


# Whitening of noisy with correction
cov(t(n))
cov = cov((t(n)-xn))
L = eigen(cov)$values
V = eigen(cov)$vectors
xn_tilde2 = V %*% diag(L^(-1/2)) %*% t(V)  %*% t(x)
cov(t(xn_tilde2)) # Check the covariance is the identity

# Collect in data frame
data = data.frame(s_1 = s_1, s_2 = s_2, 
                  x_1 = x[,1], x_2 = x[,2],
                  xn_1 = xn[,1], xn_2 = xn[,2],
                  x_tilde_1 = x_tilde[1,], x_tilde_2 = x_tilde[2,],
                  xn_tilde1_1 = xn_tilde[1,], xn_tilde1_2 = xn_tilde[2,],
                  xn_tilde2_1 = xn_tilde2[1,], xn_tilde2_2 = xn_tilde2[2,]
)
head(data)
# Generating plots
p1 = ggplot(data, aes(x = s_1, y = s_2))+ 
  geom_hex() + theme_bw() + theme(legend.position = "none") + 
  ylab(TeX("$s_1$")) + xlab(TeX("$s_2$")) + 
  labs(caption = "original components")
p2 = ggplot(data, aes(x = x_1, y = x_2))+ 
  geom_hex() + theme_bw() + theme(legend.position = "none") + 
  ylab(TeX("$x_1$")) + xlab(TeX("$x_2$")) + 
  labs(caption = "non-noisy mixed components") 
p3 = ggplot(data, aes(x = xn_1, y = xn_2))+ 
  geom_hex() + theme_bw() + theme(legend.position = "none") + 
  ylab(TeX("$x_1$'")) + xlab(TeX("$x_2$'")) + 
  labs(caption = "noisy mixed components") 
p4 = ggplot(data, aes(x = x_tilde_1, y = x_tilde_2))+ 
  geom_hex() + theme_bw() +  theme(legend.position = "none") + 
  ylab(TeX("$z_1$")) + xlab(TeX("$z_2$")) + 
  labs(caption = "non-noisy whitened components") 
p5 = ggplot(data, aes(x = xn_tilde1_1, y = xn_tilde1_2))+ 
  geom_hex() + theme_bw() + theme(legend.position = "none") + 
  ylab(TeX("$z_1$'")) + xlab(TeX("$z_2$'")) + 
  labs(caption = "noisy whitened components w/o correction") 
p6 = ggplot(data, aes(x = xn_tilde2_1, y = xn_tilde2_2))+ 
  geom_hex() + theme_bw() + theme(legend.position = "none") + 
  ylab(TeX("$z_1$'")) + xlab(TeX("$z_2$'")) + 
  labs(caption = "noisy whitened components w/ correction") 

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2) # Arrange in grid



