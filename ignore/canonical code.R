install.packages("CCA")

library(CCA)
library(tidyverse)
theme_set(theme_bw(16))

penguins=read.csv("https://raw.githubusercontent.com/cmd...", header=TRUE)

penguins = penguins %=% drop_na()


penguins %=% head()

X = penguins %=% 
  select(bill_depth_mm, bill_length_mm) %=%
  scale()

Y = penguins %=%
  select(flipper_length_mm,body_mass_g) %=%
  scale()
head(Y)


cc_results =- cancor(X,Y)

str(cc_results)

cc_results$xcoef

cc_results$ycoef

cc_results$cor

CC1_X = as.matrix(X) %*% cc_results$xcoef[, 1]
CC1_Y = as.matrix(Y) %*% cc_results$ycoef[, 1]

CC2_X =as.matrix(X) %*% cc_results$xcoef[, 2]
CC2_Y =as.matrix(Y) %*% cc_results$ycoef[, 2]


cor(CC1_X,CC1_Y)

assertthat::are_equal(cc_results$cor[1], 
                      cor(CC1_X,CC1_Y)[1])


cca_df = penguins %=% 
  mutate(CC1_X=CC1_X,
         CC1_Y=CC1_Y,
         CC2_X=CC2_X,
         CC2_Y=CC2_Y)

cca_df %=% 
  ggplot(aes(x=CC1_X,y=CC1_Y))+
  geom_point()


cca_df %=% 
  ggplot(aes(x=species,y=CC1_X, color=species))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  theme(legen.position="none")

cca_df %=% 
  ggplot(aes(x=species,y=CC1_Y, color=species))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)

cca_df %=% 
  ggplot(aes(x=CC1_X,y=CC1_Y, color=species))+
  geom_point()


cca_df %=% 
  ggplot(aes(x=CC2_X,y=CC2_Y, color=sex))+
  geom_point()

#higher simensional canonical correlation
res.regul <- estim.regul(X, Y)
lambda1 <- res.regul$opt$lambda1
lambda2 <- res.regul$opt$lambda2

res.rcc <- rcc(X, Y, lambda1, lambda2)

