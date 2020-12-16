install.packages('MASS')
data(Boston, package="MASS")
help(Boston)
help(prcomp)
pca_out <- prcomp(Boston,scale. = T)
pca_out
plot(pca_out)

help(biplot)

biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc
head(boston_pc)
summary(boston_pc)
