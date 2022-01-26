install.packages("rgl")

library(rgl)

df <- read.csv("Analytical and numerical methods/prova cw 3.csv")

head(df)

plot3d( 
  x=df$`Z.tilde.ng`, y=data$`Z.tilde.Eg`, z=data$``, 
  col = data$color, 
  type = 's', 
  radius = .1,
  xlab="Sepal Length", ylab="Sepal Width", zlab="Petal Length")