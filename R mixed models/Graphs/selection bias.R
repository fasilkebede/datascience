# Selection bias

n=200 # sample size

# Variable 1: y1 ~ N (0, 1)
y1<-rnorm(n, 0, 1)

# Variable 2: y2 ~ N (0, 1)
y2<-rnorm(n, 0, 1)

plot(y1,y2)

s=y1+y2
data<-cbind(y1, y2, s)
data<-as.data.frame(data)

sel=subset(data, s > 0)

plot(y1,y2)
cor(data$y1, data$y2)
cor(sel$y1, sel$y2)

plot(sel$y1, sel$y2)