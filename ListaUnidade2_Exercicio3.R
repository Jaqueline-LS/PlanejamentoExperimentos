X<-matrix(c(48,2,2,
            2,48,-24,
            2,-24,48), ncol=3, byrow = T)
y<-c(14989.9,9698.5005,10231.9)

z<-solve(X)%*%y

solve(X,y)

X<-matrix(c(32,2,
            2,32), ncol=2)
X
y<-c(6098,6542)
solve(X,y)
