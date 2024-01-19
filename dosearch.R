#################################################################################
#  dosearch.R                                                                   # 
#  Investigate recoverability of the ACE using dosearch package                 #
#################################################################################
library(dagitty)
library(dosearch)

coor <- list(x=c(U=1, Z1=2, Z2=2, X=3, Y=4, M_Z2=3, M_X=4, M_Y=5),                   
           y=c(U=0, Z1=-1,Z2=1, X=0,Y=0, M_Z2=-1.5, M_X=-1.5, M_Y=-1.5))

mDAGa <- dagitty("dag{X [exposure] Y [outcome] U [unobserved] 
                 X -> Y; Z1 -> X; Z1 -> Y; Z2 -> X; Z2 -> Y; U -> Z1; U -> Z2;  U -> X;
                 Z1 -> M_Z2; Z1 -> M_X; Z1 -> M_Y}") 

mDAGb <- dagitty("dag{X [exposure] Y [outcome] U [unobserved] 
                 X -> Y; Z1 -> X; Z1 -> Y; Z2 -> X; Z2 -> Y; U -> Z1; U -> Z2;  U -> X;
                 Z1 -> M_Z2; Z1 -> M_X; Z1 -> M_Y;
                 Z2 -> M_X; Z2 -> M_Y; X -> M_Z2; X -> M_Y}")

mDAGc <- dagitty("dag{X [exposure] Y [outcome] U [unobserved] 
                 X -> Y; Z1 -> X; Z1 -> Y; Z2 -> X; Z2 -> Y; U -> Z1; U -> Z2;  U -> X;
                 Z1 -> M_Z2; Z1 -> M_X; Z1 -> M_Y;
                 Z2 -> M_X; Z2 -> M_Y; X -> M_Z2; X -> M_Y; 
                 Y -> M_X; Y -> M_Z2}")

mDAGd <- dagitty("dag{X [exposure] Y [outcome] U [unobserved] 
                 X -> Y; Z1 -> X; Z1 -> Y; Z2 -> X; Z2 -> Y; U -> Z1; U -> Z2;  U -> X;
                 Z1 -> M_Z2; Z1 -> M_X; Z1 -> M_Y;
                 Z2 -> M_Z2; X -> M_X}")  

mDAGe <- dagitty("dag{X [exposure] Y [outcome] U [unobserved] 
                 X -> Y; Z1 -> X; Z1 -> Y; Z2 -> X; Z2 -> Y; U -> Z1; U -> Z2;  U -> X;
                 Z1 -> M_Z2; Z1 -> M_X; Z1 -> M_Y;
                 Z2 -> M_X; Z2 -> M_Y; X -> M_Z2; X -> M_Y; 
                 Z2 -> M_Z2; X -> M_X}")

mDAGf <- dagitty("dag{X [exposure] Y [outcome] U [unobserved] 
                 X -> Y; Z1 -> X; Z1 -> Y; Z2 -> X; Z2 -> Y; U -> Z1; U -> Z2;  U -> X;
                 Z1 -> M_Z2; Z1 -> M_X; Z1 -> M_Y;
                 Y -> M_X; Y -> M_Z2;
                 Z2 -> M_Z2; X -> M_X;}")  


dlist <- list(mDAGa, mDAGb, mDAGc, mDAGd, mDAGe, mDAGf)
labs <- c("A","B","C","D","E","F")

par(mfrow=c(3,2), oma=c(2,2,2,2))
for(i in 1:6)
{d <- dlist[[i]]
coordinates(d) <- coor
plot(d)
print(i)
print(impliedConditionalIndependencies(d))
legend("topleft", labs[i], bty="n", cex=1.5)
}  

data1 <- "P(X*, Y*, Z1, Z2*, M_X, M_Y, M_Z2)"
query1 <- "P(Y|do(X))"
query2 <- "P(Z1,Z2)"
query3 <- "P"
for(i in 1:10)
{d <- dlist[[i]]
print(labs[i])
out <- dosearch(data1, query1, d, missing_data = "M_X : X, M_Y : Y, M_Z2 : Z2")
print(out)
}

b <- dosearch(data1, query1, dlist[[2]], missing_data = "M_X : X, M_Y : Y, M_Z2 : Z2", control = list(draw_derivation = TRUE))
c <- dosearch(data1, query1, dlist[[3]], missing_data = "M_X : X, M_Y : Y, M_Z2 : Z2", control = list(draw_derivation = TRUE))
d <- dosearch(data1, query1, dlist[[4]], missing_data = "M_X : X, M_Y : Y, M_Z2 : Z2", control = list(draw_derivation = TRUE))
e <- dosearch(data1, query1, dlist[[5]], missing_data = "M_X : X, M_Y : Y, M_Z2 : Z2", control = list(draw_derivation = TRUE))
f <- dosearch(data1, query1, dlist[[6]], missing_data = "M_X : X, M_Y : Y, M_Z2 : Z2", control = list(draw_derivation = TRUE))
get_derivation(c, run_again = TRUE)


