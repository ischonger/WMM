#Aufgabe 1

fsin <- function(x){sin(x) * 15}
fcos <- function(x){cos(x) * (-8)}
fsum <- function(x){fsin(x) + fcos(x)}
fpol <- function(x){sqrt(15*15 + (-8)*(-8)) * cos(x + atan(-(15/(-8))))}
 
plot(fsin,0,13,main="Aufgabe 1", col='blue')
plot(fcos,0,13,add=TRUE, col='green')
plot(fsum,0,13,add=TRUE, col='yellow')
plot(fpol,0,13,add=TRUE, col='red',type='p')


#Aufgabe 2
#(a): Ein Feld mit sechs Wellen f_1,...,f_6
a <- function() {
        for(f in 1:6) {
          amp <- 1 / f;
          swing <- function(x){amp * sin(x*f)};
          plot(swing,add=TRUE,to=2*pi, ylab="y", main="(a)")
        }
      }
#(b): Ein Feld mit der Summenkurve f_1+f_2+f_3+f_4+f_5+f_6
b <- function() {
        for (f in 1:6) {
          amp <- 1 / f;
          swing <- function(x){amp * sin(x*f)};
        
        }
     }
