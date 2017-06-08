# Aufgabe 2

shannon <- function (x, fj, SF, padding=FALSE)
{
  sinc <- function (t) ifelse (t==0, 1, sin(t)/t)
  fx <- rep (0, length(x))
  for (j in seq(along=fj))
    fx <- fx + fj[j] * sinc (pi * (x-j/SF) * SF)
  if (padding)
    for (j in seq(along=fj))
    {
      fx <- fx + fj[j] * sinc (pi * (x-(j-length(fj))/SF) * SF)
      fx <- fx + fj[j] * sinc (pi * (x-(j+length(fj))/SF) * SF)
    }
  return (fx)
}
# code from file 'shannon.R'

#source('shannon.R');

# (a)
x  <- seq(0.001,0.025,by=0.005);
fx <- seq(0.001,0.025,by=0.01);
SF <- (1/fx);

par(mfrow=c(1,2));
shannonNoPadding <- shannon(x, fx, SF, padding=FALSE); plot(shannonNoPadding, type='l');
shannonPadding   <- shannon(x, fx, SF, padding=TRUE); plot(shannonPadding, type='l');
dist <- shannonPadding - shannonNoPadding; dist

par(mfrow=c(2,2));
interval  <- seq(0.1, 2*pi, length=100)
finterval <- seq(0.1, 2*pi, length=10)
shannonNoPadding <- shannon(sin(interval), finterval, (1/finterval), padding=FALSE); plot(shannonNoPadding, type='l');
shannonPadding   <- shannon(sin(interval), finterval, (1/finterval), padding=TRUE); plot(shannonPadding, type='l');
distSin <- shannonPadding - shannonNoPadding; distSin

shannonNoPadding <- shannon(cos(interval), finterval, (1/finterval), padding=FALSE); plot(shannonNoPadding, type='l');
shannonPadding   <- shannon(cos(interval), finterval, (1/finterval), padding=TRUE); plot(shannonPadding, type='l');
distSin <- shannonPadding - shannonNoPadding; distSin

# bei padding wird noch etwas hinzuaddiert.
# (j-length(fj))/SF:
# (j+length(fj))/SF:
# padding=FALSE: anhand der Spitzen (da, wo abgetastet wird) kann der ursprgl Funktionsverlauf nachvollzogen werden
# die Wertverläufe bei padding=TRUE liegen höherwertiger. an ihnen ist keine ähnlichkeit zur ursprgl Funktion er-
# kennbar.
# ...


# (b)
resampling <- function(fun, sf, tmax, m) {
  z <- seq(0, tmax, length=sf)
  x <- seq(0, tmax, length=m);
  s <- shannon(fun(x), x, m, padding=TRUE);
  s <- shannon(fun(x), x, m, padding=FALSE);
  
  
  #Abtastwerte
  plot(z, fun(z), from=0, to=tmax, xlab = "sin", col='red');
  par(new=TRUE);
  #ursprgl Signalverlauf
  plot(fun, from=0, to=tmax, xlab = "sin");
  #reproduzierter Signalverlauf der Abtastwerte
  par(new=TRUE);
  #plot(s/tmax, seq(0,tmax,length=m), type='l', col="green");
  quot <- (s/tmax);
  #plot(seq(0,tmax,length=m), (quot/max(quot)), type='l', col="green");
  plot(seq(0,tmax,length=m), (quot/max(quot)), type='l', col="green");
  
  
  #plot(z, fun(z), type='p', from=0, to=tmax, col='red', add=TRUE);
  #plot(sin,0,1);plot(cos,0,1,add=TRUE);
  #plot(x, shannon(x, (1/x), length(x)), type='b', from=0, to=tmax, col='blue');
  
  #lines(s, type='l', add=TRUE, col="green", add=TRUE);
  return(fun)
}

dev.off();
par(mfrow=c(1,2));
resampling(sin, 10, pi, 200); 
resampling(cos, 10, pi, 500);

# c
dev.off();
par(mfrow=c(1,1));
sinSwing <- function(x, amp=1, f=96,phase=0) { amp * sin((2*pi*f)*x + phase) }
# c1
resampling(sinSwing, 880, 0.25, 200)

saegezahn <- function(x, f=96) {
  x - floor(f*x) }
plot(saegezahn, type='l')