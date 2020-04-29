##complete r file for analysis of timing data
##broken into sections
## 4/28/2020

##DATA#############
one.core <-c(0.268, 0.329,  0.464,  0.736,  1.266,  2.339,  4.461,  8.728,  17.306,  34.399,  68.566,  138.74,  276.541,  550.899,  1997.628)
four.core <-c(0.763, 0.793,  0.831,  0.882,  1.018,  1.299,  1.844,  2.891,  5.04,    9.372,   18.016,  35.415,  70.981,   139.58,   277.946)
eight.core <-c(0.986, 0.983,  1.016,  1.05,   1.117,  1.249,  1.507,  2.053,  3.143,   5.318,   9.634,   18.89,   37.038,   75.045,   144.961)
sixteen.core <-c(1.442, 1.44,   1.449,  1.472,  1.523,  1.575,  1.719,  2.018,  2.715,   4.257,   7.15,    13.326,  25.283,   50.088,   97.418)
tt.core <-c(2.462, 2.381,  2.406,  2.442,  2.452,  2.511,  2.578,  2.739,  3.146,   4.335,   7.35,    13.021,  23.552,   44.366,   82.618)

iterations <-c(1024, 2048,   4096,    8192,    16384,   32768,   65536,   131072,  262144,	524288,   1048576,  2097152,  4194304,   8388608,   16777216)

##LOG MANIPULATIONS
log.iterations =log2(iterations) #log base 2
log.one <-c(log(one.core)) #natural logs 
log.four <-c(log(four.core))
log.eight <-c(log(eight.core))
log.sixteen <-c(log(sixteen.core))
log.tt <-c(log(tt.core))
ln.2k = log(2000)


##log graph
plot(1, main="Logarithmic Plot of Timing Data (Time vs. Iterations)", type="n", xlab="log2(iterations)", ylab="ln(time in seconds)", xlim=c(10,log2(16777216)), ylim=c(-1, ln.2k))
lines(log.one ~ log.iterations, col="darkgreen", lwd=2)
points(log.one ~ log.iterations, col="darkgreen", pch=16)
lines(log.four ~ log.iterations, col="chartreuse3", lwd=2)
points(log.four ~ log.iterations, col="chartreuse3", pch=16)
lines(log.eight ~ log.iterations, col="darkcyan", lwd=2)
points(log.eight ~ log.iterations, col="darkcyan", pch=16)
lines(log.sixteen ~ log.iterations, col="red", lwd=2)
points(log.sixteen ~ log.iterations, col="red", pch=16)
lines(log.tt ~ log.iterations, col="blue", lwd=2)
points(log.tt ~ log.iterations, col="blue", pch=16)
legend(x=11, y=7, legend=c("one core", "four cores", "eight cores", "sixteen cores", "thirty-two cores (virtual)")
       , col=c("darkgreen", "chartreuse3", "darkcyan","red","blue"), lwd=2, pch=16, title="", box.lty = 0)
legend(11,7,c(" "," "),title="number of cores",cex=1.2, bty='n', title.adj=0.3, box.lty = 0)



##linear modeling of 4 cores
par(mfrow=c(1,1))
plot(1, main="General Linear Model
    of Four Core Performace", type="n", xlab="log2(iterations)", ylab="ln(time in seconds)", xlim=c(10,log2(16777216)), ylim=c(-1, ln.2k))
lines(log.four ~ log.iterations, col="chartreuse3", lwd=2)
points(log.four ~ log.iterations, col="chartreuse3", pch=16)

four.lm2 <-lm(formula = log(four.core) ~ log2(iterations))
abline(four.lm2$coef, lwd=2)
summary(four.lm2)
legend(10,7, c(" "," "),title = "Equation of Least Squares Line", box.lty = 0, cex=1.4, bg="transparent")
legend(10,5, legend=c("four cores","least sqaures line"),col=c("chartreuse3","black"), lwd=2,  box.lty = 0, bg="transparent")
legend(10,6.5, c("ln(y) = 0.4389*log2(x) - 5.7094"), box.lty = 0, bg="transparent")


plot(1, main="Linear model of Four Core Performance,
     for Iterations Greater Than 2^16", type="n", xlab="log2(iterations)", ylab="ln(time in seconds)", xlim=c(10,log2(16777216)), ylim=c(-1, ln.2k))
four.trunc <-c(2.891,  5.04,    9.372,   18.016,  35.415,  70.981,   139.58,   277.946)
trunc.iter <-c(131072,  262144,	524288,   1048576,  2097152,  4194304,   8388608,   16777216)
log.trunc4 <-log(four.trunc)
lines(log.trunc4 ~ log2(trunc.iter), col="chartreuse3", lwd=2)
trunlm4 <-lm(formula = log.trunc4 ~ log2(trunc.iter))
summary(trunlm4)
abline(trunlm4$coef, lwd=2)
abline(v=16)
legend(16.3,7, c(" "," "),title = "Equation of Least Squares Line", box.lty = 0, cex=1.3, bg="transparent")
legend(10,5, legend=c("four cores","least sqaures line"),col=c("chartreuse3","black"), lwd=2,  box.lty = 0, bg="transparent")
legend(16.3,6.5, c("ln(y) = 0.658534*log2(x) -10.224498 \nfor x = [2^16, infinity)"), box.lty = 0, bg="transparent")

##8 core models#################################################
par(mfrow=c(1,2))
eight.lm <-lm(formula = log(eight.core) ~ log2(iterations))
plot(1, main="General Linear Model
    of Eight Core Performace", type="n", xlab="log2(iterations)", ylab="ln(time in seconds)", xlim=c(10,log2(16777216)), ylim=c(-1, ln.2k))
lines(log.eight ~ log.iterations, col="darkcyan", lwd=2)
points(log.eight ~ log.iterations, col="darkcyan", pch=16)
abline(eight.lm$coefficients, lwd=2)
legend(10,7, c(" "," "),title = "Equation of Least Squares Line", box.lty = 0, cex=1.4, bg="transparent")
legend(10,5, legend=c("eight cores","least sqaures line"),col=c("darkcyan","black"), lwd=2,  box.lty = 0, bg="transparent")
legend(10,6.5, c("ln(y) = 0.35922*log2(x) - 4.61190"), box.lty = 0, bg="transparent")
summary(eight.lm)

#truncated lm8
plot(1, main="Linear model of Eight Core Performance,
     for Iterations Greater Than 2^17", type="n", xlab="log2(iterations)", ylab="ln(time in seconds)", xlim=c(10,log2(16777216)), ylim=c(-1, ln.2k))

lines(log.eight ~ log.iterations, col="darkcyan", lwd=2)
points(log.eight ~ log.iterations, col="darkcyan", pch=16)

trunc.8 <-c(  2.053, 3.143,   5.318,   9.634,   18.89,   37.038,   75.045,   144.961)
trunc.it8 <-c(   131072, 262144,	524288,   1048576,  2097152,  4194304,   8388608,   16777216)
trunlm8 <-lm(formula = log(trunc.8) ~ log2(trunc.it8))
abline(trunlm8$coefficients,lwd=2)
abline(v=17)
legend(17.2,7, c(" "," "),title = "Equation of \nLeast Squares Line", box.lty = 0, cex=1.2, bg="transparent")
legend(10,5, legend=c("eight cores","least sqaures line"),col=c("darkcyan","black"), lwd=2,  box.lty = 0, bg="transparent")
legend(16.3,6.5, c("ln(y) = 0.62096*log2(x) - 10.02391 \nfor x > 2^17"), box.lty = 0, bg="transparent")
summary(trunlm8)

##16 core model ##################################################################
par(mfrow=c(1,2))
sixteen.lm <-lm(formula = log(sixteen.core) ~ log2(iterations))
plot(1, main="General Linear Model
    of Sixteen Core Performace", type="n", xlab="log2(iterations)", ylab="ln(time in seconds)", xlim=c(10,log2(16777216)), ylim=c(-1, ln.2k))
lines(log.sixteen ~ log.iterations, col="red", lwd=2)
points(log.sixteen ~ log.iterations, col="red", pch=16)
abline(sixteen.lm$coefficients, lwd=2)
legend(10,7, c(" "," "),title = "Equation of Least Squares Line", box.lty = 0, cex=1.4, bg="transparent")
legend(10,5, legend=c("sixteen cores","least sqaures line"),col=c("red","black"), lwd=2,  box.lty = 0, bg="transparent")
legend(10,6.5, c("ln(y) = 0.35922*log2(x) - 4.61190"), box.lty = 0, bg="transparent")
summary(eight.lm)

#truncated lm16
plot(1, main="Linear model of Sixteen Core Performance,
     for Iterations Greater Than 2^18", type="n", xlab="log2(iterations)", ylab="ln(time in seconds)", xlim=c(10,log2(16777216)), ylim=c(-1, ln.2k))

lines(log.sixteen ~ log.iterations, col="red", lwd=2)
points(log.sixteen ~ log.iterations, col="red", pch=16)

trunc.16 <-c(  2.715,   4.257,   7.15,    13.326,  25.283,   50.088,   97.418)
trunc.it16 <-c(262144,	524288,   1048576,  2097152,  4194304,   8388608,   16777216)
trunlm16 <-lm(formula = log(trunc.16) ~ log2(trunc.it16))
abline(trunlm16$coefficients,lwd=2)
abline(v=18)
legend(10,7, c(" "," "),title = "Equation of \nLeast Squares Line", box.lty = 0, cex=1.2, bg="transparent")
legend(10,5, legend=c("sixteen cores","least sqaures line"),col=c("red","black"), lwd=2,  box.lty = 0, bg="transparent")
legend(10,6.5, c("ln(y) = 0.60479*log2(x) -10.02528 \nfor x = [2^18, infinity)"), box.lty = 0, bg="transparent")
summary(trunlm16)

####32 core################################
par(mfrow=c(1,2))
tt.lm <-lm(formula = log(tt.core) ~ log2(iterations))
plot(1, main="General Linear Model
    of Thirty Two Core Performace", type="n", xlab="log2(iterations)", ylab="ln(time in seconds)", xlim=c(10,log2(16777216)), ylim=c(-1, ln.2k))
lines(log.tt ~ log.iterations, col="blue", lwd=2)
points(log.tt ~ log.iterations, col="blue", pch=16)
abline(tt.lm$coefficients, lwd=2)
legend(10,7, c(" "," "),title = "Equation of Least Squares Line", box.lty = 0, cex=1.4, bg="transparent")
legend(10,5, legend=c("thirty-two cores","least sqaures line"),col=c("blue","black"), lwd=2,  box.lty = 0, bg="transparent")
legend(10,6.5, c("ln(y) = 0.23153*log2(x) - 2.21257"), box.lty = 0, bg="transparent")
summary(tt.lm)

#truncated lm32
plot(1, main="Linear model of Thirty Two Core Performance,
     for Iterations Greater Than 2^19", type="n", xlab="log2(iterations)", ylab="ln(time in seconds)", xlim=c(10,log2(16777216)), ylim=c(-1, ln.2k))
lines(log.tt ~ log.iterations, col="blue", lwd=2)
points(log.tt ~ log.iterations, col="blue", pch=16)
trunc.32 <-c(4.257,   7.15,    13.326,  25.283,   50.088,   97.418)
trunc.it32 <-c(524288,   1048576,  2097152,  4194304,   8388608,   16777216)
trunlm32 <-lm(formula = log(trunc.32) ~ log2(trunc.it32))
abline(trunlm32$coefficients,lwd=2)
abline(v=19)
legend(10,7, c(" "," "),title = "Equation of \nLeast Squares Line", box.lty = 0, cex=1.2, bg="transparent")
legend(10,5, legend=c("thirty-two cores","least sqaures line"),col=c("blue","black"), lwd=2,  box.lty = 0, bg="transparent")
legend(10,6.5, c("ln(y) = 0.63236*log2(x) - 10.64105 \nfor x = [2^19, infinity)"), box.lty = 0, bg="transparent")
summary(trunlm32)





##standard graph
par(mfrow=c(1,1))
plot(1, main="Plot of Timing Data (Time vs. Iterations)", type="n", xlab="iterations", ylab="time (s)", xlim=c(10,(16777216)), ylim=c(0, 2000))
lines(one.core ~ iterations, col="darkgreen", lwd=2)
points(one.core ~ iterations, col="darkgreen", pch=16)
lines(four.core ~ iterations, col="chartreuse3", lwd=2)
points(four.core ~ iterations, col="chartreuse3", pch=16)
lines(eight.core ~ iterations, col="darkcyan", lwd=2)
points(eight.core ~ iterations, col="darkcyan", pch=16)
lines(sixteen.core ~ iterations, col="red", lwd=3)
points(sixteen.core ~ iterations, col="red", pch=16)
lines(tt.core ~ iterations, col="blue", lwd=3)
points(tt.core ~ iterations, col="blue", pch=16)
legend(x=3500, y=1500, legend=c("one core", "four cores", "eight cores", "sixteen cores", "thirty-two cores (virtual)")
       , col=c("darkgreen", "chartreuse3", "darkcyan","red","blue"), lwd=2, pch=16, title="", box.lty = 0)
legend(3500,1500,c(" "," "),title="number of cores",cex=1.2, bty='n', title.adj=0.3, box.lty = 0)

#########SPEEDUP#############
SU1to4 <- one.core/four.core
SU1to8 <- one.core/eight.core
SU1to16 <- one.core/sixteen.core
SU1to32 <- one.core/tt.core

plot(SU1to32 ~ log2(iterations) , pch=16, lwd=2, col="blue", main="Speedup Produced from \n4, 8, 16 and 32 Cores by Number of Iterations", ylab="Speedup", xlab="Log2(Number of Iterations)")
lines(SU1to32 ~ log2(iterations), pch=16, lwd=2, col="blue")
points(SU1to8 ~ log2(iterations), pch=16, lwd=2, col="darkcyan")
lines(SU1to8 ~ log2(iterations) , pch=16, lwd=2, col="darkcyan")
points(SU1to16 ~log2(iterations), pch=16, lwd=2, col="red")
lines(SU1to16 ~ log2(iterations), pch=16, lwd=2, col="red" )
points(SU1to4 ~ log2(iterations), pch=16, lwd=2, col="chartreuse3" )
lines(SU1to4 ~ log2(iterations) , pch=16, lwd=2, col="chartreuse3")
abline(h=1, lty="dashed")
legend(x=11, y=20, legend=c( "four cores", "eight cores", "sixteen cores", "thirty-two cores (virtual)")
       , col=c( "chartreuse3", "darkcyan","red","blue"), lwd=2, pch=16, title="", box.lty = 0)
legend(11,20,c(" "," "),title="number of cores",cex=1.2, bty='n', title.adj=0.3, box.lty = 0)
legend(10.8,13.5, legend="break-even point", lwd=1, lty ="dashed",box.lty = 0)

#max speedup
max(SU1to32)
n <- length(SU1to8)
sort(SU1to32,partial=n-1)[n-1]

####converge
#3.07301204,3.0543013,3.02654248,3.07703214,2.96828252,3.14824971, 3.11620038,3.00622756,3.03036373,3.0282606,3.05758368,3.05963918,3.10702847,2.95121935,3.05346764,3.06555038,3.22177573,3.00683911,3.0876756,3.07847306

accept <-c(3.00212372,3.07010706,2.98663091,3.00265011,3.04868394,3.05752558,3.01758487,3.10164328,3.02472052,3.05105619,2.97719294,3.01336404,3.07365879, 3.09617736,2.96853623,3.00205041)
mean(accept)
##3.03943535

hist(accept)

##constant inflection point NEED UPDATING
#2^(ln2(numprocs)+14)

####application of modeling##########

##predict result of a single run with 32 cores at 2^24 iterations 
predict(trunlm32, newdata=data.frame(trunc.it32=16777216), interval = "predict") ## e^(result) is value in seconds
## 95% conf that completion time is between e^(4.535624) and e^(4.732151) --> (76.6382270566, 113.539523389) seconds
##the actual value is included in our CI
##large CI b/c not much data

##extrapolate further
## predict result of single 32 core run at 2^30 iterations
predict(trunlm32, newdata=data.frame(trunc.it32=1073741824), interval = "predict") ## e^(result) is value in seconds
## 95% conf that completion time is between e^(7.9634) and e^(8.696147) --> (2873.93945512, 5979.82749776) seconds
## confidence range is even larger, due to  

