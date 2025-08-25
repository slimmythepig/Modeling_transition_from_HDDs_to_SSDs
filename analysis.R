
# Import required libraries
library(tseries)
library(DIMORA)

# Import data about hard disk drives (HDD) shipments
HDD.shipments <- read.csv("HDD_unit_shipments_worldwide.csv", header = T)
colnames(HDD.shipments) <- c("year", "HDDshipment")

# Convert data to a time series
HDD.ts <- ts(HDD.shipments$HDDshipment)

# Display data
tsdisplay(HDD.ts)

pdf("plots/HDD-ACF.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
Acf(HDD.ts, main="")
par(op)
dev.off()

pdf("plots/HDD-ts.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, HDD.shipments$HDDshipment, type = "l",
     xlab = "Year", ylab = "HDD unit shipments (10^6)")
par(op)
dev.off()


#-------------------------------------------------------------------------------
# Simple Bass model
#-------------------------------------------------------------------------------
# We fit a simple Bass model to map the progress in HDD shipments since
# they entered the market. We aim to obtain the estimated market potential m, 
# the innovation factor p and the imitation factor q.

bass.mod <- BM(HDD.ts, 
               prelimestimates = c(sum(HDD.ts)*1.1, 0.01, 0.1),
               display = F)
summary(bass.mod)
m.bm <- bass.mod$coefficients[[1]]
p.bm <- bass.mod$coefficients[[2]]
q.bm <- bass.mod$coefficients[[3]]
pq.ratio.bm <- p.bm/q.bm
pq.ratio.bm

# Cumulative process
pred.HDD.cumulative.bm <- fitted(bass.mod)
pdf("plots/HDD-bass-cumulative.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, cumsum(HDD.ts),
     xlab="Year", ylab="Cumulative HDD unit shipments (10^6)", 
     pch=2, cex=1)
lines(HDD.shipments$year, pred.HDD.cumulative.bm, lw=2, col="blue")
legend("topleft", legend=c("Observed data", "BM"), 
       col=c("black", "blue"), pch=c(2,NA),
       lty=c(NA, "solid"), lw=c(NA,2))
par(op)
dev.off()

# Instantaneous process
pred.HDD.instantaneous.bm <- make.instantaneous(pred.HDD.cumulative.bm)
pdf("plots/HDD-bass-instant.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, HDD.ts, 
     xlab = "Year", ylab = "HDD unit shipments (10^6)", 
     pch=6, cex=1)
lines(HDD.shipments$year, pred.HDD.instantaneous.bm, lw=2, col="blue")
legend("topleft", legend=c("Observed data", "BM"), 
       col=c("black", "blue"), pch=c(2,NA),
       lty=c(NA, "solid"), lw=c(NA,2))
par(op)
dev.off()


#-------------------------------------------------------------------------------
# Generalized Bass model with rectangular shock
#-------------------------------------------------------------------------------
# We try to add a shock to the process to model the new increase in shipments
# observed in the years after ~2003. This new acceleration might be due to the 
# increase in global PC shipments observed in the same years 
# (see: https://www.jonpeddie.com/blog/what-happened-in-2010-to-2012/)

m0 <- m.bm # preliminary value for market potential (est. from simple BM)
p0 <- p.bm # preliminary value for innovation (est. from simple BM)
q0 <- q.bm # preliminary value for imitation (est. from simple BM)
a0 <- which(HDD.shipments$year==2003) # preliminary value for shock start 
b0 <- which(HDD.shipments$year==2007) # preliminary value for shock end 
c0 <- 0.1 # preliminary value for shock intensity
Gbass.mod <- GBM(HDD.ts, 
                 shock = "rett", 
                 prelimestimates = c(m0, p0, q0, a0, b0, c0), 
                 nshock = 1,
                 display = F)
summary(Gbass.mod)
m.gbm <- Gbass.mod$coefficients[[1]]
p.gbm <- Gbass.mod$coefficients[[2]]
q.gbm <- Gbass.mod$coefficients[[3]]
a.gbm <- Gbass.mod$coefficients[[4]]
b.gbm <- Gbass.mod$coefficients[[5]]
c.gbm <- Gbass.mod$coefficients[[6]]
pq.ratio.gbm <- p.gbm/q.gbm
pq.ratio.gbm

# Cumulative process
pred.HDD.cumulative.gbm <- fitted(Gbass.mod)
pdf("plots/HDD-Gbass-cumulative.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, cumsum(HDD.ts),
     xlab="Year", ylab="Cumulative HDD unit shipments (10^6)", 
     pch=2, cex=1)
lines(HDD.shipments$year, pred.HDD.cumulative.gbm, 
      lw=2, col="blue")
lines(HDD.shipments$year, pred.HDD.cumulative.bm, 
      lw=2, col="red", lty="dotdash")
legend("topleft", legend=c("BM", "GBM"), 
       col=c("red", "blue"), lty=c("dotdash", "solid"), lw=c(2,2))
par(op)
dev.off()

# Instantaneous process
pred.HDD.instantaneous.gbm <- make.instantaneous(pred.HDD.cumulative.gbm)
pdf("plots/HDD-Gbass-instant.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, HDD.ts, 
     xlab = "Year", ylab = "HDD unit shipments (10^6)", 
     pch=6, cex=1)
lines(HDD.shipments$year, pred.HDD.instantaneous.gbm, 
      lw=2, col="blue")
lines(HDD.shipments$year, pred.HDD.instantaneous.bm, 
      lw=2, col="red", lty="dotdash")
legend("topleft", legend=c("BM", "GBM"), 
       col=c("red", "blue"), lty=c("dotdash", "solid"), lw=c(2,2))
par(op)
dev.off()

# Residuals analysis

residuals.gbm <- HDD.ts - pred.HDD.instantaneous.gbm
pdf("plots/HDD-Gbass-residuals.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, residuals.gbm, 
     xlab="Year", ylab="Residuals")
par(op)
dev.off()
Acf(residuals.gbm)
Pacf(residuals.gbm)
# Residuals of the instantaneous time series show a funnel shape, 
# suggesting that the Generalized Bass model might still be missing something.
# However, the increase in variance seems inherent to the data.


#-------------------------------------------------------------------------------
# Guseo-Guidolin model
#-------------------------------------------------------------------------------
# We fit GGM model to check whether the Bass model improves when we 
# assume that the market potential is not constant.

GG.mod <- GGM(HDD.ts, display = F)
summary(GG.mod)
pcqc.ratio <- as.numeric(coefficients(GG.mod)[2]/coefficients(GG.mod)[3])
pcqc.ratio
psqs.ratio <- as.numeric(coefficients(GG.mod)[4]/coefficients(GG.mod)[5])
psqs.ratio

# Cumulative process
pred.HDD.cumulative.ggm <- fitted(GG.mod)
pdf("plots/HDD-ggm-cumulative.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, cumsum(HDD.ts),
     xlab="Year", ylab="Cumulative unit shipments (10^6)", 
     pch=2, cex=1)
lines(HDD.shipments$year, pred.HDD.cumulative.ggm, 
      lw=2, col="blue")
lines(HDD.shipments$year, pred.HDD.cumulative.bm, 
      lw=2, col="red", lty="dotdash")
legend("topleft", legend=c("BM", "GGM"), 
       col=c("red", "blue"), lty=c("dotdash", "solid"), lw=c(2,2))
par(op)
dev.off()

# Instantaneous process
pred.HDD.instantaneous.ggm <- make.instantaneous(pred.HDD.cumulative.ggm)
pdf("plots/HDD-ggm-instant.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, HDD.ts, 
     xlab = "Year", ylab = "Unit shipments (10^6)", 
     pch=6, cex=1)
lines(HDD.shipments$year, pred.HDD.instantaneous.ggm, 
      lw=2, col="blue")
lines(HDD.shipments$year, pred.HDD.instantaneous.bm, 
      lw=2, col="red", lty="dotdash")
legend("topleft", legend=c("BM", "GGM"), 
       col=c("red", "blue"), lty=c("dotdash", "solid"), lw=c(2,2))
par(op)
dev.off()

# Residuals analysis
residuals.ggm <- HDD.ts - pred.HDD.instantaneous.ggm
plot(HDD.shipments$year, residuals.ggm, 
     xlab="Year", ylab="Residuals")
Acf(residuals.ggm)
Pacf(residuals.ggm)


#-------------------------------------------------------------------------------
# Dynamic regression 
#-------------------------------------------------------------------------------
# Now we use the global PC unit shipments as predictor and we model
# the errors with an Arima, to see to what extent PC shipments
# can explain the increase in HDD shipments.

# Import data
PC.shipments <- read.csv("PC_unit_shipments_worldwide.csv", header=T)
range(PC.shipments$Year)

# Convert data to a time series
PC.ts <- ts(PC.shipments$PC.unit.shipments)

# Display data
tsdisplay(PC.ts)

# Fix the two time series to span the same period (2006-2022) 
HDD.ts.9422 <- HDD.ts[19:47]
PC.ts <- PC.ts[1:29]

# Fit the model
dynreg.mod <- auto.arima(HDD.ts.9422, seasonal=F, xreg=PC.ts)
summary(dynreg.mod)

# Cumulative process
pred.HDD.cumulative.dynreg <- cumsum(dynreg.mod$fitted)
pdf("plots/dynreg-cumulative.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments[19:47, "year"], cumsum(HDD.ts.9422),
     xlab = "Year", ylab = "Cumulative unit shipments (10^6)",
     pch=2, cex=1)
lines(HDD.shipments[19:47, "year"], pred.HDD.cumulative.dynreg,
      col = "blue", lwd = 2)
legend("topleft", legend=c("HDD", "Dyn. Reg. Arima(2,0,1) errors"), 
       col=c("black", "blue"), pch=c(2,NA),
       lty=c(NA,"solid"), lw=c(NA,2))
par(op)
dev.off()

# Instantaneous process
pred.HDD.instantaneous.dynreg <- dynreg.mod$fitted
pdf("plots/dynreg-instant.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments[19:47, "year"], HDD.ts.9422,
     xlab = "Year", ylab = "Unit shipments (10^6)", ylim=c(0,700),
     pch=2, cex=1)
lines(HDD.shipments[19:47, "year"], pred.HDD.instantaneous.dynreg,
      col = "blue", lwd = 2)
legend("bottomright", inset = c(0.1, 0), 
       legend=c("HDD", "Dyn. Reg. Arima(2,0,1) errors"), 
       col=c("black", "blue"), pch=c(2,NA),
       lty=c(NA,"solid"), lw=c(NA,2))
par(op)
dev.off()

residuals.dynreg <- HDD.ts.9422 - pred.HDD.instantaneous.dynreg
pdf("plots/HDD-dynreg-residuals.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year[19:47], residuals.dynreg, 
     xlab="Year", ylab="Residuals")
par(op)
dev.off()
Acf(residuals.dynreg)
Pacf(residuals.dynreg)


#-------------------------------------------------------------------------------
# UCRCD with solid state drives (SSD) ad competitors
#-------------------------------------------------------------------------------
# Now we fit an unbalanced competition and regime change diachronic model 
# (UCRCD) to study if and how the advent of a new competitor technology (SSDs) 
# contributed to the decay in HDD shipments worldwide.

# Import data about solid state drives (SSD) shipments
SSD.shipments <- read.csv("SSD_unit_shipments_worldwide.csv", header = T)
colnames(SSD.shipments) <- c("year", "SSDshipment")

# Convert data to a time series
SSD.ts <- ts(SSD.shipments$SSDshipment)

# Display data
tsdisplay(SSD.ts)

pdf("plots/SSD-ts.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(SSD.shipments$year, SSD.shipments$SSDshipment, type="l",
     xlab="Year", ylab="SSD unit shipments (10^6)")
par(op)
dev.off()

pdf("plots/SSD-ACF.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
Acf(SSD.ts, main="")
par(op)
dev.off()

# First we fit a Bass model just to get an initial estimate of the parameters
bass.mod.SSD <- BM(SSD.ts, prelimestimates = c(sum(HDD.ts)*1.1, 0.01, 0.1),
                   display = F)
summary(bass.mod.SSD)
m0.SSD <- bass.mod.SSD$coefficients[[1]]
p0.SSD <- bass.mod.SSD$coefficients[[2]]
q0.SSD <- bass.mod.SSD$coefficients[[3]]

# Fit the UCRCD model
UCRCD.mod <- UCRCD(HDD.ts, SSD.ts,
                   m1=m0, m2=m0.SSD, p1c=p0, q1c=q0, p2=p0.SSD, q2=q0.SSD,
                   display=F)
summary(UCRCD.mod)
q1c <- UCRCD.mod$Estimate["q1c",1]
q2 <- UCRCD.mod$Estimate["q2",1]
gamma <- UCRCD.mod$Estimate["gamma",1]
q1c
q2-gamma
# q1c and q2-gamma are both negative, confirming the hypothesis that the two
# technologies are indeed in full competition

# Cumulative process
pred.HDD.cumulative.ucrcd <- UCRCD.mod$fitted[[1]]
pred.SSD.cumulative.ucrcd <- UCRCD.mod$fitted[[2]]
pdf("plots/ucrcd-cumulative.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, cumsum(HDD.ts),
     xlab="Year", ylab="Cumulative unit shipments (10^6)", 
     pch=2, cex=1)
lines(SSD.shipments$year, cumsum(SSD.ts), type="p", 
     pch=1, cex=1)
lines(HDD.shipments$year, pred.HDD.cumulative.ucrcd, 
      lw=2, col="blue")
lines(SSD.shipments$year, pred.SSD.cumulative.ucrcd,
      lw=2, col="red")
legend("topleft", legend=c("HDD", "SSD", "predicted HDD", "predicted SSD"), 
       col=c("black", "black", "blue", "red"), pch=c(2,1,NA,NA),
       lty=c(NA,NA,"solid","solid"), lw=c(NA,NA,2,2))
par(op)
dev.off()

# Instantaneous process
pred.HDD.instantaneous.ucrcd <- UCRCD.mod$fitted.i[[1]]
pred.SSD.instantaneous.ucrcd <- UCRCD.mod$fitted.i[[2]]
pdf("plots/ucrcd-instant.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, HDD.ts,
     xlab="Year", ylab="Unit shipments (10^6)", 
     pch=2, cex=1)
lines(SSD.shipments$year, SSD.ts, type="p", 
      pch=1, cex=1)
lines(HDD.shipments$year, pred.HDD.instantaneous.ucrcd, 
      lw=2, col="blue")
lines(SSD.shipments$year, pred.SSD.instantaneous.ucrcd,
      lw=2, col="red")
legend("topleft", legend=c("HDD", "SSD", "predicted HDD", "predicted SSD"), 
       col=c("black", "black", "blue", "red"), pch=c(2,1,NA,NA),
       lty=c(NA,NA,"solid","solid"), lw=c(NA,NA,2,2))
par(op)
dev.off()

# Residuals analysis
residuals.HDD.ucrcd <- UCRCD.mod$residuals.i[[1]]
residuals.SSD.ucrcd <- UCRCD.mod$residuals.i[[2]]
pdf("plots/ucrcd-residuals.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, residuals.HDD.ucrcd, 
     xlab="Year", ylab="Residuals", col="blue", pch=2)
lines(SSD.shipments$year, residuals.SSD.ucrcd, type="p",
      col="red", pch=1)
legend("topleft", legend=c("HDD", "SSD"), 
       col=c("blue", "red"), pch=c(2,1))
par(op)
dev.off()
Acf(residuals.HDD.ucrcd)
Pacf(residuals.HDD.ucrcd)
Acf(residuals.SSD.ucrcd)
Pacf(residuals.SSD.ucrcd)


#-------------------------------------------------------------------------------
# ARIMA
#-------------------------------------------------------------------------------
# We fit an Arima model for both series to see if we can obtain solve the
# issue with heteroskedasticity in the residuals

HDD.arima <- auto.arima(HDD.ts, stationary=FALSE, seasonal=FALSE)
summary(HDD.arima)

SSD.arima <- auto.arima(SSD.ts, stationary=FALSE, seasonal=FALSE)
summary(SSD.arima)

# Cumulative process
pred.HDD.cumulative.arima <- cumsum(HDD.arima$fitted)
pred.SSD.cumulative.SSD.arima <- cumsum(SSD.arima$fitted)
pdf("plots/arima-cumulative.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, cumsum(HDD.ts),
     xlab = "Year", ylab = "Cumulative unit shipments (10^6)",
     pch=2, cex=1) 
lines(SSD.shipments$year, cumsum(SSD.ts), type="p",
       pch=1, cex=1) 
lines(HDD.shipments$year, pred.HDD.cumulative.arima,
      col = "blue", lwd = 2)  
lines(SSD.shipments$year, pred.SSD.cumulative.SSD.arima,
      col = "red",  lwd = 2)  
legend("topleft", legend=c("HDD", "SSD", "Arima(2,2,2)", "Arima(0,1,0)"), 
       col=c("black", "black", "blue", "red"), pch=c(2,1,NA,NA),
       lty=c(NA,NA,"solid","solid"), lw=c(NA,NA,2,2))
par(op)
dev.off()

# Instantaneous process
pred.HDD.instantaneous.arima <- HDD.arima$fitted
pred.SSD.instantaneous.arima <- SSD.arima$fitted
pdf("plots/arima-instant.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, HDD.ts,
     xlab = "Year", ylab = "Unit shipments (10^6)", ylim=c(-10, 700),
     pch=2, cex=1) 
lines(SSD.shipments$year, SSD.ts, type="p",
      pch=1, cex=1) 
lines(HDD.shipments$year, pred.HDD.instantaneous.arima,
      col = "blue", lwd = 2)  
lines(SSD.shipments$year, pred.SSD.instantaneous.arima,
      col = "red",  lwd = 2)  
legend("topleft", legend=c("HDD", "SSD", "Arima(2,2,2)", "Arima(0,1,0)"), 
       col=c("black", "black", "blue", "red"), pch=c(2,1,NA,NA),
       lty=c(NA,NA,"solid","solid"), lw=c(NA,NA,2,2))
par(op)
dev.off()

# Residuals analysis
residuals.HDD.arima <- HDD.arima$residuals
residuals.SSD.arima <- SSD.arima$residuals
pdf("plots/arima-residuals.pdf", width=6, height=4)
op <- par(
  mar = c(3.2, 3.2, 0.3, 0.3),   # bottom, left, top, right in *text lines*
  mgp = c(2, 0.45, 0),           # moves axis title/labels inward
  omi = c(0, 0, 0, 0)            # zero outer margins
)
plot(HDD.shipments$year, residuals.HDD.arima, 
     xlab="Year", ylab="Residuals", pch=2)
lines(SSD.shipments$year, residuals.SSD.arima, type="p",
      col=, pch=20)
legend("topleft", legend=c("HDD", "SSD"), 
       col=c("black", "black"), pch=c(2,20))
par(op)
dev.off()
Acf(residuals.HDD.arima)
Pacf(residuals.HDD.arima)
Acf(residuals.SSD.arima)
Pacf(residuals.SSD.arima)
# Heteroskedasticity is still present, but at least residuals do not show any 
# sign of autocorrelation

