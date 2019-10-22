setwd("E:\\ADI BUANA\\project")
data1 <- read.csv("project3.csv")

attach(data1)
head(data1, 5)

ols <- lm(formula = Y ~ X1+X2+X3+X4+X5, data = data1)
summary(ols)

# Asumsi normalitas 

residual <- resid(ols)
shapiro <- shapiro.test(residual)
shapiro

## Multikol
library(car)
vif(ols)
# Asumsi heteroskedastisitas
library(lmtest)
heteros <- bptest(ols)
heteros

#autoorelasi
auto <- dwtest(ols)
auto




library(spgwr)
#library(GWmodel)
col.bw <- gwr.sel(Y ~ X1+X2+X3+X4+X5, 
                  coords = cbind(data1$LONG,data1$LAT))

col.bw <- gwr.sel(Y ~ X1+X2+X3+X4+X5,coords = cbind(data1$LONG, data1$LAT),
                  data = data1, adapt = T, gweight = gwr.Gauss)
#OLS#
RegGlob=lm(formula=Y~X1+X2+X3+X4+X5,data=data1)
summary(RegGlob)


# Mencari bandwidth optimal (adaptive bandwidth)

gwrk <- gwr(Y ~ X1+X2+X3+X4+X5, 
            coords = cbind(data1$LONG, data1$LAT),
            data = data1, adapt = col.bw,
            hatmatrix = T, gweight = gwr.Gauss)

#out2 
gwrk
names(gwrk)
names(gwrk$SDF)


# Menampilkan nilai koefisien beta 2
gwrk$SDF$"(Intercept)"
gwrk$SDF$X1
gwrk$SDF$X2
gwrk$SDF$X3
gwrk$SDF$X4
gwrk$SDF$X5


# Uji Kecocokan Model 2
BFC02.gwr.test(gwrk)

# Uji Pengaruh Geografis terhadap setiap prediktor 2
LMZ.F3GWR.test(gwrk)


# Melihat nilai bandwidth 2
gwrk$bandwidth


# Menampilkan Nilai koefisien dan nilai prediksi 2
gwrk$SDF[,3:8]
gwrk$SDF[,c(10,21)]






####
####


##GAUSSIAN FIXED##
###Mendapatkan Bandwidth
fixgauss=gwr.sel(Y ~X1+X2+X3+X4+X5, data = data1,adapt=FALSE,coords=cbind(data1$LONG, data1$LAT),
                 gweight=gwr.Gauss)
###Estimasi Parameter
gwr.fixgauss=gwr(Y~X1+X2+X3+X4+X5, data = data1,bandwidth=fixgauss,
                 coords=cbind(data1$LONG, data1$LAT),hatmatrix=TRUE,gweight=gwr.Gauss)
gwr.fixgauss
###Membaca Output
names(gwr.fixgauss)
names(gwr.fixgauss$SDF)
gwr.fixgauss$SDF$`(Intercept)`
gwr.fixgauss$SDF$X1
gwr.fixgauss$SDF$X2
gwr.fixgauss$SDF$X3
gwr.fixgauss$SDF$X4
gwr.fixgauss$SDF$X5

#Uji Kesesuaian Model
BFC02.gwr.test(gwr.fixgauss)
LMZ.F1GWR.test(gwr.fixgauss)

##GAUSSIAN Addaptive##
###Mendapatkan Bandwidth
adaptgauss=gwr.sel(Y~X1+X2+X3+X4+X5,data = data1,adapt=TRUE,
                   coords=cbind(data1$LONG, data1$LAT),gweight=gwr.Gauss)
###Estimasi Parameter
gwr.adaptgauss=gwr(Y~X1+X2+X3+X4+X5, data = data1,adapt=adaptgauss,
                   coords=cbind(data1$LONG, data1$LAT),hatmatrix=TRUE,gweight=gwr.Gauss)
gwr.adaptgauss

###Membaca Output
names(gwr.adaptgauss)
names(gwr.adaptgauss$SDF)
gwr.adaptgauss$SDF$`(Intercept)`
gwr.adaptgauss$SDF$X1
gwr.adaptgauss$SDF$X2
gwr.adaptgauss$SDF$X3
gwr.adaptgauss$SDF$X4
gwr.adaptgauss$SDF$X5

#BANDWIDTH
B <- gwr.adaptgauss$bandwidth
B


#Uji Kesesuaian Model
BFC02.gwr.test(gwr.adaptgauss)
LMZ.F1GWR.test(gwr.adaptgauss)

###Uji Parameter Model
#Menghitung T
t.beta0ab <- gwr.adaptgauss$SDF$`(Intercept)`/gwr.adaptgauss$SDF$`(Intercept)_se`
t.beta0ab
t.beta1ab <- gwr.adaptgauss$SDF$X1/gwr.adaptgauss$SDF$X1_se
t.beta1ab
t.beta2ab <- gwr.adaptgauss$SDF$X2/gwr.adaptgauss$SDF$X2_se
t.beta2ab
t.beta3ab <- gwr.adaptgauss$SDF$X3/gwr.adaptgauss$SDF$X3_se
t.beta3ab
t.beta4ab <- gwr.adaptgauss$SDF$X4/gwr.adaptgauss$SDF$X4_se
t.beta4ab
t.beta5ab <- gwr.adaptgauss$SDF$X5/gwr.adaptgauss$SDF$X5_se
t.beta5ab


#Pemodelan GWR Tiap Wilayah
result <- as.data.frame(gwr.adaptgauss$SDF)
coefresult <- data.frame(result$X.Intercept.,result$X1,result$X2,result$X3,
                         result$X4,result$X5)
head(coefresult,10)

#Uji Kebaikan Model
AIC(ols)
