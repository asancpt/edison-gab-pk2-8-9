---
# bibliography: 'bib/manual.bib'
output:
  html_document:
    keep_md: yes
    toc: yes
---



# PK08 - Two-compartment distribution models

이 예제는 Pharmacokinetic and Pharmacodynamic Data Analysis 교과서의 예제입니다.  
소스 코드는 [깃헙](https://github.com/asancpt/edison-gab)에 올라와 있습니다.
에디슨 앱은 <https://www.edison.re.kr/simulation> 에서 확인할 수 있습니다.


```r
# setwd("D:/Gab (2)")

dPK08 = read.csv("data-raw/PK08.csv", skip=1)
colnames(dPK08) = c("TIME", "DV") ; dPK08
```

```
##     TIME    DV
## 1   0.08 1.810
## 2   0.25 1.400
## 3   0.50 1.170
## 4   0.75 1.010
## 5   1.00 0.970
## 6   1.33 0.958
## 7   1.67 0.800
## 8   2.00 0.819
## 9   2.50 0.790
## 10  3.07 0.725
## 11  3.50 0.647
## 12  4.03 0.663
## 13  5.00 0.591
## 14  7.00 0.524
## 15 11.00 0.406
## 16 23.00 0.237
## 17 29.00 0.154
## 18 35.00 0.116
## 19 47.25 0.065
```

```r
Div = 100

png('figure1.png', type = 'cairo')
plot(DV ~ TIME, data=dPK08, type="o")
dev.off()
```

```
## pdf 
##   2
```

```r
png('figure2.png', type = 'cairo')
plot(log(DV) ~ TIME, data=dPK08, type="o")
dev.off()
```

```
## pdf 
##   2
```

```r
##
fPK08a = function(THETA) # biexponential
{
  A     = THETA[1]
  alpha = THETA[2]
  B     = THETA[3]
  beta  = THETA[4]
  TIME  = e$DATA[,"TIME"]
  Cp = A*exp(-alpha*TIME) + B*exp(-beta*TIME) # eq 8:1
  return(Cp)
}

nlr(fPK08a, dPK08, pNames=c("A", "alpha", "B", "beta"), IE=c(2, 2, 1, 0.1), Error="POIS")
```

```
## $Est
##              A     alpha          B        beta   PoisErrVar    PoisErrSD
## PE  1.03915428  1.936571 0.84425551 0.057857835 2.297337e-03  0.047930541
## SE  0.07655964  0.306198 0.02586652 0.002521422 7.465295e-04  0.007787618
## RSE 7.36749549 15.811347 3.06382616 4.357961640 3.249543e+01 16.247715600
## 
## $Cov
##                        A        alpha             B         beta
## A           5.861379e-03 1.402474e-02  2.172136e-04 6.572893e-06
## alpha       1.402474e-02 9.375722e-02  5.624845e-03 3.299323e-04
## B           2.172136e-04 5.624845e-03  6.690769e-04 4.400452e-05
## beta        6.572893e-06 3.299323e-04  4.400452e-05 6.357570e-06
## PoisErrVar -3.568058e-08 1.053162e-06 -9.873449e-08 6.465128e-08
##               PoisErrVar
## A          -3.568058e-08
## alpha       1.053162e-06
## B          -9.873449e-08
## beta        6.465128e-08
## PoisErrVar  5.573063e-07
## 
## $run
## $run$m
## [1] 9
## 
## $run$n
## [1] 10
## 
## $run$run
## [1] 9
## 
## $run$p.value
## [1] 0.3185932
## 
## 
## $AIC
## [1] -62.72047
## 
## $AICc
## [1] -58.10509
```

```r
wnl5(fPK08a, dPK08, pNames=c("A", "alpha", "B", "beta"), IE=c(2, 2, 1, 0.1), Error="POIS")
```

```
## $PE
##          A      alpha          B       beta 
## 1.03928193 1.93216827 0.84465877 0.05759117 
## 
## $WRSS
## [1] 0.04358074
## 
## $run
## $run$m
## [1] 9
## 
## $run$n
## [1] 10
## 
## $run$run
## [1] 9
## 
## $run$p.value
## [1] 0.3185932
## 
## 
## $AIC
## [1] -51.52966
## 
## $SBC
## [1] -47.7519
```

```r
##
fPK08b = function(THETA) # Takada
{
  Vc   = THETA[1]
  beta = THETA[2]
  Vmax = THETA[3]
  Kd   = THETA[4]
  TIME = e$DATA[,"TIME"]
  Vt   = Vmax*TIME/(Kd + TIME)         # eq 8:3
  Cp   = Div/(Vc + Vt)*exp(-beta*TIME) # eq 8:2
  return(Cp)
}  

nlr(fPK08b, dPK08, pNames=c("Vc", "beta", "Vmax", "Kd"), IE=c(100, 0.1, 140, 1.0), Error="POIS")
```

```
## $Est
##            Vc        beta      Vmax         Kd   PoisErrVar   PoisErrSD
## PE  46.875432 0.052493264 91.345708  0.7678991 8.892274e-04  0.02981992
## SE   2.034537 0.001797439  3.726205  0.1229457 2.886836e-04  0.00484045
## RSE  4.340305 3.424132613  4.079233 16.0106595 3.246454e+01 16.23227090
## 
## $Cov
##                       Vc          beta          Vmax            Kd
## Vc          4.139339e+00 -1.255699e-03  7.999224e-01  1.965453e-01
## beta       -1.255699e-03  3.230787e-06 -4.736060e-03 -1.332410e-04
## Vmax        7.999224e-01 -4.736060e-03  1.388460e+01  2.879978e-01
## Kd          1.965453e-01 -1.332410e-04  2.879978e-01  1.511565e-02
## PoisErrVar -1.068954e-07  1.006454e-08  7.677645e-07 -1.019526e-07
##               PoisErrVar
## Vc         -1.068954e-07
## beta        1.006454e-08
## Vmax        7.677645e-07
## Kd         -1.019526e-07
## PoisErrVar  8.333823e-08
## 
## $run
## $run$m
## [1] 9
## 
## $run$n
## [1] 10
## 
## $run$run
## [1] 11
## 
## $run$p.value
## [1] 0.4904523
## 
## 
## $AIC
## [1] -80.66002
## 
## $AICc
## [1] -76.04463
```

```r
wnl5(fPK08b, dPK08, pNames=c("Vc", "beta", "Vmax", "Kd"), IE=c(100, 0.1, 140, 1.0), Error="POIS")
```

```
## $PE
##          Vc        beta        Vmax          Kd 
## 46.87649328  0.05238586 91.33752968  0.76898408 
## 
## $WRSS
## [1] 0.01688478
## 
## $run
## $run$m
## [1] 9
## 
## $run$n
## [1] 10
## 
## $run$run
## [1] 11
## 
## $run$p.value
## [1] 0.4904523
## 
## 
## $AIC
## [1] -69.54551
## 
## $SBC
## [1] -65.76775
```

```r
##
fPK08c = function(THETA) # Colburn
{
  Vc   = THETA[1]
  beta = THETA[2]
  Vmax = THETA[3]                     # Erratum p 516 initial value : Vt -> Vmax
  Kv   = THETA[4]
  TIME = e$DATA[,"TIME"]
  Vt = Vmax*(1 - exp(-Kv*TIME))        # eq 8:5
  Cp = Div/(Vc + Vt)*exp(-beta*TIME)   # eq 8:4
  return(Cp)
}

nlr(fPK08c, dPK08, pNames=c("Vc", "beta", "Vt", "Kv"), IE=c(100, 0.1, 140, 1.0), Error="POIS")
```

```
## $Est
##            Vc        beta        Vt         Kv   PoisErrVar    PoisErrSD
## PE  50.462121 0.056329259 71.705644  1.1069764 1.547986e-03  0.039344455
## SE   2.169659 0.002184103  3.391626  0.1636311 5.027699e-04  0.006389336
## RSE  4.299580 3.877385912  4.729928 14.7818030 3.247897e+01 16.239482538
## 
## $Cov
##                       Vc          beta            Vt            Kv
## Vc          4.707421e+00 -1.156712e-03 -1.512253e+00 -2.456052e-01
## beta       -1.156712e-03  4.770305e-06 -4.489085e-03  1.886977e-04
## Vt         -1.512253e+00 -4.489085e-03  1.150312e+01 -2.417520e-01
## Kv         -2.456052e-01  1.886977e-04 -2.417520e-01  2.677513e-02
## PoisErrVar  9.858964e-07  2.993853e-08  3.843832e-06  3.168829e-07
##              PoisErrVar
## Vc         9.858964e-07
## beta       2.993853e-08
## Vt         3.843832e-06
## Kv         3.168829e-07
## PoisErrVar 2.527775e-07
## 
## $run
## $run$m
## [1] 9
## 
## $run$n
## [1] 10
## 
## $run$run
## [1] 11
## 
## $run$p.value
## [1] 0.4904523
## 
## 
## $AIC
## [1] -70.18341
## 
## $AICc
## [1] -65.56802
```

```r
wnl5(fPK08c, dPK08, pNames=c("Vc", "beta", "Vt", "Kv"), IE=c(100, 0.1, 140, 1.0), Error="POIS")
```

```
## $PE
##          Vc        beta          Vt          Kv 
## 50.45618040  0.05614555 71.68257771  1.10500133 
## 
## $WRSS
## [1] 0.02938035
## 
## $run
## $run$m
## [1] 9
## 
## $run$n
## [1] 10
## 
## $run$run
## [1] 11
## 
## $run$p.value
## [1] 0.4904523
## 
## 
## $AIC
## [1] -59.02116
## 
## $SBC
## [1] -55.2434
```

```r
##
fPK08d = function(THETA) # Reparametrized Cl model
{
  Cl    = THETA[1]
  alpha = THETA[2]
  B     = THETA[3]
  beta  = THETA[4]
  TIME = e$DATA[,"TIME"]
  Cp = alpha*(Div/Cl - B/beta)*exp(-alpha*TIME) + B*exp(-beta*TIME) # eq 8:8
  return(Cp)
}

nlr(fPK08d, dPK08, pNames=c("Cl", "alpha", "B", "beta"), IE=c(10, 2, 1, 0.1), Error="POIS") # fitting failure
```

```
## Warning in log(Ci): NaNs produced
```

```
## Error in optim(rep(0.1, e$nPara), ObjEst, method = Method): L-BFGS-B needs finite values of 'fn'
```

```r
nlr(fPK08d, dPK08, pNames=c("Cl", "alpha", "B", "beta"), IE=c(10, 2, 1, 0.1))
```

```
## $Est
##            Cl      alpha          B        beta    AddErrVar     AddErrSD
## PE  7.2559606  2.4682915 0.89574735 0.067116129 1.657470e-03  0.040712043
## SE  0.5183085  0.3475647 0.03150794 0.006506637 5.367252e-04  0.006591725
## RSE 7.1432096 14.0811842 3.51750312 9.694594813 3.238219e+01 16.191093449
## 
## $Cov
##                     Cl        alpha            B         beta    AddErrVar
## Cl        2.686437e-01 6.952167e-02 1.018085e-02 3.260898e-03 3.227226e-06
## alpha     6.952167e-02 1.208012e-01 7.990943e-03 1.176193e-03 9.205527e-07
## B         1.018085e-02 7.990943e-03 9.927504e-04 1.643184e-04 1.085713e-07
## beta      3.260898e-03 1.176193e-03 1.643184e-04 4.233632e-05 3.813465e-08
## AddErrVar 3.227226e-06 9.205527e-07 1.085713e-07 3.813465e-08 2.880739e-07
## 
## $run
## $run$m
## [1] 9
## 
## $run$n
## [1] 10
## 
## $run$run
## [1] 7
## 
## $run$p.value
## [1] 0.0767174
## 
## 
## $AIC
## [1] -57.68934
## 
## $AICc
## [1] -53.07395
```

```r
wnl5(fPK08d, dPK08, pNames=c("Cl", "alpha", "B", "beta"), IE=c(10, 2, 1, 0.1), Error="POIS")
```

```
## $PE
##         Cl      alpha          B       beta 
## 6.57794779 1.93129527 0.84465837 0.05760005 
## 
## $WRSS
## [1] 0.04358086
## 
## $run
## $run$m
## [1] 9
## 
## $run$n
## [1] 10
## 
## $run$run
## [1] 9
## 
## $run$p.value
## [1] 0.3185932
## 
## 
## $AIC
## [1] -51.52961
## 
## $SBC
## [1] -47.75185
```

```r
##
require(deSolve)
```

```
## Loading required package: deSolve
```

```r
PK2c = function(t, y, p)
{
  dy1dt = (-p["Cl"]*y[1] - p["Cld"]*y[1] + p["Cld"]*y[2])/p["Vc"]   # Eq 8:9
  dy2dt = (p["Cld"]*y[1] - p["Cld"]*y[2])/p["Vt"]                   # Eq 8:10
  return(list(c(dy1dt, dy2dt)))
}

Times = c(0, dPK08[,"TIME"])
iTime = 2:length(Times)
lsoda(y=c(Div/50, 0), times=Times, func=PK2c, parms=c(Vc=50, Cl=7, Cld=50, Vt=60))
```

```
##     time          1         2
## 1   0.00 2.00000000 0.0000000
## 2   0.08 1.83058208 0.1233253
## 3   0.25 1.54439253 0.3284971
## 4   0.50 1.25731832 0.5271856
## 5   0.75 1.07480564 0.6454671
## 6   1.00 0.95718145 0.7139767
## 7   1.33 0.86094646 0.7593368
## 8   1.67 0.80135389 0.7761229
## 9   2.00 0.76395634 0.7772013
## 10  2.50 0.72610586 0.7653582
## 11  3.07 0.69513292 0.7439560
## 12  3.50 0.67537627 0.7260511
## 13  4.03 0.65301344 0.7036257
## 14  5.00 0.61507881 0.6635133
## 15  7.00 0.54435616 0.5873525
## 16 11.00 0.42648213 0.4601705
## 17 23.00 0.20509649 0.2212977
## 18 29.00 0.14222880 0.1534637
## 19 35.00 0.09863155 0.1064234
## 20 47.25 0.04671484 0.0504049
```

```r
fPK08e = function(THETA)
{
  Fs = lsoda(y=c(Div/THETA[1],0), times=Times, func=PK2c, parms=c(Vc=THETA[1], Cl=THETA[2], Cld=THETA[3], Vt=THETA[4]))
  return(Fs[iTime,2])
}

nlr(fPK08e, dPK08, pNames=c("Vc", "Cl", "Cld", "Vt"), IE=c(50, 7, 50, 60), Error="POIS")
```

```
## $Est
##            Vc        Cl       Cld        Vt   PoisErrVar    PoisErrSD
## PE  53.090802 6.6100046 51.514041 57.222674 2.297402e-03  0.047931217
## SE   2.350242 0.2047092  7.590782  2.817886 7.465887e-04  0.007788125
## RSE  4.426834 3.0969607 14.735365  4.924421 3.249709e+01 16.248545149
## 
## $Cov
##                       Vc            Cl           Cld            Vt
## Vc          5.523636e+00  2.958878e-03 -1.347338e+01 -3.218090e+00
## Cl          2.958878e-03  4.190588e-02  7.131453e-03 -9.587430e-02
## Cld        -1.347338e+01  7.131453e-03  5.761997e+01  4.072239e+00
## Vt         -3.218090e+00 -9.587430e-02  4.072239e+00  7.940479e+00
## PoisErrVar  2.857874e-06  7.995914e-06  3.398088e-05  4.685004e-06
##              PoisErrVar
## Vc         2.857874e-06
## Cl         7.995914e-06
## Cld        3.398088e-05
## Vt         4.685004e-06
## PoisErrVar 5.573946e-07
## 
## $run
## $run$m
## [1] 9
## 
## $run$n
## [1] 10
## 
## $run$run
## [1] 9
## 
## $run$p.value
## [1] 0.3185932
## 
## 
## $AIC
## [1] -62.72087
## 
## $AICc
## [1] -58.10549
```

```r
wnl5(fPK08e, dPK08, pNames=c("Vc", "Cl", "Cld", "Vt"), IE=c(50, 7, 50, 60), Error="POIS")
```

```
## $PE
##        Vc        Cl       Cld        Vt 
## 53.088317  6.577636 51.335966 57.196376 
## 
## $WRSS
## [1] 0.04357991
## 
## $run
## $run$m
## [1] 9
## 
## $run$n
## [1] 10
## 
## $run$run
## [1] 9
## 
## $run$p.value
## [1] 0.3185932
## 
## 
## $AIC
## [1] -51.53002
## 
## $SBC
## [1] -47.75226
```

```r
######## multiple dosing########
```

## Figures

![](figure1.png)

![](figure2.png)
