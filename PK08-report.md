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
## 1   0.05 1.920
## 2   0.10 1.710
## 3   0.25 1.400
## 4   0.40 1.280
## 5   0.50 1.170
## 6   0.70 0.990
## 7   1.00 0.970
## 8   1.30 0.962
## 9   1.60 0.820
## 10  2.00 0.819
## 11  2.50 0.780
## 12  3.00 0.729
## 13  3.50 0.643
## 14  4.00 0.665
## 15  5.00 0.591
## 16  6.00 0.567
## 17  7.00 0.524
## 18 10.00 0.426
## 19 15.00 0.332
## 20 20.00 0.267
## 21 25.00 0.179
## 22 35.00 0.116
## 23 45.00 0.069
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
##              A      alpha          B        beta   PoisErrVar    PoisErrSD
## PE  1.09702032  2.2241012 0.85517722 0.060580004 2.332974e-03  0.048300865
## SE  0.06213283  0.2868177 0.02330448 0.002526805 6.889081e-04  0.007131427
## RSE 5.66378097 12.8958944 2.72510509 4.171021296 2.952919e+01 14.764594032
## 
## $Cov
##                        A        alpha             B          beta
## A           3.860488e-03 9.865563e-03 -3.096518e-08 -4.755957e-06
## alpha       9.865563e-03 8.226442e-02  4.401645e-03  3.029052e-04
## B          -3.096518e-08 4.401645e-03  5.430987e-04  4.115985e-05
## beta       -4.755957e-06 3.029052e-04  4.115985e-05  6.384743e-06
## PoisErrVar -4.690200e-08 8.737138e-07 -9.439557e-08  5.318549e-08
##               PoisErrVar
## A          -4.690200e-08
## alpha       8.737138e-07
## B          -9.439557e-08
## beta        5.318549e-08
## PoisErrVar  4.745944e-07
## 
## $run
## $run$m
## [1] 10
## 
## $run$n
## [1] 13
## 
## $run$run
## [1] 7
## 
## $run$p.value
## [1] 0.01702786
## 
## 
## $AIC
## [1] -75.81262
## 
## $AICc
## [1] -72.28321
```

```r
wnl5(fPK08a, dPK08, pNames=c("A", "alpha", "B", "beta"), IE=c(2, 2, 1, 0.1), Error="POIS")
```

```
## $PE
##          A      alpha          B       beta 
## 1.09725824 2.21980682 0.85563811 0.06031816 
## 
## $WRSS
## [1] 0.0535843
## 
## $run
## $run$m
## [1] 10
## 
## $run$n
## [1] 13
## 
## $run$run
## [1] 7
## 
## $run$p.value
## [1] 0.01702786
## 
## 
## $AIC
## [1] -59.30948
## 
## $SBC
## [1] -54.76751
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
##            Vc        beta      Vmax          Kd   PoisErrVar    PoisErrSD
## PE  46.894300 0.053980512 90.659489  0.75237686 8.989454e-04  0.029982418
## SE   1.368629 0.001807649  3.421266  0.09452026 2.652349e-04  0.004423173
## RSE  2.918540 3.348707532  3.773754 12.56288744 2.950511e+01 14.752556819
## 
## $Cov
##                       Vc          beta          Vmax            Kd
## Vc          1.873145e+00 -7.323392e-04  6.130021e-01  9.506642e-02
## beta       -7.323392e-04  3.267597e-06 -4.634375e-03 -1.031728e-04
## Vmax        6.130021e-01 -4.634375e-03  1.170506e+01  2.219362e-01
## Kd          9.506642e-02 -1.031728e-04  2.219362e-01  8.934079e-03
## PoisErrVar  2.134815e-07  8.285782e-09  6.092591e-07 -7.050173e-08
##               PoisErrVar
## Vc          2.134815e-07
## beta        8.285782e-09
## Vmax        6.092591e-07
## Kd         -7.050173e-08
## PoisErrVar  7.034953e-08
## 
## $run
## $run$m
## [1] 11
## 
## $run$n
## [1] 12
## 
## $run$run
## [1] 13
## 
## $run$p.value
## [1] 0.4928244
## 
## 
## $AIC
## [1] -97.621
## 
## $AICc
## [1] -94.09159
```

```r
wnl5(fPK08b, dPK08, pNames=c("Vc", "beta", "Vmax", "Kd"), IE=c(100, 0.1, 140, 1.0), Error="POIS")
```

```
## $PE
##          Vc        beta        Vmax          Kd 
## 46.89122035  0.05387517 90.65061182  0.75324961 
## 
## $WRSS
## [1] 0.02066403
## 
## $run
## $run$m
## [1] 11
## 
## $run$n
## [1] 12
## 
## $run$run
## [1] 13
## 
## $run$p.value
## [1] 0.4928244
## 
## 
## $AIC
## [1] -81.2253
## 
## $SBC
## [1] -76.68332
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
## PE  49.338320 0.058548700 71.750188  1.2009764 1.555610e-03  0.039441223
## SE   1.551390 0.002195125  3.023266  0.1448428 4.591537e-04  0.005820733
## RSE  3.144392 3.749229202  4.213601 12.0604173 2.951599e+01 14.757993473
## 
## $Cov
##                       Vc          beta            Vt            Kv
## Vc          2.406811e+00 -7.377758e-04 -7.149222e-01 -1.476397e-01
## beta       -7.377758e-04  4.818574e-06 -4.413737e-03  1.702700e-04
## Vt         -7.149222e-01 -4.413737e-03  9.140140e+00 -2.171509e-01
## Kv         -1.476397e-01  1.702700e-04 -2.171509e-01  2.097943e-02
## PoisErrVar  1.317468e-06  2.421028e-08  3.338369e-06  2.391046e-07
##              PoisErrVar
## Vc         1.317468e-06
## beta       2.421028e-08
## Vt         3.338369e-06
## Kv         2.391046e-07
## PoisErrVar 2.108221e-07
## 
## $run
## $run$m
## [1] 11
## 
## $run$n
## [1] 12
## 
## $run$run
## [1] 13
## 
## $run$p.value
## [1] 0.4928244
## 
## 
## $AIC
## [1] -85.08346
## 
## $AICc
## [1] -81.55405
```

```r
wnl5(fPK08c, dPK08, pNames=c("Vc", "beta", "Vt", "Kv"), IE=c(100, 0.1, 140, 1.0), Error="POIS")
```

```
## $PE
##          Vc        beta          Vt          Kv 
## 49.32862304  0.05837012 71.72548263  1.19921635 
## 
## $WRSS
## [1] 0.03574559
## 
## $run
## $run$m
## [1] 11
## 
## $run$n
## [1] 12
## 
## $run$run
## [1] 13
## 
## $run$p.value
## [1] 0.4928244
## 
## 
## $AIC
## [1] -68.62055
## 
## $SBC
## [1] -64.07857
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

## Warning in log(Ci): NaNs produced

## Warning in log(Ci): NaNs produced

## Warning in log(Ci): NaNs produced

## Warning in log(Ci): NaNs produced

## Warning in log(Ci): NaNs produced

## Warning in log(Ci): NaNs produced

## Warning in log(Ci): NaNs produced

## Warning in log(Ci): NaNs produced

## Warning in log(Ci): NaNs produced

## Warning in log(Ci): NaNs produced
```

```
## Error in solve.default(e$InvCov): Lapack routine dgesv: system is exactly singular: U[2,2] = 0
```

```r
nlr(fPK08d, dPK08, pNames=c("Cl", "alpha", "B", "beta"), IE=c(10, 2, 1, 0.1))
```

```
## $Est
##            Cl      alpha          B       beta    AddErrVar     AddErrSD
## PE  7.5243566  2.6987010 0.90667934 0.07039224  0.001613627  0.040169977
## SE  0.4318987  0.2782432 0.02775376 0.00557502  0.000475975  0.005924511
## RSE 5.7400088 10.3102657 3.06103398 7.91993663 29.497210937 14.748605469
## 
## $Cov
##                     Cl        alpha            B         beta    AddErrVar
## Cl        1.865365e-01 4.373727e-02 7.116504e-03 2.308854e-03 4.955951e-06
## alpha     4.373727e-02 7.741930e-02 5.390919e-03 7.843814e-04 1.127440e-06
## B         7.116504e-03 5.390919e-03 7.702713e-04 1.233206e-04 1.884121e-07
## beta      2.308854e-03 7.843814e-04 1.233206e-04 3.108085e-05 6.131551e-08
## AddErrVar 4.955951e-06 1.127440e-06 1.884121e-07 6.131551e-08 2.265522e-07
## 
## $run
## $run$m
## [1] 11
## 
## $run$n
## [1] 12
## 
## $run$run
## [1] 9
## 
## $run$p.value
## [1] 0.09919028
## 
## 
## $AIC
## [1] -72.60225
## 
## $AICc
## [1] -69.07284
```

```r
wnl5(fPK08d, dPK08, pNames=c("Cl", "alpha", "B", "beta"), IE=c(10, 2, 1, 0.1), Error="POIS")
```

```
## $PE
##         Cl      alpha          B       beta 
## 6.81355034 2.21794544 0.85561685 0.06033118 
## 
## $WRSS
## [1] 0.05358461
## 
## $run
## $run$m
## [1] 10
## 
## $run$n
## [1] 13
## 
## $run$run
## [1] 7
## 
## $run$p.value
## [1] 0.01702786
## 
## 
## $AIC
## [1] -59.30935
## 
## $SBC
## [1] -54.76737
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
##     time          1          2
## 1   0.00 2.00000000 0.00000000
## 2   0.05 1.89116750 0.07934952
## 3   0.10 1.79203478 0.15122198
## 4   0.25 1.54439229 0.32849733
## 5   0.40 1.35669102 0.45960592
## 6   0.50 1.25731764 0.52718614
## 7   0.70 1.10511099 0.62656940
## 8   1.00 0.95718146 0.71397674
## 9   1.30 0.86776559 0.75667930
## 10  1.60 0.81142840 0.77431233
## 11  2.00 0.76395638 0.77720124
## 12  2.50 0.72610586 0.76535820
## 13  3.00 0.69856476 0.74678700
## 14  3.50 0.67537629 0.72605113
## 15  4.00 0.65423971 0.70489155
## 16  5.00 0.61507879 0.66351333
## 17  6.00 0.57861076 0.62429427
## 18  7.00 0.54435603 0.58735247
## 19 10.00 0.45331028 0.48911830
## 20 15.00 0.33413219 0.36052605
## 21 20.00 0.24628830 0.26574357
## 22 25.00 0.18153828 0.19587820
## 23 35.00 0.09863167 0.10642316
## 24 45.00 0.05358744 0.05782092
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
##            Vc       Cl       Cld        Vt   PoisErrVar    PoisErrSD
## PE  51.221301 6.844916 58.539774 58.052404  0.002334982  0.048321647
## SE   1.735368 0.199332  6.997845  2.533039  0.000690112  0.007140817
## RSE  3.387980 2.912117 11.954000  4.363366 29.555352314 14.777676157
## 
## $Cov
##                       Vc            Cl           Cld            Vt
## Vc          3.011501e+00 -1.172816e-02 -8.8933824813 -1.749482e+00
## Cl         -1.172816e-02  3.973323e-02  0.0812678400 -1.061720e-01
## Cld        -8.893382e+00  8.126784e-02 48.9698311626  2.186720e+00
## Vt         -1.749482e+00 -1.061720e-01  2.1867198860  6.416285e+00
## PoisErrVar  2.816126e-06  6.688403e-06  0.0000287765  4.640299e-06
##              PoisErrVar
## Vc         2.816126e-06
## Cl         6.688403e-06
## Cld        2.877650e-05
## Vt         4.640299e-06
## PoisErrVar 4.762546e-07
## 
## $run
## $run$m
## [1] 10
## 
## $run$n
## [1] 13
## 
## $run$run
## [1] 7
## 
## $run$p.value
## [1] 0.01702786
## 
## 
## $AIC
## [1] -75.81309
## 
## $AICc
## [1] -72.28368
```

```r
wnl5(fPK08e, dPK08, pNames=c("Vc", "Cl", "Cld", "Vt"), IE=c(50, 7, 50, 60), Error="POIS")
```

```
## $PE
##        Vc        Cl       Cld        Vt 
## 51.204445  6.812074 58.414166 58.032418 
## 
## $WRSS
## [1] 0.05358257
## 
## $run
## $run$m
## [1] 10
## 
## $run$n
## [1] 13
## 
## $run$run
## [1] 7
## 
## $run$p.value
## [1] 0.01702786
## 
## 
## $AIC
## [1] -59.31022
## 
## $SBC
## [1] -54.76825
```

```r
######## multiple dosing########
```

## Figures

![](figure1.png)

![](figure2.png)
