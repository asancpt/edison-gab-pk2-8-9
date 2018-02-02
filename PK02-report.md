---
# bibliography: 'bib/manual.bib'
output:
  html_document:
    keep_md: yes
    toc: yes
---



# PK2- One-compartment oral dosing

이 예제는 Pharmacokinetic and Pharmacodynamic Data Analysis 교과서의 예제입니다.  
소스 코드는 [깃헙](https://github.com/asancpt/edison-gab)에 올라와 있습니다.
에디슨 앱은 <https://www.edison.re.kr/simulation> 에서 확인할 수 있습니다.


```r
# source('index.R')

PK02 = read.csv("data-raw/PK02.csv", skip=1)
colnames(PK02) = c("TIME","DV")
DV2 = c(4.7, 4.48, 4.22, 3.87, 3.57, 2.97, 2.25, 1.74, 1.02, 0.77, 0.61, 0.36, 0.2)
PK02 = cbind(PK02, DV2)
```

```
## Error in data.frame(..., check.names = FALSE): arguments imply differing number of rows: 17, 13
```

```r
PK02
```

```
##    TIME   DV
## 1    10 0.00
## 2    15 0.28
## 3    20 0.54
## 4    30 1.25
## 5    40 2.00
## 6    50 2.03
## 7    60 1.95
## 8    90 1.85
## 9   100 1.72
## 10  120 1.60
## 11  140 1.34
## 12  160 1.09
## 13  180 0.86
## 14  210 0.78
## 15  240 0.60
## 16  300 0.21
## 17  360 0.18
```

```r
dPK02 = PK02

## Plot

# jpeg(file = 'result/figure1.png', type='cairo')
# plot(DV2 ~ TIME, data=PK02, type="o")
# lines(DV ~ TIME, data=PK02, type="o", col="red")
# dev.off()
# 
# jpeg(file = 'result/figure2.png', type = 'cairo')
# plot(log(DV2) ~ TIME, data=PK02, type="o")
# lines(log(DV) ~ TIME, data=PK02, type="o", col="red")
# dev.off()

## NCA
R1 = sNCA(PK02$TIME, PK02$DV2, dose=100, adm="Bolus", doseUnit="ug", timeUnit="min") ; R1
```

```
## Error in sNCA(PK02$TIME, PK02$DV2, dose = 100, adm = "Bolus", doseUnit = "ug", : Check input types!
```

```
## Error in eval(expr, envir, enclos): object 'R1' not found
```

```r
R2 = sNCA(PK02$TIME, PK02$DV, dose=100, adm="Extravascular", doseUnit="ug", timeUnit="min") ; R2
```

```
##            b0          CMAX         CMAXD          TMAX          TLAG 
##  1.543456e+00  2.030000e+00  2.030000e-02  5.000000e+01  1.000000e+01 
##          CLST         CLSTP          TLST        LAMZHL          LAMZ 
##  1.800000e-01  1.663124e-01  3.600000e+02  7.476994e+01  9.270399e-03 
##        LAMZLL        LAMZUL       LAMZNPT        CORRXY            R2 
##  9.000000e+01  3.600000e+02  1.000000e+01 -9.852075e-01  9.706337e-01 
##         R2ADJ        AUCLST        AUCALL        AUCIFO       AUCIFOD 
##  9.669630e-01  3.305500e+02  3.305500e+02  3.499666e+02  3.499666e+00 
##        AUCIFP       AUCIFPD        AUCPEO        AUCPEP       AUMCLST 
##  3.484902e+02  3.484902e+00  5.548140e+00  5.147966e+00  4.261000e+04 
##       AUMCIFO       AUMCIFP       AUMCPEO       AUMCPEP          VZFO 
##  5.169447e+04  5.100366e+04  1.757338e+01  1.645698e+01  3.082300e+01 
##          VZFP          CLFO          CLFP      MRTEVLST      MRTEVIFO 
##  3.095359e+01  2.857415e-01  2.869522e-01  1.289064e+02  1.477126e+02 
##      MRTEVIFP 
##  1.463561e+02 
## attr(,"units")
##  [1] ""            "ug/L"        "ug/L/ug"     "min"         "min"        
##  [6] "ug/L"        "ug/L"        "min"         "min"         "/min"       
## [11] "min"         "min"         ""            ""            ""           
## [16] ""            "min*ug/L"    "min*ug/L"    "min*ug/L"    "min*ug/L/ug"
## [21] "min*ug/L"    "min*ug/L/ug" "%"           "%"           "min2*ug/L"  
## [26] "min2*ug/L"   "min2*ug/L"   "%"           "%"           "L"          
## [31] "L"           "L/min"       "L/min"       "min"         "min"        
## [36] "min"
```

```r
BA = R2["AUCIFO"]/R1["AUCIFO"]; BA * 100 # Absolute Bioavailability (BA)
```

```
## Error in eval(expr, envir, enclos): object 'R1' not found
```

```
## Error in eval(expr, envir, enclos): object 'BA' not found
```

```r
## Model without tlag
Dose = 100

fPK01 = function(THETA) # Prediction function
{
  DOSE = Dose
  TIME = e$DATA[,"TIME"]

  BA   = BA
  K    = THETA[1]
  Ka   = THETA[2]
  V    = THETA[3]

  F  = BA*DOSE/V*Ka/(Ka - K) * (exp(-K*TIME) - exp(-Ka*TIME))
  H1 = 1

  return(cbind(F, H1))
}

nlr(fPK01, dPK02, pNames=c("Ka", "Ka", "V"), IE=c(0.05, 0.1, 30))
```

```
## Error in e$Fx(vPara[1:e$nTheta]): object 'BA' not found
```

```r
wnl5(fPK01, dPK02, pNames=c("Ka", "Ka", "V"), IE=c(0.05, 0.1, 30))
```

```
## Error in e$Fx(vPara): object 'BA' not found
```

```r
## Model with tlag
fPK02 = function(THETA) # Prediction function
{
  DOSE = Dose
  TIME = e$DATA[,"TIME"]

  BA   = BA
  K    = THETA[1]
  Ka   = THETA[2]
  V    = THETA[3]
  tlag = THETA[4]

  F  = BA*DOSE/V*Ka/(Ka - K) * (exp(-K*(TIME - tlag)) - exp(-Ka*(TIME - tlag)))
  H1 = 1

  return(cbind(F, H1))
}
BA = 1
Dose = 100
colnames(PK02) = c("TIME", "DV", "DV2")
```

```
## Error in `colnames<-`(`*tmp*`, value = c("TIME", "DV", "DV2")): 'names' attribute [3] must be the same length as the vector [2]
```

```r
nlr(fPK02, dPK02, pNames=c("Ka", "Ka", "V", "tlag"), IE=c(0.05, 0.1, 30, 20))
```

```
## $Est
##               Ka          Ka        V      tlag   AddErrVar    AddErrSD
## PE   0.010110127  0.03021029 28.45123 11.495191  0.24179045  0.49172192
## SE   0.006120683  0.02122374 13.06396  3.189944  0.05864321  0.05963046
## RSE 60.540118876 70.25332892 45.91703 27.750249 24.25373334 12.12686667
## 
## $Cov
##                      Ka            Ka             V          tlag
## Ka         3.746276e-05 -1.174219e-04 -7.746652e-02 -3.191263e-03
## Ka        -1.174219e-04  4.504470e-04  2.590532e-01  2.241856e-02
## V         -7.746652e-02  2.590532e-01  1.706671e+02  8.061794e+00
## tlag      -3.191263e-03  2.241856e-02  8.061794e+00  1.017574e+01
## AddErrVar -1.856686e-09  2.585447e-09  4.069133e-06 -4.016032e-06
##               AddErrVar
## Ka        -1.856686e-09
## Ka         2.585447e-09
## V          4.069133e-06
## tlag      -4.016032e-06
## AddErrVar  3.439026e-03
## 
## $run
## $run$m
## [1] 17
## 
## $run$n
## [1] 17
## 
## $run$run
## [1] 12
## 
## $run$p.value
## [1] 0.02721811
## 
## 
## $AIC
## [1] 26.97442
## 
## $AICc
## [1] 32.42897
```

```r
wnl5(fPK02, dPK02, pNames=c("Ka", "Ka", "V", "tlag"), IE=c(0.05, 0.1, 30, 20))
```

```
## $PE
##          Ka          Ka           V        tlag 
##  0.01011006  0.03021076 28.45136106 11.49550270 
## 
## $WRSS
## [1] 8.220817
## 
## $run
## $run$m
## [1] 17
## 
## $run$n
## [1] 17
## 
## $run$run
## [1] 12
## 
## $run$p.value
## [1] 0.02721811
## 
## 
## $AIC
## [1] 43.81338
## 
## $SBC
## [1] 47.14624
```

## Figures

![](figure1.png)

![](figure2.png)
