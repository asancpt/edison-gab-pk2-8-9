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
에디슨 앱은 <https://www.edison.re.kr/simulation> 에서 확인하세요.


```r
# source('index.R')

PK02 = read.csv("data-raw/PK02.csv", skip=1)
colnames(PK02) = c("TIME","DV")
DV2 = c(4.7, 4.48, 4.22, 3.87, 3.57, 2.97, 2.25, 1.74, 1.02, 0.77, 0.61, 0.36, 0.2)
PK02 = cbind(PK02, DV2)
PK02
```

```
##    TIME   DV  DV2
## 1    10 0.00 4.70
## 2    15 0.28 4.48
## 3    20 0.55 4.22
## 4    30 1.20 3.87
## 5    40 2.00 3.57
## 6    60 1.95 2.97
## 7    90 1.85 2.25
## 8   120 1.60 1.74
## 9   180 0.86 1.02
## 10  210 0.78 0.77
## 11  240 0.60 0.61
## 12  300 0.21 0.36
## 13  360 0.18 0.20
```

```r
dPK02 = PK02

## Plot

jpeg(file = 'result/figure1.png', type='cairo')
plot(DV2 ~ TIME, data=PK02, type="o")
lines(DV ~ TIME, data=PK02, type="o", col="red")
dev.off()
```

```
## pdf 
##   2
```

```r
jpeg(file = 'result/figure2.png', type = 'cairo')
plot(log(DV2) ~ TIME, data=PK02, type="o")
lines(log(DV) ~ TIME, data=PK02, type="o", col="red")
dev.off()
```

```
## pdf 
##   2
```

```r
## NCA
R1 = sNCA(PK02$TIME, PK02$DV2, dose=100, adm="Bolus", doseUnit="ug", timeUnit="min") ; R1
```

```
##            b0          CMAX         CMAXD          TMAX          TLAG 
##  1.626233e+00  4.700000e+00  4.700000e-02  1.000000e+01            NA 
##          CLST         CLSTP          TLST        LAMZHL          LAMZ 
##  2.000000e-01  2.049213e-01  3.600000e+02  7.770316e+01  8.920450e-03 
##        LAMZLL        LAMZUL       LAMZNPT        CORRXY            R2 
##  1.000000e+01  3.600000e+02  1.300000e+01 -9.999083e-01  9.998167e-01 
##         R2ADJ        AUCLST        AUCALL        AUCIFO       AUCIFOD 
##  9.998000e-01  5.515147e+02  5.515147e+02  5.739351e+02  5.739351e+00 
##        AUCIFP       AUCIFPD        AUCPEO        AUCPEP       AUMCLST 
##  5.744868e+02  5.744868e+00  3.906434e+00  3.998713e+00  5.289150e+04 
##       AUMCIFO       AUMCIFP       AUMCPEO       AUMCPEP            C0 
##  6.347621e+04  6.373666e+04  1.667508e+01  1.701558e+01  5.172941e+00 
##       AUCPBEO       AUCPBEP           VZO           VZP           CLO 
##  8.601096e+00  8.592836e+00  1.953217e+01  1.951341e+01  1.742357e-01 
##           CLP      MRTIVLST      MRTIVIFO      MRTIVIFP          VSSO 
##  1.740684e-01  9.590225e+01  1.105982e+02  1.109454e+02  1.927016e+01 
##          VSSP 
##  1.931209e+01 
## attr(,"units")
##  [1] ""            "ug/L"        "ug/L/ug"     "min"         "min"        
##  [6] "ug/L"        "ug/L"        "min"         "min"         "/min"       
## [11] "min"         "min"         ""            ""            ""           
## [16] ""            "min*ug/L"    "min*ug/L"    "min*ug/L"    "min*ug/L/ug"
## [21] "min*ug/L"    "min*ug/L/ug" "%"           "%"           "min2*ug/L"  
## [26] "min2*ug/L"   "min2*ug/L"   "%"           "%"           "ug/L"       
## [31] "%"           "%"           "L"           "L"           "L/min"      
## [36] "L/min"       "min"         "min"         "min"         "L"          
## [41] "L"
```

```r
R2 = sNCA(PK02$TIME, PK02$DV, dose=100, adm="Extravascular", doseUnit="ug", timeUnit="min") ; R2
```

```
##            b0          CMAX         CMAXD          TMAX          TLAG 
##  1.557534e+00  2.000000e+00  2.000000e-02  4.000000e+01  1.000000e+01 
##          CLST         CLSTP          TLST        LAMZHL          LAMZ 
##  1.800000e-01  1.647689e-01  3.600000e+02  7.424929e+01  9.335405e-03 
##        LAMZLL        LAMZUL       LAMZNPT        CORRXY            R2 
##  9.000000e+01  3.600000e+02  7.000000e+00 -9.822141e-01  9.647446e-01 
##         R2ADJ        AUCLST        AUCALL        AUCIFO       AUCIFOD 
##  9.576935e-01  3.308750e+02  3.308750e+02  3.501564e+02  3.501564e+00 
##        AUCIFP       AUCIFPD        AUCPEO        AUCPEP       AUMCLST 
##  3.485249e+02  3.485249e+00  5.506520e+00  5.064170e+00  4.230750e+04 
##       AUMCIFO       AUMCIFP       AUMCPEO       AUMCPEP          VZFO 
##  5.131423e+04  5.055210e+04  1.755210e+01  1.630912e+01  3.059178e+01 
##          VZFP          CLFO          CLFP      MRTEVLST      MRTEVIFO 
##  3.073499e+01  2.855866e-01  2.869236e-01  1.278655e+02  1.465466e+02 
##      MRTEVIFP 
##  1.450459e+02 
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
##   AUCIFO 
## 61.00976
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
## $Est
##               Ka           Ka         V   AddErrVar    AddErrSD
## PE   0.013202011  0.013202169 12.822846  0.28467603  0.53355040
## SE   0.009843412  0.009843494  9.516335  0.07895478  0.07398999
## RSE 74.559948530 74.559673174 74.213911 27.73495916 13.86747958
## 
## $Cov
##                      Ka            Ka             V     AddErrVar
## Ka         9.689277e-05 -8.887105e-05 -9.070128e-02 -1.733549e-09
## Ka        -8.887105e-05  9.689437e-05  8.972688e-02  1.730679e-09
## V         -9.070128e-02  8.972688e-02  9.056064e+01  2.096069e-06
## AddErrVar -1.733549e-09  1.730679e-09  2.096069e-06  6.233857e-03
## 
## $run
## $run$m
## [1] 9
## 
## $run$n
## [1] 17
## 
## $run$run
## [1] 5
## 
## $run$p.value
## [1] 0.0005408779
## 
## 
## $AIC
## [1] 25.22596
## 
## $AICc
## [1] 30.22596
```

```r
wnl5(fPK01, dPK02, pNames=c("Ka", "Ka", "V"), IE=c(0.05, 0.1, 30))
```

```
## $PE
##          Ka          Ka           V 
##  0.01320192  0.01320229 12.82293529 
## 
## $WRSS
## [1] 7.40159
## 
## $run
## $run$m
## [1] 9
## 
## $run$n
## [1] 17
## 
## $run$run
## [1] 5
## 
## $run$p.value
## [1] 0.0005408779
## 
## 
## $AIC
## [1] 32.02203
## 
## $SBC
## [1] 33.71688
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
nlr(fPK02, dPK02, pNames=c("Ka", "Ka", "V", "tlag"), IE=c(0.05, 0.1, 30, 20))
```

```
## $Est
##               Ka          Ka        V     tlag   AddErrVar    AddErrSD
## PE   0.010511207  0.02747011 27.35399 11.37546  0.24891659  0.49891541
## SE   0.007765467  0.02428842 16.62814  3.47740  0.06903709  0.06918717
## RSE 73.877969930 88.41760204 60.78873 30.56931 27.73503065 13.86751533
## 
## $Cov
##                      Ka            Ka             V          tlag
## Ka         6.030247e-05 -1.720152e-04 -1.247885e-01 -5.199936e-03
## Ka        -1.720152e-04  5.899272e-04  3.823044e-01  2.944083e-02
## V         -1.247885e-01  3.823044e-01  2.764952e+02  1.260623e+01
## tlag      -5.199936e-03  2.944083e-02  1.260623e+01  1.209231e+01
## AddErrVar -3.834051e-09  1.139948e-08  7.673617e-06  2.655465e-07
##               AddErrVar
## Ka        -3.834051e-09
## Ka         1.139948e-08
## V          7.673617e-06
## tlag       2.655465e-07
## AddErrVar  4.766120e-03
## 
## $run
## $run$m
## [1] 11
## 
## $run$n
## [1] 15
## 
## $run$run
## [1] 12
## 
## $run$p.value
## [1] 0.3136559
## 
## 
## $AIC
## [1] 23.73581
## 
## $AICc
## [1] 32.30724
```

```r
wnl5(fPK02, dPK02, pNames=c("Ka", "Ka", "V", "tlag"), IE=c(0.05, 0.1, 30, 20))
```

```
## $PE
##          Ka          Ka           V        tlag 
##  0.01051129  0.02746992 27.35381481 11.37543534 
## 
## $WRSS
## [1] 6.471827
## 
## $run
## $run$m
## [1] 11
## 
## $run$n
## [1] 15
## 
## $run$run
## [1] 12
## 
## $run$p.value
## [1] 0.3136559
## 
## 
## $AIC
## [1] 32.27696
## 
## $SBC
## [1] 34.53676
```

## Figures

![](result/figure1.png)

![](result/figure2.png)

