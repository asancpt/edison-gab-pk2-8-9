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
```

```
## Warning in file(file, "rt"): 파일 'data-raw/PK02.csv'를 여는데 실패했습니
## 다: No such file or directory
```

```
## Error in file(file, "rt"): 커넥션을 열 수 없습니다
```

```r
colnames(PK02) = c("TIME","DV")
```

```
## Error in colnames(PK02) = c("TIME", "DV"): 객체 'PK02'를 찾을 수 없습니다
```

```r
DV2 = c(4.7, 4.48, 4.22, 3.87, 3.57, 2.97, 2.25, 1.74, 1.02, 0.77, 0.61, 0.36, 0.2)
PK02 = cbind(PK02, DV2)
```

```
## Error in cbind(PK02, DV2): 객체 'PK02'를 찾을 수 없습니다
```

```r
PK02
```

```
## Error in eval(expr, envir, enclos): 객체 'PK02'를 찾을 수 없습니다
```

```r
dPK02 = PK02
```

```
## Error in eval(expr, envir, enclos): 객체 'PK02'를 찾을 수 없습니다
```

```r
## Plot

jpeg(file = 'result/figure1.png', type='cairo')
```

```
## Error in jpeg(file = "result/figure1.png", type = "cairo"): X11을 사용할 수 없습니다
```

```r
plot(DV2 ~ TIME, data=PK02, type="o")
```

```
## Error in eval(m$data, eframe): 객체 'PK02'를 찾을 수 없습니다
```

```r
lines(DV ~ TIME, data=PK02, type="o", col="red")
```

```
## Error in eval(m$data, eframe): 객체 'PK02'를 찾을 수 없습니다
```

```r
dev.off()
```

```
## null device 
##           1
```

```r
jpeg(file = 'result/figure2.png', type = 'cairo')
```

```
## Error in jpeg(file = "result/figure2.png", type = "cairo"): X11을 사용할 수 없습니다
```

```r
plot(log(DV2) ~ TIME, data=PK02, type="o")
```

```
## Error in eval(m$data, eframe): 객체 'PK02'를 찾을 수 없습니다
```

```r
lines(log(DV) ~ TIME, data=PK02, type="o", col="red")
```

```
## Error in eval(m$data, eframe): 객체 'PK02'를 찾을 수 없습니다
```

```r
dev.off()
```

```
## Error in dev.off(): cannot shut down device 1 (the null device)
```

```r
## NCA
R1 = sNCA(PK02$TIME, PK02$DV2, dose=100, adm="Bolus", doseUnit="ug", timeUnit="min") ; R1
```

```
## Error in sNCA(PK02$TIME, PK02$DV2, dose = 100, adm = "Bolus", doseUnit = "ug", : 객체 'PK02'를 찾을 수 없습니다
```

```
## Error in eval(expr, envir, enclos): 객체 'R1'를 찾을 수 없습니다
```

```r
R2 = sNCA(PK02$TIME, PK02$DV, dose=100, adm="Extravascular", doseUnit="ug", timeUnit="min") ; R2
```

```
## Error in sNCA(PK02$TIME, PK02$DV, dose = 100, adm = "Extravascular", doseUnit = "ug", : 객체 'PK02'를 찾을 수 없습니다
```

```
## Error in eval(expr, envir, enclos): 객체 'R2'를 찾을 수 없습니다
```

```r
BA = R2["AUCIFO"]/R1["AUCIFO"]; BA * 100 # Absolute Bioavailability (BA)
```

```
## Error in eval(expr, envir, enclos): 객체 'R2'를 찾을 수 없습니다
```

```
## Error in eval(expr, envir, enclos): 객체 'BA'를 찾을 수 없습니다
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
## Error in nlr(fPK01, dPK02, pNames = c("Ka", "Ka", "V"), IE = c(0.05, 0.1, : 객체 'dPK02'를 찾을 수 없습니다
```

```r
wnl5(fPK01, dPK02, pNames=c("Ka", "Ka", "V"), IE=c(0.05, 0.1, 30))
```

```
## Error in wnl5(fPK01, dPK02, pNames = c("Ka", "Ka", "V"), IE = c(0.05, : 객체 'dPK02'를 찾을 수 없습니다
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
## Error in colnames(PK02) = c("TIME", "DV", "DV2"): 객체 'PK02'를 찾을 수 없습니다
```

```r
nlr(fPK02, dPK02, pNames=c("Ka", "Ka", "V", "tlag"), IE=c(0.05, 0.1, 30, 20))
```

```
## Error in nlr(fPK02, dPK02, pNames = c("Ka", "Ka", "V", "tlag"), IE = c(0.05, : 객체 'dPK02'를 찾을 수 없습니다
```

```r
wnl5(fPK02, dPK02, pNames=c("Ka", "Ka", "V", "tlag"), IE=c(0.05, 0.1, 30, 20))
```

```
## Error in wnl5(fPK02, dPK02, pNames = c("Ka", "Ka", "V", "tlag"), IE = c(0.05, : 객체 'dPK02'를 찾을 수 없습니다
```

## Figures

![](figure1.png)

![](figure2.png)
