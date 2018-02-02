---
# bibliography: 'bib/manual.bib'
output:
  html_document:
    keep_md: yes
    toc: yes
---



# PK09 - Modeling of fraction absorbed and nonlinear bioavailability across the liver

이 예제는 Pharmacokinetic and Pharmacodynamic Data Analysis 교과서의 예제입니다.  
소스 코드는 [깃헙](https://github.com/asancpt/edison-gab)에 올라와 있습니다.
에디슨 앱은 <https://www.edison.re.kr/simulation> 에서 확인할 수 있습니다.


```r
dPK09 = read.csv("data-raw/PK09.csv", skip=1, as.is=TRUE)
```

```
## Warning in file(file, "rt"): 파일 'data-raw/PK09.csv'를 여는데 실패했습니
## 다: No such file or directory
```

```
## Error in file(file, "rt"): 커넥션을 열 수 없습니다
```

```r
colnames(dPK09) = c("TIME", "DV", "CMT") ; dPK09
```

```
## Error in colnames(dPK09) = c("TIME", "DV", "CMT"): 객체 'dPK09'를 찾을 수 없습니다
```

```
## Error in eval(expr, envir, enclos): 객체 'dPK09'를 찾을 수 없습니다
```

```r
dPK09 = dPK09[dPK09[,"DV"] != "missing",] ; dPK09
```

```
## Error in eval(expr, envir, enclos): 객체 'dPK09'를 찾을 수 없습니다
```

```
## Error in eval(expr, envir, enclos): 객체 'dPK09'를 찾을 수 없습니다
```

```r
dPK09[,"DV"] = as.numeric(dPK09[,"DV"]) ; dPK09
```

```
## Error in eval(expr, envir, enclos): 객체 'dPK09'를 찾을 수 없습니다
```

```
## Error in eval(expr, envir, enclos): 객체 'dPK09'를 찾을 수 없습니다
```

```r
# Dosing
# infusion 2 / 0.0056hr
# oral 6

QH = 3.3
VH = 0.02

ivPKde = function(t, y, p)
{
  if (t < 0.0056) {
    RateIn = 357.1429 # 2/0.0056
  } else {
    RateIn = 0
  }
  CL = p["Vmax"]/(p["Km"] + y[4])
  dy1dt = -p["Ka"]*y[1]                           # absorption, gut comp
  dy2dt = (RateIn - QH*(y[2] - y[4]) - p["Cld"]*(y[2] - y[3]))/p["Vc"] # central, plasma
  dy3dt = (p["Cld"]*(y[2] - y[3]))/p["Vt"]                # peripheral, tissue
  dy4dt = (p["Ka"]*y[1] + QH*(y[2] - y[4]) - CL*y[4])/VH  # hepatic comp
  return(list(c(dy1dt, dy2dt, dy3dt, dy4dt)))
}

poPKde = function(t, y, p)
{
  CL = p["Vmax"]/(p["Km"] + y[4])
  if (t < p["Tlag"]) {
    Ka = 0
  } else {
    Ka = p["Ka"]
  }
  dy1dt = -Ka*y[1]                           # absorption, gut comp
  dy2dt = (-QH*(y[2] - y[4]) - p["Cld"]*(y[2] - y[3]))/p["Vc"] # central, plasma
  dy3dt = (p["Cld"]*(y[2] - y[3]))/p["Vt"]                # peripheral, tissue
  dy4dt = (Ka*y[1] + QH*(y[2] - y[4]) - CL*y[4])/VH  # hepatic comp
  return(list(c(dy1dt, dy2dt, dy3dt, dy4dt)))
}

Dpo = 6

Times1 = c(0, dPK09[dPK09[,"CMT"]==1,"TIME"]) # IV dosing
```

```
## Error in eval(expr, envir, enclos): 객체 'dPK09'를 찾을 수 없습니다
```

```r
iTime1 = 2:length(Times1)
```

```
## Error in eval(expr, envir, enclos): 객체 'Times1'를 찾을 수 없습니다
```

```r
lsoda(y=c(0, 0, 0, 0), times=Times1, func=ivPKde, parms=c(Vc=0.34, Cld=1.84, Vt=0.38, Vmax=0.13, Km=0.31, Ka=11.3, Tlag=0.062))
```

```
## Error in lsoda(y = c(0, 0, 0, 0), times = Times1, func = ivPKde, parms = c(Vc = 0.34, : 함수 "lsoda"를 찾을 수 없습니다
```

```r
Times2 = c(0, dPK09[dPK09[,"CMT"]==2,"TIME"]) # oral dosing
```

```
## Error in eval(expr, envir, enclos): 객체 'dPK09'를 찾을 수 없습니다
```

```r
iTime2 = 2:length(Times2)
```

```
## Error in eval(expr, envir, enclos): 객체 'Times2'를 찾을 수 없습니다
```

```r
lsoda(y=c(0.38*Dpo, 0, 0, 0), times=Times2, func=poPKde, parms=c(Vc=0.34, Cld=1.84, Vt=0.38, Vmax=0.13, Km=0.31, Ka=11.3, Tlag=0.062))
```

```
## Error in lsoda(y = c(0.38 * Dpo, 0, 0, 0), times = Times2, func = poPKde, : 함수 "lsoda"를 찾을 수 없습니다
```

```r
fPK09 = function(THETA)
{
  Ka   = THETA[1]
  Vc   = THETA[2]
  Vt   = THETA[3]
  Km   = THETA[4]
  Vmax = THETA[5]
  Cld  = THETA[6]
  Tlag = THETA[7]
  Fa   = THETA[8]

  Fs1 = lsoda(y=c(0, 0, 0, 0), times=Times1, func=ivPKde, parms=c(Ka=Ka, Vc=Vc, Vt=Vt, Km=Km, Vmax=Vmax, Cld=Cld, Tlag=Tlag))
  Fs2 = lsoda(y=c(Fa*Dpo,0, 0, 0), times=Times2, func=poPKde, parms=c(Ka=Ka, Vc=Vc, Vt=Vt, Km=Km, Vmax=Vmax, Cld=Cld, Tlag=Tlag))
  return(c(Fs1[iTime1,3], Fs2[iTime2,3]))
}

nlr(fPK09, dPK09, pNames=c("Ka", "Vc", "Vt", "Km", "Vmax", "Cld", "Tlag", "Fa"),
     IE=c(10, 0.4, 0.6, 0.3, 0.1, 2, 0.08, 0.5),
     LB=c(0, 0.001, 0.001, 0, 0, 0, 0, 0),
     UB=c(30, 1, 2, 1, 1, 4, 1, 1), Error="P")
```

```
## Error in nlr(fPK09, dPK09, pNames = c("Ka", "Vc", "Vt", "Km", "Vmax", : 객체 'dPK09'를 찾을 수 없습니다
```

```r
nlr(fPK09, dPK09, pNames=c("Ka", "Vc", "Vt", "Km", "Vmax", "Cld", "Tlag", "Fa"),
     IE=c(10, 0.4, 0.6, 0.3, 0.1, 2, 0.08, 0.5),
     LB=c(0, 0.001, 0.001, 0, 0, 0, 0, 0),
     UB=c(30, 1, 2, 1, 1, 4, 1, 1))
```

```
## Error in nlr(fPK09, dPK09, pNames = c("Ka", "Vc", "Vt", "Km", "Vmax", : 객체 'dPK09'를 찾을 수 없습니다
```

```r
# nlr(fPK09, dPK09, pNames=c("Ka", "Vc", "Vt", "Km", "Vmax", "Cld", "Tlag", "Fa"), IE=c(10, 0.4, 0.6, 0.3, 0.1, 2, 0.08, 0.5), Error="C")
# e$PE

wnl5(fPK09, dPK09, pNames=c("Ka", "Vc", "Vt", "Km", "Vmax", "Cld", "Tlag", "Fa"), IE=c(10, 0.4, 0.6, 0.3, 0.1, 2, 0.08, 0.5), Error="P")
```

```
## Error in wnl5(fPK09, dPK09, pNames = c("Ka", "Vc", "Vt", "Km", "Vmax", : 객체 'dPK09'를 찾을 수 없습니다
```

