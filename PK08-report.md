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
dPK08 = read.csv("data-raw/PK08.csv", skip=1)
```

```
## Warning in file(file, "rt"): 파일 'data-raw/PK08.csv'를 여는데 실패했습니
## 다: No such file or directory
```

```
## Error in file(file, "rt"): 커넥션을 열 수 없습니다
```

```r
colnames(dPK08) = c("TIME", "DV") ; dPK08
```

```
## Error in colnames(dPK08) = c("TIME", "DV"): 객체 'dPK08'를 찾을 수 없습니다
```

```
## Error in eval(expr, envir, enclos): 객체 'dPK08'를 찾을 수 없습니다
```

```r
Div = 100

png('figure1.png', type = 'cairo')
```

```
## Error in png("figure1.png", type = "cairo"): X11을 사용할 수 없습니다
```

```r
plot(DV ~ TIME, data=dPK08, type="o")
```

```
## Error in eval(m$data, eframe): 객체 'dPK08'를 찾을 수 없습니다
```

```r
dev.off()
```

```
## null device 
##           1
```

```r
png('figure2.png', type = 'cairo')
```

```
## Error in png("figure2.png", type = "cairo"): X11을 사용할 수 없습니다
```

```r
plot(log(DV) ~ TIME, data=dPK08, type="o")
```

```
## Error in eval(m$data, eframe): 객체 'dPK08'를 찾을 수 없습니다
```

```r
dev.off()
```

```
## Error in dev.off(): cannot shut down device 1 (the null device)
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
## Error in nlr(fPK08a, dPK08, pNames = c("A", "alpha", "B", "beta"), IE = c(2, : 객체 'dPK08'를 찾을 수 없습니다
```

```r
wnl5(fPK08a, dPK08, pNames=c("A", "alpha", "B", "beta"), IE=c(2, 2, 1, 0.1), Error="POIS")
```

```
## Error in wnl5(fPK08a, dPK08, pNames = c("A", "alpha", "B", "beta"), IE = c(2, : 객체 'dPK08'를 찾을 수 없습니다
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
## Error in nlr(fPK08b, dPK08, pNames = c("Vc", "beta", "Vmax", "Kd"), IE = c(100, : 객체 'dPK08'를 찾을 수 없습니다
```

```r
wnl5(fPK08b, dPK08, pNames=c("Vc", "beta", "Vmax", "Kd"), IE=c(100, 0.1, 140, 1.0), Error="POIS")
```

```
## Error in wnl5(fPK08b, dPK08, pNames = c("Vc", "beta", "Vmax", "Kd"), IE = c(100, : 객체 'dPK08'를 찾을 수 없습니다
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
## Error in nlr(fPK08c, dPK08, pNames = c("Vc", "beta", "Vt", "Kv"), IE = c(100, : 객체 'dPK08'를 찾을 수 없습니다
```

```r
wnl5(fPK08c, dPK08, pNames=c("Vc", "beta", "Vt", "Kv"), IE=c(100, 0.1, 140, 1.0), Error="POIS")
```

```
## Error in wnl5(fPK08c, dPK08, pNames = c("Vc", "beta", "Vt", "Kv"), IE = c(100, : 객체 'dPK08'를 찾을 수 없습니다
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

# nlr(fPK08d, dPK08, pNames=c("Cl", "alpha", "B", "beta"), IE=c(10, 2, 1, 0.1), Error="POIS") # fitting failure
nlr(fPK08d, dPK08, pNames=c("Cl", "alpha", "B", "beta"), IE=c(10, 2, 1, 0.1))
```

```
## Error in nlr(fPK08d, dPK08, pNames = c("Cl", "alpha", "B", "beta"), IE = c(10, : 객체 'dPK08'를 찾을 수 없습니다
```

```r
wnl5(fPK08d, dPK08, pNames=c("Cl", "alpha", "B", "beta"), IE=c(10, 2, 1, 0.1), Error="POIS")
```

```
## Error in wnl5(fPK08d, dPK08, pNames = c("Cl", "alpha", "B", "beta"), IE = c(10, : 객체 'dPK08'를 찾을 수 없습니다
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
```

```
## Error in eval(expr, envir, enclos): 객체 'dPK08'를 찾을 수 없습니다
```

```r
iTime = 2:length(Times)
```

```
## Error in eval(expr, envir, enclos): 객체 'Times'를 찾을 수 없습니다
```

```r
lsoda(y=c(Div/50, 0), times=Times, func=PK2c, parms=c(Vc=50, Cl=7, Cld=50, Vt=60))
```

```
## Error in checkInput(y, times, func, rtol, atol, jacfunc, tcrit, hmin, : 객체 'Times'를 찾을 수 없습니다
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
## Error in nlr(fPK08e, dPK08, pNames = c("Vc", "Cl", "Cld", "Vt"), IE = c(50, : 객체 'dPK08'를 찾을 수 없습니다
```

```r
wnl5(fPK08e, dPK08, pNames=c("Vc", "Cl", "Cld", "Vt"), IE=c(50, 7, 50, 60), Error="POIS")
```

```
## Error in wnl5(fPK08e, dPK08, pNames = c("Vc", "Cl", "Cld", "Vt"), IE = c(50, : 객체 'dPK08'를 찾을 수 없습니다
```

```r
######## multiple dosing########
```

## Figures

![](figure1.png)

![](figure2.png)
