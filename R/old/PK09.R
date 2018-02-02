dPK09 = read.csv("data-raw/PK09.csv", skip=1, as.is=TRUE)
colnames(dPK09) = c("TIME", "DV", "CMT") ; dPK09
dPK09 = dPK09[dPK09[,"DV"] != "missing",] ; dPK09
dPK09[,"DV"] = as.numeric(dPK09[,"DV"]) ; dPK09

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
iTime1 = 2:length(Times1)
lsoda(y=c(0, 0, 0, 0), times=Times1, func=ivPKde, parms=c(Vc=0.34, Cld=1.84, Vt=0.38, Vmax=0.13, Km=0.31, Ka=11.3, Tlag=0.062))

Times2 = c(0, dPK09[dPK09[,"CMT"]==2,"TIME"]) # oral dosing
iTime2 = 2:length(Times2)
lsoda(y=c(0.38*Dpo, 0, 0, 0), times=Times2, func=poPKde, parms=c(Vc=0.34, Cld=1.84, Vt=0.38, Vmax=0.13, Km=0.31, Ka=11.3, Tlag=0.062))

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

nlr(fPK09, dPK09, pNames=c("Ka", "Vc", "Vt", "Km", "Vmax", "Cld", "Tlag", "Fa"),
     IE=c(10, 0.4, 0.6, 0.3, 0.1, 2, 0.08, 0.5),
     LB=c(0, 0.001, 0.001, 0, 0, 0, 0, 0),
     UB=c(30, 1, 2, 1, 1, 4, 1, 1))

# nlr(fPK09, dPK09, pNames=c("Ka", "Vc", "Vt", "Km", "Vmax", "Cld", "Tlag", "Fa"), IE=c(10, 0.4, 0.6, 0.3, 0.1, 2, 0.08, 0.5), Error="C")
# e$PE

wnl5(fPK09, dPK09, pNames=c("Ka", "Vc", "Vt", "Km", "Vmax", "Cld", "Tlag", "Fa"), IE=c(10, 0.4, 0.6, 0.3, 0.1, 2, 0.08, 0.5), Error="P")

