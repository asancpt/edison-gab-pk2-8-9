dPK08 = read.csv("data-raw/PK08.csv", skip=1)
colnames(dPK08) = c("TIME", "DV") ; dPK08

Div = 100

png('figure1.png', type = 'cairo')
plot(DV ~ TIME, data=dPK08, type="o")
dev.off()

png('figure2.png', type = 'cairo')
plot(log(DV) ~ TIME, data=dPK08, type="o")
dev.off()

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
wnl5(fPK08a, dPK08, pNames=c("A", "alpha", "B", "beta"), IE=c(2, 2, 1, 0.1), Error="POIS")

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
wnl5(fPK08b, dPK08, pNames=c("Vc", "beta", "Vmax", "Kd"), IE=c(100, 0.1, 140, 1.0), Error="POIS")

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
wnl5(fPK08c, dPK08, pNames=c("Vc", "beta", "Vt", "Kv"), IE=c(100, 0.1, 140, 1.0), Error="POIS")

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
nlr(fPK08d, dPK08, pNames=c("Cl", "alpha", "B", "beta"), IE=c(10, 2, 1, 0.1))
wnl5(fPK08d, dPK08, pNames=c("Cl", "alpha", "B", "beta"), IE=c(10, 2, 1, 0.1), Error="POIS")

##
require(deSolve)
PK2c = function(t, y, p)
{
  dy1dt = (-p["Cl"]*y[1] - p["Cld"]*y[1] + p["Cld"]*y[2])/p["Vc"]   # Eq 8:9
  dy2dt = (p["Cld"]*y[1] - p["Cld"]*y[2])/p["Vt"]                   # Eq 8:10
  return(list(c(dy1dt, dy2dt)))
}

Times = c(0, dPK08[,"TIME"])
iTime = 2:length(Times)
lsoda(y=c(Div/50, 0), times=Times, func=PK2c, parms=c(Vc=50, Cl=7, Cld=50, Vt=60))

fPK08e = function(THETA)
{
  Fs = lsoda(y=c(Div/THETA[1],0), times=Times, func=PK2c, parms=c(Vc=THETA[1], Cl=THETA[2], Cld=THETA[3], Vt=THETA[4]))
  return(Fs[iTime,2])
}

nlr(fPK08e, dPK08, pNames=c("Vc", "Cl", "Cld", "Vt"), IE=c(50, 7, 50, 60), Error="POIS")
wnl5(fPK08e, dPK08, pNames=c("Vc", "Cl", "Cld", "Vt"), IE=c(50, 7, 50, 60), Error="POIS")


######## multiple dosing########
