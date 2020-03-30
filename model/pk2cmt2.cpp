Model file:  pk2cmt.cpp 
  $PROB
# Model: `pk2cmt`
  - Two-compartment PK model
  - Dual first-order absorption
  - Optional nonlinear clearance from `CENT`
  - Source: `mrgsolve` internal library
  - Date: `r Sys.Date()`
  - Version: `r packageVersion("mrgsolve")`
  
  $PARAM @annotated
  TVCL   :  1  : Clearance (volume/time)
  TVVC   : 20  : Central volume (volume)
  Q    :  2  : Inter-compartmental clearance (volume/time)
  VP   : 10  : Peripheral volume of distribution (volume)
  KA1  :  1  : Absorption rate constant 1 (1/time)
  KA2  :  1  : Absorption rate constant 2 (1/time)
  VMAX :  0  : Maximum velocity (mass/time)
  KM   :  2  : Michaelis Constant (mass/volume)
  WT   :  70  : Body weight
  CL_WT   :  0.75  : Power exponent
  VC_WT   :  0.75  : Power exponent
  WTref   :  70  : Reference WT
  
  $CMT  @annotated
  EV1    : First extravascular compartment (mass)
  CENT   : Central compartment (mass)
  PERIPH : Peripheral compartment (mass) 
  EV2    : Second extravascular compartment (mass)
  AUC    : Dummy AUC compartment
  
$GLOBAL 
#define CP (CENT/VC)
#define CT (PERIPH/VP)
#define CLNL (VMAX/(KM+CP))
  
$MAIN
double CL = TVCL * pow(WT / WTref, CL_WT) * exp(ECL);
double VC = TVVC * pow(WT / WTref, VC_WT) * exp(EVC);

$OMEGA @annotated @block
  ECL: 0: ETA on clearance
  EVC: 0 0 : ETA on volume
  
  $ODE
  dxdt_EV1 = -KA1*EV1;
dxdt_EV2 = -KA2*EV2;
dxdt_CENT = KA1*EV1 + KA2*EV2 - (CL+CLNL+Q)*CP  + Q*CT;
dxdt_PERIPH = Q*CP - Q*CT;
dxdt_AUC = CP;

$CAPTURE  @annotated
  CP : Plasma concentration (mass/time)
  AUC : AUC