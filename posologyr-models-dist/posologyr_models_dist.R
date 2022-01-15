mod_dist_run001 <- list(
   ppk_model = RxODE::RxODE({
      centr(0) = 0;
      depot(0) = 0;

      TVCl = THETA_Cl;
      TVVc = THETA_Vc;
      TVKa = THETA_Ka;

      Cl = TVCl*exp(ETA_Cl);
      Vc = TVVc*exp(ETA_Vc);
      Ka = TVKa*exp(ETA_Ka);

      K20 = Cl/Vc;
      Cc = centr/Vc;

      d/dt(depot) = -Ka*depot;
      d/dt(centr) = Ka*depot - K20*centr;
      d/dt(AUC) = Cc;
   }),
   error_model = function(f,sigma) {
      g <- sigma[2] + sigma[1]*f
      return(g)
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
         c(0.2,
           0, 0.2,
           0, 0, 0.2)}),
   sigma = c(b_prop=0.05,a_add=0.00)
)

mod_dist_run003 <- list(
   ppk_model = RxODE::RxODE({
      centr(0) = 0;
      depot(0) = 0;

      TVCl   = THETA_Cl;
      TVVc   = THETA_Vc;
      TVKa   = THETA_Ka;
      TVALAG = THETA_ALAG;

      Cl   = TVCl*exp(ETA_Cl);
      Vc   = TVVc*exp(ETA_Vc);
      Ka   = TVKa*exp(ETA_Ka);
      ALAG = TVALAG*exp(ETA_ALAG);

      K20 = Cl/Vc;
      Cc = centr/Vc;

      d/dt(depot) = -Ka*depot;
      d/dt(centr) = Ka*depot - K20*centr;
      d/dt(AUC)   = Cc;

      alag(depot) = ALAG;

   }),
   error_model = function(f,sigma) {
      g <- sigma[2] + sigma[1]*f
      return(g)
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_ALAG=1.0),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_ALAG ~
         c(0.2,
           0, 0.2,
           0, 0, 0.2,
           0, 0, 0, 0.2)}),
   sigma = c(b_prop=0.05,a_add=0.00)
)

mod_dist_run006 <- list(
   ppk_model = RxODE::RxODE({
      depot(0) = 0;
      centr(0) = 0;

      TVCl = THETA_Cl;
      TVVc = THETA_Vc;
      TVKa = THETA_Ka;
      TVD2 = THETA_D2;

      Cl = TVCl*exp(ETA_Cl);
      Vc = TVVc*exp(ETA_Vc);
      Ka = TVKa*exp(ETA_Ka);
      D2 = TVD2*exp(ETA_D2);

      K20 = Cl/Vc;
      Cc = centr/Vc;

      d/dt(depot) = -Ka * depot;
      d/dt(centr) = Ka * depot - K20*centr;
      d/dt(AUC) = Cc;

      dur(centr) = D2;
      F(depot) = 0.2;
      F(centr) = 0.8;
   }),
   error_model = function(f,sigma) {
      g <- sigma[2] + sigma[1]*f
      return(g)
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_D2=4.0),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_D2 ~
         c(0.2,
           0, 0.2,
           0, 0, 0.2,
           0, 0, 0, 0.2)}),
   sigma = c(b_prop=0.05,a_add=0.00)
)

mod_dist_run101 <- list(
   ppk_model = RxODE::RxODE({
      centr(0) = 0;
      depot(0) = 0;
      periph(0) = 0;

      TVCl = THETA_Cl;
      TVVc = THETA_Vc;
      TVKa = THETA_Ka;
      TVVp = THETA_Vp;

      Cl = TVCl*exp(ETA_Cl);
      Vc = TVVc*exp(ETA_Vc);
      Ka = TVKa*exp(ETA_Ka);
      Vp = TVVp*exp(ETA_Vp);
      Q = 4;

      K20 = Cl/Vc;
      K23 = Q / Vc;
      K32 = Q / Vp;
      Cc = centr/Vc;

      d/dt(depot) = -Ka*depot;
      d/dt(centr) = Ka*depot - K20*centr - K23*centr + K32*periph;
      d/dt(periph) = K23*centr - K32*periph;
      d/dt(AUC) = Cc;
   }),
   error_model = function(f,sigma) {
      g <- sigma[2] + sigma[1]*f
      return(g)
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Vp=50.0),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_Vp ~
         c(0.2,
           0, 0.2,
           0, 0, 0.2,
           0, 0, 0, 0.2)}),
   sigma = c(b_prop=0.05,a_add=0.00)
)


mod_dist_run201 <- list(
   ppk_model   = RxODE::RxODE({
      depot(0) = 0;
      centr(0) = 0;
      ETA_Ka   = 0;

      TVVmax  = THETA_Vmax;
      TVVc  = THETA_Vc;
      TVKa  = THETA_Ka;
      TVKm  = THETA_Km;

      Vmax  = TVVmax*exp(ETA_Vmax);
      Vc    = TVVc*exp(ETA_Vc);
      Ka    = TVKa*exp(ETA_Ka);
      Km    = TVKm*exp(ETA_Km);
      Cc    = centr/Vc;

      d/dt(depot)  = - Ka*depot;
      d/dt(centr)  =   Ka*depot - Vmax*(centr/Vc)/(Km+(centr/Vc));
      d/dt(AUC)    =   Cc;
   }),
   error_model = function(f,sigma){
      g <- sigma[2] + sigma[1]*f
      return(g)
   },
   theta = c(THETA_Vmax=10000, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Km=2500),
   omega = lotri::lotri({ETA_Vmax + ETA_Vc + ETA_Ka + ETA_Km ~
         c(0.2   ,
           0     ,     0.2,
           0     ,       0,     0,
           0     ,       0,     0,   0.2)}),
   sigma       = c(prop = 0.05, add = 0.00)
)

mod_dist_run207 <- list(
   ppk_model   = RxODE::RxODE({
      centr(0) = 0;
      depot(0) = 0;

      TVVmax= THETA_Vmax;
      TVVc  = THETA_Vc;
      TVKa  = THETA_Ka;
      TVKm  = THETA_Km;
      TVCl  = THETA_Cl;

      Vmax  = TVVmax*exp(ETA_Vmax);
      Vc    = TVVc*exp(ETA_Vc);
      Ka    = TVKa*exp(ETA_Ka);
      Km    = TVKm*exp(ETA_Km);
      Cl    = TVCl*exp(ETA_Cl);

      Cc    = centr/Vc;
      Ke    = Cl/Vc;

      d/dt(depot)  = - Ka*depot;
      d/dt(centr)  =   Ka*depot - Vmax*(centr/Vc)/(Km+(centr/Vc)) - Ke*centr;
      d/dt(AUC)    =   Cc;
   }),
   error_model = function(f,sigma){
      g <- sigma[2] + sigma[1]*f
      return(g)
   },
   theta = c(THETA_Vmax=10000, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Km=2500, THETA_Cl=4.0),
   omega = lotri::lotri({ETA_Vmax + ETA_Vc + ETA_Ka + ETA_Km + ETA_Cl ~
         c(0.2   ,
           0     ,     0.2,
           0     ,       0,   0.2,
           0     ,       0,     0,   0.2,
           0     ,       0,     0,     0,    0.2)}),
   sigma       = c(b_prop=0.05,a_add=0.00)
)

mod_dist_run301 <- list(
   ppk_model   = RxODE::RxODE({

      TVCl  = THETA_Cl*((BW/75)^1.5)*(0.75^SEX);
      TVVc  = THETA_Vc;
      TVKa  = THETA_Ka;

      Cl    = TVCl*exp(ETA_Cl);
      Vc    = TVVc*exp(ETA_Vc);
      Ka    = TVKa*exp(ETA_Ka);

      K20   = Cl/Vc;
      Cc    = centr/Vc;

      d/dt(depot)  = - Ka*depot;
      d/dt(centr)  =   Ka*depot - K20*centr;
      d/dt(AUC)    =   Cc;
   }),
   error_model = function(f,sigma){
      g <- sigma[2] + sigma[1]*f
      return(g)
   },
   covariates = c("BW","SEX"),
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
         c(0.2   ,
           0     ,     0.2,
           0     ,       0,   0.2)}),
   sigma       = c(b_prop=0.05,a_add=0.00)
)

mod_dist_run405 <- list(
   ppk_model   = RxODE::RxODE({
      centr(0) = 0;
      depot(0) = 0;

      TVCl  = THETA_Cl;
      TVVc  = THETA_Vc;
      TVKa  = THETA_Ka;

      Cl    = TVCl*exp(ETA_Cl);
      Vc    = TVVc*exp(ETA_Vc);
      Ka    = TVKa*exp(ETA_Ka);

      K20   = Cl/Vc;
      Cc    = centr/Vc;

      d/dt(depot)  = - Ka*depot;
      d/dt(centr)  =   Ka*depot - K20*centr;
      d/dt(AUC)    =   Cc;
   }),
   error_model = function(f,sigma){
      g <- sigma[2] + sigma[1]*f
      return(g)
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
         c(0.2   ,
           0     ,     0.2,
           0     ,       0,   0.2)}),
   sigma       = c(b_prop=0.05,a_add=5000)
)

mod_dist_run504 <- list(
   ppk_model = RxODE::RxODE({
      centr(0) = 0;
      depot(0) = 0;

      TVCl = THETA_Cl;
      TVVc = THETA_Vc;
      TVKa = THETA_Ka;

      Cl = TVCl*exp(ETA_Cl);
      Vc = TVVc*exp(ETA_Vc);
      Ka = TVKa*exp(ETA_Ka);

      K20 = Cl/Vc;
      Cc = centr/Vc;

      d/dt(depot) = -Ka*depot;
      d/dt(centr) = Ka*depot - K20*centr;
      d/dt(AUC) = Cc;
   }),
   error_model = function(f,sigma) {
      g <- sigma[2] + sigma[1]*f
      return(g)
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
         c(1,
           0, 1,
           0, 0, 1)}),
   sigma = c(b_prop=0.05,a_add=0.00)
)

mod_dist_run513 <- list(
   ppk_model = RxODE::RxODE({
      depot(0) = 0;
      centr(0) = 0;

      TVCl = THETA_Cl;
      TVVc = THETA_Vc;
      TVKa = THETA_Ka;

      Cl = TVCl*exp(ETA_Cl);
      Vc = TVVc*exp(ETA_Vc);
      Ka = TVKa*exp(ETA_Ka);

      K20 = Cl/Vc;
      Cc = centr/Vc;

      d/dt(depot) = -Ka*depot;
      d/dt(centr) = Ka*depot - K20*centr;
      d/dt(AUC) = Cc;
   }),
   error_model = function(f,sigma) {
      g <- sigma[2] + sigma[1]*f
      return(g)
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
         c(2,
           0, 2,
           0, 0, 2)}),
   sigma = c(b_prop=0.05,a_add=0.00)
)
