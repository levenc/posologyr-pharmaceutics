mod_run001 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(0.2,
        0, 0.2,
        0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run002 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(0.2,
        0, 0.2,
        0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run003 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_ALAG=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_ALAG ~
      c(0.2,
        0, 0.2,
        0, 0, 0.2,
        0, 0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run004 <- list(
  ppk_model = RxODE::RxODE({
    depot(0) = 0;
    centr(0) = 0;

    TVCl = THETA_Cl;
    TVVc = THETA_Vc;
    TVD2 = THETA_D2;

    Cl = TVCl*exp(ETA_Cl);
    Vc = TVVc*exp(ETA_Vc);
    D2 = TVD2*exp(ETA_D2);

    K20 = Cl/Vc;
    Cc = centr/Vc;

    d/dt(depot) = 0;
    d/dt(centr) = -K20*centr;
    d/dt(AUC) = Cc;

    dur(centr) = D2;
  }),
  error_model = function(f,sigma) {
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_D2=4.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_D2 ~
      c(0.2,
        0, 0.2,
        0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run005 <- list(
  ppk_model = RxODE::RxODE({
    depot(0) = 0;
    centr(0) = 0;

    TVCl = THETA_Cl;
    TVVc = THETA_Vc;
    TVKa = THETA_Ka;
    TVD1 = THETA_D1;

    Cl = TVCl*exp(ETA_Cl);
    Vc = TVVc*exp(ETA_Vc);
    Ka = TVKa*exp(ETA_Ka);
    D1 = TVD1*exp(ETA_D1);

    K20 = Cl/Vc;
    Cc = centr/Vc;

    d/dt(depot) = -Ka * depot;
    d/dt(centr) = Ka * depot - K20*centr;
    d/dt(AUC) = Cc;

    dur(depot) = D1;
  }),
  error_model = function(f,sigma) {
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_D1=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_D1 ~
      c(0.2,
        0, 0.2,
        0, 0, 0.2,
        0, 0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run006 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_D2=4.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_D2 ~
      c(0.2,
        0, 0.2,
        0, 0, 0.2,
        0, 0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run007 <- list(
  ppk_model = RxODE::RxODE({
    depot(0) = 0;
    centr(0) = 0;
    periph(0) = 0;

    TVCl = THETA_Cl;
    TVVc = THETA_Vc;
    TVKa1 = THETA_Ka1;
    TVKa3 = THETA_Ka3;

    Cl = TVCl*exp(ETA_Cl);
    Vc = TVVc*exp(ETA_Vc);
    Ka1 = TVKa1*exp(ETA_Ka1);
    Ka3 = TVKa3*exp(ETA_Ka3);

    K20 = Cl/Vc;
    Cc = centr/Vc;

    d/dt(depot) = -Ka1 * depot;
    d/dt(centr) = Ka1 * depot + Ka3 * periph - K20*centr;
    d/dt(periph) = -Ka3 * periph;
    d/dt(AUC) = Cc;

    F(depot) = 0.2;
    F(periph) = 0.8;
  }),
  error_model = function(f,sigma) {
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka1=1.0, THETA_Ka3=0.25),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka1 + ETA_Ka3 ~
      c(0.2,
        0, 0.2,
        0, 0, 0.2,
        0, 0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run008 <- list(
  ppk_model = RxODE::RxODE({
    depot(0) = 0;
    centr(0) = 0;
    periph(0) = 0;

    TVCl = THETA_Cl;
    TVVc = THETA_Vc;
    TVKa = THETA_Ka;
    TVF1 = THETA_F1;

    Cl = TVCl*exp(ETA_Cl);
    Vc = TVVc*exp(ETA_Vc);
    Ka = TVKa*exp(ETA_Ka);
    LF1 = log(TVF1/(1-TVF1));
    F1 = exp(LF1+ETA_F1) / (1+exp(LF1+ETA_F1));

    K20 = Cl/Vc;
    Cc = centr/Vc;

    d/dt(depot) = -Ka * depot;
    d/dt(centr) = Ka * depot - K20*centr;
    d/dt(AUC) = Cc;

    F(depot) = F1;
  }),
  error_model = function(f,sigma) {
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_F1=0.5),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_F1 ~
      c(0.2,
        0, 0.2,
        0, 0, 0.2,
        0, 0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run101 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Vp=50.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_Vp ~
      c(0.2,
        0, 0.2,
        0, 0, 0.2,
        0, 0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run102 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Vp=50.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_Vp ~
      c(0.2,
        0, 0.2,
        0, 0, 0.2,
        0, 0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run201 <- list(
   ppk_model   = RxODE::RxODE({
      depot(0) = 0;
      centr(0) = 0;

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
     dv <- cbind(f,1)
     g  <- diag(dv%*%sigma%*%t(dv))
     return(sqrt(g))
   },
   theta = c(THETA_Vmax=10000, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Km=2500),
   omega = lotri::lotri({ETA_Vmax + ETA_Vc + ETA_Ka + ETA_Km ~
       c(0.2   ,
         0     ,     0.2,
         0     ,       0,   0.2,
         0     ,       0,     0,   0.2)}),
   sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)}))

mod_run202 <- list(
   ppk_model   = RxODE::RxODE({
      depot(0) = 0;
      centr(0) = 0;

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
     dv <- cbind(f,1)
     g  <- diag(dv%*%sigma%*%t(dv))
     return(sqrt(g))
   },
   theta = c(THETA_Vmax=10000, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Km=2500),
   omega = lotri::lotri({ETA_Vmax + ETA_Vc + ETA_Ka + ETA_Km ~
       c(0.2   ,
         0     ,     0.2,
         0     ,       0,     0,
         0     ,       0,     0,   0.2)}),
   sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)}))

 mod_run203 <- list(
   ppk_model   = RxODE::RxODE({
     centr(0) = 0;
     depot(0) = 0;

     TVCl  = THETA_Cl;
     TVVc  = THETA_Vc;
     TVKa  = THETA_Ka;
     TVKm  = THETA_Km;

     Cl    = TVCl*exp(ETA_Cl);
     Vc    = TVVc*exp(ETA_Vc);
     Ka    = TVKa*exp(ETA_Ka);
     Km    = TVKm*exp(ETA_Km);

     Cc    = centr/Vc;
     Vmax  =10000;
     Ke    = Cl/Vc;

     d/dt(depot)  = - Ka*depot;
     d/dt(centr)  =   Ka*depot - Vmax*(centr/Vc)/(Km+(centr/Vc)) - Ke*centr;
     d/dt(AUC)    =   Cc;
   }),
   error_model = function(f,sigma){
     dv <- cbind(f,1)
     g  <- diag(dv%*%sigma%*%t(dv))
     return(sqrt(g))
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Km=2500),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_Km ~
       c(0.2   ,
         0     ,     0.2,
         0     ,       0,   0.2,
         0     ,       0,     0,   0.2)}),
   sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)}))

mod_run204 <- list(
   ppk_model   = RxODE::RxODE({
     centr(0) = 0;
     depot(0) = 0;

     TVCl  = THETA_Cl;
     TVVc  = THETA_Vc;
     TVKa  = THETA_Ka;
     TVKm  = THETA_Km;

     Cl    = TVCl*exp(ETA_Cl);
     Vc    = TVVc*exp(ETA_Vc);
     Ka    = TVKa*exp(ETA_Ka);
     Km    = TVKm*exp(ETA_Km);

     Cc    = centr/Vc;
     Vmax  =10000;
     Ke    = Cl/Vc;

     d/dt(depot)  = - Ka*depot;
     d/dt(centr)  =   Ka*depot - Vmax*(centr/Vc)/(Km+(centr/Vc)) - Ke*centr;
     d/dt(AUC)    =   Cc;
   }),
   error_model = function(f,sigma){
     dv <- cbind(f,1)
     g  <- diag(dv%*%sigma%*%t(dv))
     return(sqrt(g))
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Km=2500),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_Km ~
       c(0.2   ,
         0     ,     0.2,
         0     ,       0,   0.2,
         0     ,       0,     0,   0.2)}),
   sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)}))

mod_run205 <- list(
   ppk_model   = RxODE::RxODE({
     centr(0) = 0;
     depot(0) = 0;

     TVCl  = THETA_Cl;
     TVVc  = THETA_Vc;
     TVKa  = THETA_Ka;
     TVVmax= THETA_Vmax;

     Cl    = TVCl*exp(ETA_Cl);
     Vc    = TVVc*exp(ETA_Vc);
     Ka    = TVKa*exp(ETA_Ka);
     Vmax  = TVVmax*exp(ETA_Vmax);

     Cc    = centr/Vc;
     Km    = 2500;
     Ke    = Cl/Vc;

     d/dt(depot)  = - Ka*depot;
     d/dt(centr)  =   Ka*depot - Vmax*(centr/Vc)/(Km+(centr/Vc)) - Ke*centr;
     d/dt(AUC)    =   Cc;
   }),
   error_model = function(f,sigma){
     dv <- cbind(f,1)
     g  <- diag(dv%*%sigma%*%t(dv))
     return(sqrt(g))
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Vmax=10000),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_Vmax ~
       c(0.2   ,
         0     ,     0.2,
         0     ,       0,   0.2,
         0     ,       0,     0,   0.2)}),
   sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)}))

mod_run206 <- list(
   ppk_model   = RxODE::RxODE({
     centr(0) = 0;
     depot(0) = 0;

     TVCl  = THETA_Cl;
     TVVc  = THETA_Vc;
     TVKa  = THETA_Ka;
     TVVmax= THETA_Vmax;

     Cl    = TVCl*exp(ETA_Cl);
     Vc    = TVVc*exp(ETA_Vc);
     Ka    = TVKa*exp(ETA_Ka);
     Vmax  = TVVmax*exp(ETA_Vmax);

     Cc    = centr/Vc;
     Km    = 2500;
     Ke    = Cl/Vc;

     d/dt(depot)  = - Ka*depot;
     d/dt(centr)  =   Ka*depot - Vmax*(centr/Vc)/(Km+(centr/Vc)) - Ke*centr;
     d/dt(AUC)    =   Cc;
   }),
   error_model = function(f,sigma){
     dv <- cbind(f,1)
     g  <- diag(dv%*%sigma%*%t(dv))
     return(sqrt(g))
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Vmax=10000),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_Vmax ~
       c(0.2   ,
         0     ,     0.2,
         0     ,       0,   0.2,
         0     ,       0,     0,   0.2)}),
   sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)}))

mod_run207 <- list(
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
     dv <- cbind(f,1)
     g  <- diag(dv%*%sigma%*%t(dv))
     return(sqrt(g))
   },
   theta = c(THETA_Vmax=10000, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Km=2500, THETA_Cl=4.0),
   omega = lotri::lotri({ETA_Vmax + ETA_Vc + ETA_Ka + ETA_Km + ETA_Cl ~
       c(0.2   ,
         0     ,     0.2,
         0     ,       0,   0.2,
         0     ,       0,     0,   0.2,
         0     ,       0,     0,     0,    0.2)}),
   sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)}))

mod_run208 <- list(
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
     dv <- cbind(f,1)
     g  <- diag(dv%*%sigma%*%t(dv))
     return(sqrt(g))
   },
   theta = c(THETA_Vmax=10000, THETA_Vc=70.0, THETA_Ka=1.0, THETA_Km=2500, THETA_Cl=4.0),
   omega = lotri::lotri({ETA_Vmax + ETA_Vc + ETA_Ka + ETA_Km + ETA_Cl ~
       c(0.2   ,
         0     ,     0.2,
         0     ,       0,   0.2,
         0     ,       0,     0,   0.2,
         0     ,       0,     0,     0,    0.2)}),
   sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)}))

mod_run301 <- list(
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
     dv <- cbind(f,1)
     g  <- diag(dv%*%sigma%*%t(dv))
     return(sqrt(g))
   },
   covariates = c("BW","SEX"),
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
       c(0.2   ,
         0     ,     0.2,
         0     ,       0,   0.2)}),
   sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)}))

mod_run302 <- list(
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
     dv <- cbind(f,1)
     g  <- diag(dv%*%sigma%*%t(dv))
     return(sqrt(g))
   },
   covariates = c("BW","SEX"),
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
       c(0.2   ,
         0     ,     0.2,
         0     ,       0,   0.2)}),
   sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)}))

mod_run401 <- list(
  ppk_model   = RxODE::RxODE({

    TVCl     = THETA_Cl;
    TVVc     = THETA_Vc;
    TVKa     = THETA_Ka;
    TVClmet  = THETA_Clmet;
    TVVcmet  = THETA_Vcmet;

    Cl       = TVCl*exp(ETA_Cl);
    Vc       = TVVc*exp(ETA_Vc);
    Ka       = TVKa*exp(ETA_Ka);
    Clmet    = TVClmet*exp(ETA_Clmet);
    Vcmet    = TVVcmet*exp(ETA_Vcmet);

    K23      = Cl/Vc;
    K30      = Clmet/Vcmet;

    Ccentr   = centr/Vc;
    Cmet     = centrmet/Vcmet;

    if(CMT==2){
      Cc     = Ccentr;
    } else if (CMT==3){
      Cc     = Cmet;
    }

    d/dt(depot)     = - Ka*depot;
    d/dt(centr)     =   Ka*depot - K23*centr;
    d/dt(centrmet)  =         .7 * K23*centr - K30*centrmet;


    d/dt(AUC)    =   Cc;
  }),
  error_model = function(f,sigma){
    dv <- cbind(f,1)
    g  <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0,
            THETA_Clmet=2.5, THETA_Vcmet=60.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_Clmet + ETA_Vcmet ~
      c(0.2   ,
        0     ,     0.2,
        0     ,       0,     0.2,
        0     ,       0,       0,     0.2,
        0     ,       0,       0,       0,     0.2)}),
  sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)}))

mod_run402 <- list(
  ppk_model   = RxODE::RxODE({

    TVCl     = THETA_Cl;
    TVVc     = THETA_Vc;
    TVKa     = THETA_Ka;
    TVClmet  = THETA_Clmet;
    TVVcmet  = THETA_Vcmet;

    Cl       = TVCl*exp(ETA_Cl);
    Vc       = TVVc*exp(ETA_Vc);
    Ka       = TVKa*exp(ETA_Ka);
    Clmet    = TVClmet*exp(ETA_Clmet);
    Vcmet    = TVVcmet*exp(ETA_Vcmet);

    K23      = Cl/Vc;
    K30      = Clmet/Vcmet;

    Ccentr   = centr/Vc;
    Cmet     = centrmet/Vcmet;

    if(CMT==2){
      Cc     = Ccentr;
    } else if (CMT==3){
      Cc     = Cmet;
    }

    d/dt(depot)     = - Ka*depot;
    d/dt(centr)     =   Ka*depot - K23*centr;
    d/dt(centrmet)  =         .7 * K23*centr - K30*centrmet;


    d/dt(AUC)    =   Cc;
  }),
  error_model = function(f,sigma){
    dv <- cbind(f,1)
    g  <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0,
            THETA_Clmet=2.5, THETA_Vcmet=60.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka + ETA_Clmet + ETA_Vcmet ~
      c(0.2   ,
        0     ,     0.2,
        0     ,       0,     0.2,
        0     ,       0,       0,     0.2,
        0     ,       0,       0,       0,     0.2)}),
  sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)}))

mod_run403 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(0.2,
        0, 0.2,
        0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.00,0.0,5000)})
)

mod_run404 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(0.2,
        0, 0.2,
        0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.00,0.0,5000)})
)

mod_run405 <- list(
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
     dv <- cbind(f,1)
     g  <- diag(dv%*%sigma%*%t(dv))
     return(sqrt(g))
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
       c(0.2   ,
         0     ,     0.2,
         0     ,       0,   0.2)}),
   sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,5000)}))

mod_run406 <- list(
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
     dv <- cbind(f,1)
     g  <- diag(dv%*%sigma%*%t(dv))
     return(sqrt(g))
   },
   theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
   omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
       c(0.2   ,
         0     ,     0.2,
         0     ,       0,   0.2)}),
   sigma       = lotri::lotri({prop + add ~ c(0.05,0.0,5000)}))

mod_run407 <- list(
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
    Cc = log(centr/Vc);

    d/dt(depot) = -Ka*depot;
    d/dt(centr) = Ka*depot - K20*centr;
    d/dt(AUC) = Cc;
  }),
  error_model = function(f,sigma) {
    dv <- cbind(f,1)
    g  <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(.2,
        0, .2,
        0, 0, .2)}),
  sigma = lotri::lotri({prop + add ~ c(0.00,0.00,0.05)})
)

mod_run408 <- list(
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
    Cc = log(centr/Vc);

    d/dt(depot) = -Ka*depot;
    d/dt(centr) = Ka*depot - K20*centr;
    d/dt(AUC) = Cc;
  }),
  error_model = function(f,sigma) {
    dv <- cbind(f,1)
    g  <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(.2,
        0, .2,
        0, 0, .2)}),
  sigma = lotri::lotri({prop + add ~ c(0.00,0.00,0.05)})
)

mod_run501 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(0.4,
        0, 0.4,
        0, 0, 0.4)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run502 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(0.6,
        0, 0.6,
        0, 0, 0.6)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run503 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(0.8,
        0, 0.8,
        0, 0, 0.8)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run504 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(1,
        0, 1,
        0, 0, 1)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run511 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(2,
        0, 0.2,
        0, 0, 0.2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run512 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(2,
        0, 0.2,
        0, 0, 2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)

mod_run513 <- list(
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
    dv <- cbind(f,1)
    g <- diag(dv%*%sigma%*%t(dv))
    return(sqrt(g))
  },
  theta = c(THETA_Cl=4.0, THETA_Vc=70.0, THETA_Ka=1.0),
  omega = lotri::lotri({ETA_Cl + ETA_Vc + ETA_Ka ~
      c(2,
        0, 2,
        0, 0, 2)}),
  sigma = lotri::lotri({prop + add ~ c(0.05,0.0,0.00)})
)
