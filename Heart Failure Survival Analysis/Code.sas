/************Survival Analysis bout PTSD vs Heart Failure***********/

PROC IMPORT DATAFILE="/home/u58947615/Heart Failure Case study/PTSD_data2"
DBMS=xlsx OUT=dataset REPLACE;
RUN;

PROC FORMAT;
	VALUE fgender 0 = "Females"
				  1 = "Males";
	VALUE fbmicat 1 = "UnderOrnormal Weight"
				  2 = "Overweight"
				  3 = "Obese";
	VALUE fcombat 0 = "Nonactive serving"
				  1 = "Active serving";
RUN;

DATA PTSD_data;
	SET dataset;
	FORMAT sexM fgender. bmicat fbmicat. combat fcombat.;
RUN;
	              
/*********Data Preparation***********/

*1. Descriptive analysis;
PROC CORR DATA=PTSD_data PLOTS(MAXPOINTS=none)=MATRIX(HISTOGRAM);
	VAR survweeks sexm age bmicat combat ptsd;
RUN;

PROC FREQ DATA=PTSD_data;
	TABLES hfailure*ptsd;
RUN;

*2. Some data visualization;
PROC UNIVARIATE DATA=PTSD_data(WHERE=(hfailure=1));
	VAR survweeks;
	CLASS ptsd;
	HISTOGRAM survweeks / KERNEL;
RUN;

/*Survival plot*/
PROC LIFETEST DATA=PTSD_data(WHERE=(hfailure=1 AND ptsd=1)) PLOTS=SURVIVAL(ATRISK) NOTABLE;
	TIME survweeks*hfailure(0);
	TITLE "PTSD = 1";
RUN;

PROC LIFETEST DATA=PTSD_data(WHERE=(hfailure=1 AND ptsd=0)) PLOTS=SURVIVAL(ATRISK) NOTABLE;
	TIME survweeks*hfailure(0);
	TITLE "PTSD = 0";
RUN;

/*Hazard plots*/
PROC LIFETEST DATA=PTSD_data(WHERE=(hfailure=1 AND ptsd=1)) PLOTS=HAZARD(BW=100) NOTABLE;
	TIME survweeks*hfailure(0);
	TITLE "PTSD = 1";
RUN;

PROC LIFETEST DATA=PTSD_data(WHERE=(hfailure=1)) PLOTS=HAZARD(BW=100) NOTABLE;
	TIME survweeks*hfailure(0);
	STRATA ptsd;
RUN;

*3. Kaplan-Meier non-parametric method;
PROC LIFETEST DATA=PTSD_data PLOTS=SURVIVAL(ATRISK CB) NOTABLE;
	STRATA ptsd;
	TIME survweeks*hfailure(0);
RUN;

*4.Parametric method;

/*4.1 Exploring Functional Form of continuous Covariates*/
PROC PHREG DATA=PTSD_data;
	CLASS sexM bmicat combat ptsd / PARAM=REF REF=first;
	MODEL survweeks*hfailure(0) = ;
	OUTPUT OUT=residuals RESMART=martingale;
RUN;

PROC LOESS DATA=residuals PLOTS=RESIDUALSBYSMOOTH(SMOOTH);
	MODEL martingale = age / SMOOTH=0.2,0.4,0.6,0.8;
RUN;

PROC PHREG DATA=PTSD_data;
	CLASS sexM bmicat combat ptsd / PARAM=REF REF=first;;
	MODEL survweeks*hfailure(0) = age sexM bmicat combat ptsd;
	ASSESS VAR=(age) / RESAMPLE;
RUN;

PROC PHREG DATA=PTSD_data;
	CLASS sexM bmicat combat ptsd / PARAM=REF REF=first;;
	MODEL survweeks*hfailure(0) = age|age sexM bmicat combat ptsd;
	ASSESS VAR=(age age*age) / RESAMPLE;
RUN;

/*4.2 Exploring whether the interaction between age and gender is significant*/
PROC PHREG DATA=PTSD_data;
	CLASS sexM bmicat combat ptsd / PARAM=REF REF=first;;
	MODEL survweeks*hfailure(0) = age|sexM bmicat combat ptsd;
RUN;

/*4.3 Final model*/
PROC PHREG DATA=PTSD_data;
	CLASS sexM bmicat combat ptsd / PARAM=REF REF=first;;
	MODEL survweeks*hfailure(0) = age sexM bmicat combat ptsd;
RUN;

PROC MEANS DATA=PTSD_data;
	VAR age;
RUN;

DATA cov;
	FORMAT sexM fgender. ptsd BEST.;
	INPUT ptsd sexM age;
	DATALINES;
	0 0 50.24
	0 1 50.24
	1 0 50.24
	1 1 50.24
	;
RUN;

PROC PHREG DATA=PTSD_data PLOTS(OVERLAY=GROUP)=(SURVIVAL);
	CLASS sexM ptsd / PARAM=REF REF=first;
	MODEL survweeks*hfailure(0) = age sexM ptsd;
	BASELINE COVARIATES = cov / ROWID=ptsd GROUP=sexM;
RUN;

*5. Check the Proportional Hazard assumption;
/*LOG-LOG plot*/
PROC LIFETEST DATA=PTSD_data PLOTS=(S,LLS) NOTABLE;
	TIME survweeks*hfailure(0);
	STRATA ptsd;
RUN;

/*ZPH test*/
PROC PHREG DATA=PTSD_data ZPH;
	CLASS sexM bmicat combat ptsd / PARAM=REF REF=first;;
	MODEL survweeks*hfailure(0) = age sexM bmicat combat ptsd;
RUN;

PROC PHREG DATA=PTSD_data;
	CLASS sexM bmicat combat ptsd / PARAM=REF REF=first;;
	MODEL survweeks*hfailure(0) = age sexM bmicat ptsd combat1 combat2 / RL;
	combat1 = combat * (survweeks <= 52);
	combat2 = combat * (survweeks > 52);
RUN;

PROC PHREG DATA=PTSD_data;
	CLASS sexM bmicat combat ptsd / PARAM=REF REF=first;;
	MODEL survweeks*hfailure(0) = age sexM bmicat ptsd / RL;
	STRATA combat;
RUN;


*6. Check the influential case;
PROC PHREG DATA=PTSD_data;
	CLASS sexM bmicat combat ptsd / PARAM=REF REF=first;;
	MODEL survweeks*hfailure(0) = age sexM bmicat combat ptsd;
	OUTPUT OUT=dfbeta DFBETA=dfage dfsexM dfbmicat dfcombat dfptsd;
RUN;

PROC SGPLOT DATA=dfbeta;
	SCATTER x=age y=dfage / MARKERCHAR=idnum;
RUN;

PROC PHREG DATA=PTSD_data;
	CLASS sexM bmicat combat ptsd / PARAM=REF REF=first;;
	MODEL survweeks*hfailure(0) = age sexM bmicat combat ptsd;
	OUTPUT OUT=LD LD=ld;
RUN;

PROC SGPLOT DATA=LD;
	SCATTER x=survweeks y=ld / MARKERCHAR=idnum;
RUN;




























