
/*--------------------------------------------------------------------------------------------*/


/*----------------------------------------------*/
/*       Data Management 		                */
/*----------------------------------------------*/

libname "";
/* Importation des datasets */
proc import datafile="\USRECM.csv"
	out=USRECM
	dbms=csv; delimiter=','; getnames=yes; 
run;
proc import datafile="\BBREC.csv"
	out=BBREC
	dbms=csv; delimiter=','; getnames=yes; 
run;
proc import datafile="\T10Y3MM.csv"
	out=T10Y3MM
	dbms=csv; delimiter=','; getnames=yes; run;
proc import datafile="\TB3MS.csv"
	out=TB3MS
	dbms=csv; delimiter=','; getnames=yes; run;
proc import datafile="\SP500.csv"
	out=SP500
	dbms=csv; delimiter=','; getnames=yes; run;
proc import datafile="\M1SL.csv"
	out=M1SL
	dbms=csv; delimiter=','; getnames=yes; run;
proc import datafile="\M2SL.csv"
	out=M2SL
	dbms=csv; delimiter=','; getnames=yes; run;
proc import datafile="\INDPRO.csv"
	out=INDPRO
	dbms=csv; delimiter=','; getnames=yes; run;
proc import datafile="\IPFUELS.csv"
	out=IPFUELS
	dbms=csv; delimiter=','; getnames=yes; run;
proc import datafile="\UMCSENT.csv"
	out=UMCSENT
	dbms=csv; delimiter=','; getnames=yes; run;
proc import datafile="\CPI.csv"
	out=CPI
	dbms=csv; delimiter=','; getnames=yes; run;
proc import datafile="\UNRATE.csv"
	out=UNRATE
	dbms=csv; delimiter=','; getnames=yes; run;

/* Regroupement et Transformation des variables */ 

* Regroupement des variables
USRECM BBREC T10Y3MM TB3MS SP500 M1SL M2SL INDPRO IPFUELS UMCSENT CPI UNRATE ; 
data monthly ;
	set USRECM ; set BBREC ; 
	set T10Y3MM ; set TB3MS ; 
	set SP500 ; 
	set M1SL ; set M2SL ;
	set INDPRO ; set IPFUELS ;  set UMCSENT ; set CPI ; set UNRATE ; 
run;

proc univariate data=monthly ;
	var SP500 M1SL M2SL INDPRO IPFUELS UMCSENT ; histogram ; run;

* Tout mettre en dif , USRECM BBREC T10Y3MM TB3MS SP500 M1SL M2SL INDPRO IPFUELS UMCSENT CPI UNRATE
	et log pour tout sauf taux et spread : SP500 M1SL M2SL INDPRO IPFUELS UMCSENT ;
data monthly ;
	set monthly ;
	SP500 = log(SP500) ;
	M1SL = log(M1SL) ;
	M2SL = log(M2SL) ;
	INDPRO = log(INDPRO) ;
	IPFUELS = log(IPFUELS) ;
	UMCSENT = log(UMCSENT) ;
run; quit; 
data monthlydifall ;
	set monthly ;
	T10Y3MM = dif(T10Y3MM);
	TB3MS = dif(TB3MS) ;
	SP500 = dif(log(SP500)) ;
	M1SL = dif(log(M1SL)) ;
	M2SL = dif(log(M2SL)) ;
	INDPRO = dif(log(INDPRO)) ;
	IPFUELS = dif(log(IPFUELS)) ;
	UMCSENT = dif(log(UMCSENT)) ;
	CPI = dif(CPI) ;
	UNRATE = dif(UNRATE) ;
run; quit; 

proc univariate data=monthly ;
	var SP500 M1SL M2SL INDPRO IPFUELS UMCSENT ; histogram ; run;
* Enleve les valeurs manquantes ;
data monthlydifall ; 
	set monthlydifall ;
	if sp500 = '.' then delete ;
run;

/* Exploring data files */

* monthly ;
proc contents data = monthly; run;
proc univariate data=monthly ;
	var USRECM ; histogram ; run;
proc freq data=monthly ;
	tables USRECM;
run;

/*----------------------------------------------*/
/*       Modelisation	 		        */
/*----------------------------------------------*/

/*--- HPLOGISTIC -------------------------------*/

* forcer a garder le spread possible de faire avec logistic simple ;

* USRECM 
T10Y3MM TB3MS SP500 M1SL M2SL INDPRO IPFUELS UMCSENT CPI UNRATE ;
proc hplogistic alpha=0.01 data=monthlydifall ;
	partition fraction(test=0.4);
	model USRECM(event='1') = T10Y3MM TB3MS SP500 M1SL M2SL INDPRO IPFUELS UMCSENT CPI UNRATE 
	/ alpha=0.01 association ctable=Roc ; * prendre les variables en differences;
	output out=Out xbeta predicted=Pred ;
	selection METHOD=backward details=all ; 
run; 
proc sgplot data=Roc aspect=1 noautolegend ;
	title 'ROC curve' ;
	xaxis values=(0 to 1 by 0.25) grid offsetmin=0.05 offsetmax=0.05;
	yaxis values=(0 to 1 by 0.25) grid offsetmin=0.05 offsetmax=0.05;
	lineparm x=0 y=0 slope=1 / lineattrs=(color=ligr) ;
	series x=FPF y=TPF ;
	inset 'Area under the curve for USRECM' / position=bottomright;
	
run; 

* BBREC 
GS10 T10Y3MM TB3MS SP500 M1SL M2SL INDPRO IPFUELS UMCSENT CPI UNRATE ;
proc hplogistic data=monthlydifall ;
	partition fraction(test=0.4);
	model bbrec(event='1') = T10Y3MM TB3MS SP500 M1SL M2SL INDPRO IPFUELS UMCSENT CPI UNRATE / association ctable=BBRoc ;
	selection METHOD=backward details=all ; 
run; 
proc sgplot data=BBRoc aspect=1 noautolegend ;
	title 'ROC curve' ;
	xaxis values=(0 to 1 by 0.25) grid offsetmin=0.05 offsetmax=0.05;
	yaxis values=(0 to 1 by 0.25) grid offsetmin=0.05 offsetmax=0.05;
	lineparm x=0 y=0 slope=1 / lineattrs=(color=ligr) ;
	series x=FPF y=TPF ;
	inset 'Area under the curve for BBROC' / position=bottomright;
run; 

/*--- HPFOREST ---------------------------------*/

/* Creation des lags */

* MONTHLYLAG 
GS10 T10Y3MM TB3MS SP500 M1SL M2SL INDPRO IPFUELS UMCSENT CPI UNRATE ;

data monthlylag ; set monthlydifall ; run; 
%macro lag_T10Y3MM();  
%do i=1 %to 10; 
data monthlylag ; set monthlylag;   
T10Y3MM_lag&i. = lag&i.(T10Y3MM); 
%end; run;
%mend;
%macro lag_TB3MS();  
%do i=1 %to 10; 
data monthlylag ; set monthlylag;   
TB3MS_lag&i. = lag&i.(TB3MS); 
%end; run;
%mend;
%macro lag_sp500();  
%do i=1 %to 10; 
data monthlylag ; set monthlylag;   
sp500_lag&i. = lag&i.(sp500); 
%end; run;
%mend;
%macro lag_M1SL();  
%do i=1 %to 10; 
data monthlylag ; set monthlylag;   
M1SL_lag&i. = lag&i.(M1SL); 
%end; run;
%mend;
%macro lag_M2SL();  
%do i=1 %to 10; 
data monthlylag ; set monthlylag;   
M2SL_lag&i. = lag&i.(M2SL); 
%end; run;
%mend;
%macro lag_INDPRO();  
%do i=1 %to 10; 
data monthlylag ; set monthlylag;   
INDPRO_lag&i. = lag&i.(INDPRO); 
%end; run;
%mend;
%macro lag_IPFUELS();  
%do i=1 %to 10; 
data monthlylag ; set monthlylag;   
IPFUELS_lag&i. = lag&i.(IPFUELS); 
%end; run;
%mend;
%macro lag_UMCSENT();  
%do i=1 %to 10; 
data monthlylag ; set monthlylag;   
UMCSENT_lag&i. = lag&i.(UMCSENT); 
%end; run;
%mend;
%macro lag_CPI();  
%do i=1 %to 10; 
data monthlylag ; set monthlylag;   
CPI_lag&i. = lag&i.(CPI); 
%end; run;
%mend;
%macro lag_UNRATE();  
%do i=1 %to 10; 
data monthlylag ; set monthlylag;   
UNRATE_lag&i. = lag&i.(UNRATE); 
%end; run;
%mend;
%lag_T10Y3MM();  %lag_TB3MS();  
%lag_sp500(); %lag_M1SL(); %lag_M2SL(); 
%lag_INDPRO(); %lag_IPFUELS(); %lag_UMCSENT(); 
%lag_CPI(); %lag_UNRATE();  

* valeurs manquantes ;
data monthlylag ; 
	set monthlylag ;
	if sp500_lag10 = '.' then delete ;
run;

* Monthly
USRECM(event='1') GS10_lag: T10Y3MM_lag: TB3MS_lag: SP500_lag: M1SL_lag: M2SL_lag: INDPRO_lag: IPFUELS_lag: 
	UMCSENT_lag: CPI_lag: UNRATE_lag:
vars_to_try mettre racine carree de nb var 113 ;
proc hpforest data=monthlylag
	vars_to_try=11 maxtrees=300 trainfraction=0.6
	maxdepth=15 leafsize=10
	alpha=0.1;
	target USRECM / level=binary;
	input T10Y3MM_lag: TB3MS_lag: SP500_lag: M1SL_lag: M2SL_lag: INDPRO_lag: IPFUELS_lag: 
	UMCSENT_lag: CPI_lag: UNRATE_lag: / level=interval ;
	ods output fitstatistics = fitstats(rename=(Ntrees=Trees)) ;
run;

* BBREC ;
proc hpforest data=monthlylag
	vars_to_try=11 maxtrees=300 trainfraction=0.6
	maxdepth=15 leafsize=10
	alpha=0.1;
	target BBREC / level=binary;
	input T10Y3MM_lag: TB3MS_lag: SP500_lag: M1SL_lag: M2SL_lag: INDPRO_lag: IPFUELS_lag: 
	UMCSENT_lag: CPI_lag: UNRATE_lag: / level=interval ;
	*save file ="\SAS Memoire S2\monthlyrf.csv";
	ods output fitstatistics = fitstats(rename=(Ntrees=Trees)) ;
run;

/*Gradient Boosting*/
/*Split data into TRAIN and TEST datasets at an 60/40 split
USRECM(event='1') GS10_lag: T10Y3MM_lag: TB3MS_lag: SP500_lag: M1SL_lag: M2SL_lag: INDPRO_lag: IPFUELS_lag: 
	UMCSENT_lag: CPI_lag: UNRATE_lag:*/
PROC SURVEYSELECT DATA= monthlydifall rat = 0.6
	OUT= selection OUTALL METHOD=SRS;
RUN;
data reces_train reces_test; 
set selection; 
if selected = 1 then output reces_train; 
else output reces_test; 
run;
PROC TREEBOOST DATA = reces_train
	CATEGORICALBINS = 10
	INTERVALBINS = 100
	EXHAUSTIVE = 5000
	INTERVALDECIMALS = MAX
	LEAFSIZE = 32
	MAXBRANCHES = 2
	ITERATIONS = 1000
	MINCATSIZE = 50
	MISSING = USEINSEARCH
	SEED = 42
	SHRINKAGE = 0.1
	SPLITSIZE = 100
	TRAINPROPORTION = 0.6 ;
	INPUT  T10Y3MM TB3MS SP500 M1SL M2SL INDPRO IPFUELS UMCSENT CPI UNRATE/ LEVEL = INTERVAL ;
	TARGET USRECM / LEVEL = BINARY ;
	importance nvars=50 outfit=base_vars ;
	subseries best;
	code file="\memoire sas 2e\tbmonthly.bin" ;
	save model = gbs_test fit=fit_stats importance=importance rules=rules ;
RUN;
