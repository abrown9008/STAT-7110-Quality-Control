/* STAT 7110 Class 4 SAS Code */

/* How do we find out which points plot out of control */
/* Without using the control chart (Phase I)? */

/* Q 6.7 From HW 1 */

/* Read in Data */

proc import datafile="/home/u44238988/sasuser.v94/STAT 7110/Class 4/Table 6E1.xlsx"
out=bearings
dbms=xlsx replace;
getnames=YES;
sheet="Sheet2";
run;

/* Add Sample Size Variable & Fix Sample Number */

options validvarname=ANY;
data bearings;
set bearings;
n = 5;
"Sample Number"n = monotonic();
run;

/* Plot Shewhart X-bar & R Charts */

proc sort data=bearings;
by "Sample Number"n;
run;

/* Outputting Limits */

/* Note, if our data were in raw format, we'd also use the outhistory option
 in the xrchart statment */

proc shewhart history=bearings (rename=(X = DiameterX
										R = DiameterR
										n = Diametern));
										
xrchart Diameter*"Sample Number"n/outlimits=out_lim 
								  outtable=out_tab;/*outhistory = summary_data*/
run;

/* Join Control Limits to Bearings Data Table */

data _NULL_;
set out_lim;
call symputx("LCLx",_LCLx_);
call symputx("UCLx",_UCLx_);
call symputx("LCLr",_LCLr_);
call symputx("UCLr",_UCLr_);
run;

data bearings1;
set bearings;
LCLx = &LCLx;
UCLx = &UCLx;
LCLr = &LCLr;
UCLr = &UCLr;
run;

/* Now, using PROC SQL, we can compare each of our plotting statistics
	to their respective control limits to determine which samples are OOC */
	
/* Checking X-bars */

proc sql;
select "Sample Number"n,X,LCLx,UCLx
from bearings1
where X > UCLx or X < LCLx;
quit; 

/* Checking Rs */

proc sql;
select "Sample Number"n,R,LCLr,UCLr
from bearings1
where R > UCLr or R < LCLr;
quit; 

/* P-Charts */

/* 5 Year Graduation Rate Problem */

/* Read in Data */

proc import datafile="/home/u44238988/sasuser.v94/STAT 7110/Class 4/Enrollment.xlsx"
out=enrollment
dbms=xlsx replace;
getnames=YES;
run;

/* Add ID Variable */

data enrollment;
set enrollment;
ID = monotonic();
run;

proc shewhart data=enrollment;
pchart "Number Not Re-Enrolling"n*ID/subgroupn=50;
run;

/* Orange Juice Can Problem - Phase I */

/* Read in Data */

proc import datafile="/home/u44238988/sasuser.v94/STAT 7110/Class 4/Orange Juice Cans.xlsx"
out=ojc
dbms=xlsx replace;
getnames=YES;
sheet="Sheet1";
run;

/* Add ID Variable */

data ojc;
set ojc;
ID = monotonic();
run;

/* Plot P-Chart */

proc shewhart data=ojc;
pchart "Number of Nonconforming Cans"n*ID/subgroupn="Sample Size"n
										  outlimits=ojc_lims
										  outhistory=ojc_phats
										  outtable=ojc_tab;
run;

/* Alternative Method for Obtaining OOC Points */

data ojc_tab1;
set ojc_tab;
if _EXLIM_ = " " then delete;
run;

/* Determining Which Observations are OOC */

data _NULL_;
set ojc_lims;
call symputx("LCLp",_LCLp_);
call symputx("UCLp",_UCLp_);
run;

%put &=LCLp and &=UCLp;

data ojc_phats1;
set ojc_phats;
LCLp = &LCLp;
UCLp = &UCLp;
run;

proc sql;
select ID,"Number of Nonconforming CansP"n,UCLp,LCLp
from ojc_phats1
where  "Number of Nonconforming CansP"n> UCLp or "Number of Nonconforming CansP"n < LCLp;
quit;

/* Remove OBS 15 & 23 and replot */

data ojc1;
set ojc;
if ID = 15 then delete;
if ID = 23 then delete;
drop ID;
run;

data ojc1;
set ojc1;
ID = monotonic();
run;

/* Plot P-Chart */

proc shewhart data=ojc1;
pchart "Number of Nonconforming Cans"n*ID/subgroupn="Sample Size"n
										  outlimits=ojc_lims
										  outhistory=ojc_phats
										  outtable=ojc1_tab;
run;

/* Alternative Method for Obtaining OOC Points */

data ojc1_tab1;
set ojc1_tab;
if _EXLIM_ = " " then delete;
run;

/* Determining Which Observations are OOC */

data _NULL_;
set ojc_lims;
call symputx("LCLp",_LCLp_);
call symputx("UCLp",_UCLp_);
run;

data ojc_phats1;
set ojc_phats;
LCLp = &LCLp;
UCLp = &UCLp;
run;

proc sql;
select ID,"Number of Nonconforming CansP"n,UCLp,LCLp
from ojc_phats1
where  "Number of Nonconforming CansP"n> UCLp or "Number of Nonconforming CansP"n < LCLp;
quit;

/* Remove OBS 20 */

data ojc2;
set ojc1;
if ID = 20 then delete;
drop ID;
run;

data ojc2;
set ojc2;
ID = monotonic();
run;

/* Plot P-Chart */

proc shewhart data=ojc2;
pchart "Number of Nonconforming Cans"n*ID/subgroupn="Sample Size"n
										  outlimits=ojc_lims
										  outhistory=ojc_phats;
run;

/* Okay great! The chart is now in control */

/* Let's plot the Phase II Points */

/* Read in Phase II Samples */

proc import datafile="/home/u44238988/sasuser.v94/STAT 8110/Class 4/Orange Juice Cans.xlsx"
out=ojcp2
dbms=xlsx replace;
getnames=YES;
sheet="Sheet2";
run;

/* Create ID Variable */

data ojcp2;
set ojcp2;
ID = monotonic();
run;

/* Plot Phase II Chart */

proc shewhart data=ojcp2 limits=ojc_lims;
pchart "Number of Nonconforming Cans"n*ID/subgroupn="Sample Size"n;
run;

/* Notice that while no points plot OOC, it looks like there is a */
/* non-random pattern that seems like a downward shift has taken place */

/* Let's test the Phase II data against the Phase I data in a proportions test! */

data ojc21;
set ojc2;
Number_of_Conforming_Cans = "Sample Size"n - "Number of Nonconforming Cans"n;
Group="P1";
run;

data ojcp21;
set ojcp2;
Number_of_Conforming_Cans = "Sample Size"n - "Number of Nonconforming Cans"n;
Group="P2";
run;

proc sql noprint;
create table can_test as
select * from ojc21
union all 
select * from ojcp21;
quit;

proc sql;
create table can_test1 as
select Group, sum(Number_of_Conforming_Cans) as Conforming,
	   sum("Number of Nonconforming Cans"n) as Nonconforming
from can_test
group by Group;
quit;

proc transpose data=can_test1 out=can_test2;
var Conforming Nonconforming;
by Group;
run;

proc freq data=can_test2;
weight COL1;
table Group*_NAME_/ chisq riskdiff;
run; 

/* Since we have evidence that the Phase II proportion is significantly */
/* less than the Phase I proportion, it would be advisable to recalculate */
/* the control limits */ 

proc shewhart data=ojcp2;
pchart "Number of Nonconforming Cans"n*ID/subgroupn="Sample Size"n;
run;

/* Calculating Sample Size for P-Chart Using Macro Function */

%macro pchart_ss(prob,p);
data _NULL_;
n = CEIL(LOG(1-&prob)/LOG(1-&p));
call symputx("n",n);
run;
%put The required sample size to obtain at least one nonconforming observation with 
&prob probability is &=n;
%mend ;

%pchart_ss(prob=0.95,p=0.01);

/* Calculating Sample Size for Positive Lower Control Limit Using Macro Function */

%macro nonzero_lcl_ss(p,L);
data _NULL_;
n = CEIL((&L**2)*(1-&p)/&p);
call symputx("n",n);
run;
%put The required sample size to obtain a positive lower control limit assuming a fraction
nonconforming of &p is &=n;
%mend;

%nonzero_lcl_ss(p=0.05,L=3);

/* P-Chart for PO Data */

/* Read In Data */

proc import datafile="/home/u44238988/sasuser.v94/STAT 7110/Class 4/POs.xlsx"
out=po_dat
dbms=xlsx replace;
getnames=YES;
run;

/* Plot Phase I Chart */

proc shewhart data=po_dat;
pchart "Incorrect POs"n*"Sample Number"n/subgroupn="Sample Size"n
										 outtable=var_dat;
run;

/* Determing OOC Points */

data var_dat1;
set var_dat;
if _SUBP_ > _UCLp_ OR _SUBP_ < _LCLp_ then Indicator = "Sample Number"n;
else Indicator = 0;
run;

/* Remove OOC Point and Re-Plot Chart */

data var_dat2;
set var_dat1;
if Indicator ^= 0 then delete;
drop Indicator;
"Sample Number"n = monotonic();
run;

/* Notice, this is how we can plot data that's already in proportion form */

proc shewhart data=var_dat2;
pchart _SUBP_*"Sample Number"n/subgroupn=_SUBN_
							   dataunit=proportion;
run;

/* Plotting OC Curve from Orange Juice Example */

proc shewhart data=ojcp2;
pchart "Number of Nonconforming Cans"n*ID/subgroupn="Sample Size"n
										  outlimits=orange_juice_limits;
run;

"https://documentation.sas.com/?docsetId=qcug&docsetTarget=qcug_code_shwpoc.htm&docsetVersion=15.1&locale=en";

/* Calculating ARL0 */

data _NULL_;
set orange_juice_limits;
alpha = 1 - (cdf("binomial",round(50*_UCLp_),_P_,50) - 
cdf("binomial",round(50*_LCLp_),_P_,50));
ARL0 = 1/alpha;
call symputx("alpha",alpha);
call symputx("ARL0",ARL0);
run;

%put &=alpha, &=ARL0;

/* Calculating ARL1 when p = 0.27 */

%let p=0.27;

data _NULL_;
set orange_juice_limits;
beta = cdf("binomial",round(50*_UCLp_),&p,50) - 
cdf("binomial",round(50*_LCLp_),&p,50);
ARL1 = 1/(1-beta);
call symputx("beta",beta);
call symputx("ARL1",ARL1);
run;

%put &=beta, &=ARL1;