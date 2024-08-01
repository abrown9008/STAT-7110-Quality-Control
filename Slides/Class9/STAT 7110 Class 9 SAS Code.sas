/* STAT 8110 Class 9 SAS Code */

/* Customer Service Hold Times */

/* Read in Data */

proc import datafile="/home/u44238988/sasuser.v94/STAT 7110/Class 9/Class9.xlsx"
out=hold
dbms=xlsx replace;
getnames=YES;
run;

/* Use PROC MACONTROL */

proc macontrol data=hold;
ewmachart Time*Call / weight = 0.10  
                      sigmas=2.7
                      sigma0 = 1
                      mu0 = 10
                      outtable=hold_out;
run;

/* Determine which observations were OOC */

proc sql;
select * from hold_out
where _EXLIM_ ne " ";
quit;

/* Phase I Analysis: Dunder-Mifflin Fuel Costs */

proc import datafile="/home/u44238988/sasuser.v94/STAT 7110/Class 9/Class9.xlsx"
out=fuel
dbms=xlsx replace;
getnames=YES;
sheet="Sheet2";
run;

/* Add New ID Column */

data fuel;
set fuel;
month1 = monotonic();
keep month1 cost;
rename month1 = month;
run;

proc macontrol data=fuel;
ewmachart Cost*month / weight = 0.20
					   sigmas=2.962
					   outtable=fuel_out;
run;

/* If we want to use the regular estimate of SD */

proc sql;
select std(Cost)
into :sig_hat
from fuel;
select mean(Cost)
into :xbar
from fuel;
quit;

%put The standard deviation is &=sig_hat and the mean is &=xbar;

proc macontrol data=fuel;
ewmachart Cost*month / weight = 0.20
					   sigmas=2.962
					   sigma0 = &sig_hat
					   outtable=fuel_out
					   outlimits=dundy_lims;
run;

/* Read in Phase II Data */

proc import datafile="/home/u44238988/sasuser.v94/STAT 7110/Class 9/Class9.xlsx"
out=fuel2
dbms=xlsx replace;
getnames=YES;
sheet="Sheet3";
run;

proc macontrol data=fuel2 limits=dundy_lims;
ewmachart Cost*month / weight = 0.20
					   sigmas=2.962
					   outtable=fuel2_out;
run;

/* ACF in SAS */

proc arima data=fuel;
identify var=cost;
run;