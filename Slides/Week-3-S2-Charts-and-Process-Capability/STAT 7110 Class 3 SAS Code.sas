/* STAT 8110 Class 3 SAS Code */

/* Import Piston Rings Data */

proc import datafile="/home/u44238988/sasuser.v94/STAT 7110/Class 3/Piston Rings.xlsx"
out=rings
dbms=xlsx replace;
getnames=YES;
run;

/* Transform Data from Wide to Long */

proc transpose data=rings out=rings1(DROP=_NAME_ _LABEL_);
var Obs1-Obs5;
by Sample;
run;

/* Use PROC SHEWHART to plot X-bar & S Charts */

proc sort data=rings1;
by Sample;
run;

proc shewhart data=rings1;
xschart COL1*Sample;
run;

/* Variable Sample Size X-bar & S Charts */

proc import datafile="/home/u44238988/sasuser.v94/STAT 7110/Class 3/Piston Rings - Variable Sample Size.xlsx"
out=rings2
dbms=xlsx replace;
getnames=YES;
run;

proc transpose data=rings2 out=rings3(DROP=_NAME_ _LABEL_);
var Obs1-Obs5;
by Sample;
run;

proc shewhart data=rings3;
xschart COL1*Sample/outlimits=var_ss;
run;

/* Creating a Phase I S-Squared Control Chart */

%let alpha = 0.0027;
%let n = 5;

proc sql noprint;
create table s2s as
select var(COL1) as s2
from rings1
group by Sample;
select mean(s2)
into : sbar2
from s2s;
quit;

data _NULL_;
UpperQ = quantile("CHISQ",1-%sysevalf(&alpha/2),%sysevalf(&n-1));
LowerQ = quantile("CHISQ",%sysevalf(&alpha/2),%sysevalf(&n-1));
call symputx("UpperQ",UpperQ);
call symputx("LowerQ",LowerQ);
UCL = %sysevalf(&sbar2*&UpperQ/(&n-1));
LCL = %sysevalf(&sbar2*&LowerQ/(&n-1));
call symputx("UCL",UCL);
call symputx("LCL",LCL);
run;

%put &=LCL;

data s2s;
set s2s;
obs = monotonic();
UCL = &UCL;
LCL = &LCL;
run;

title "S2 Control Chart";
proc sgplot data=s2s;
series x=obs y=s2 / markers;
series x=obs y=UCL;
series x=obs y=LCL;
run;
title;

/* Control Chart for Individual Measurements & Moving Ranges */

proc import datafile="/home/u44238988/sasuser.v94/STAT 7110/Class 3/Mortgage Loan Costs.xlsx"
out=mortgages
dbms=xlsx replace;
getnames=YES;
run;

proc shewhart data=mortgages;
irchart cost*week;
run;

