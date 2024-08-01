/* NEMT-CUSUM Macro */

%macro countr(df,n,c,delta,mu0,time_var,lib,mvar,type);

/* Get Frequencies > mu0 */

%local i col_num name2 name3 dfnames;

/* Create Macro to Get Column Names */

%macro get_cols(lib,df,mvar,type);

   %global &mvar;

   proc sql noprint;
      select
         name
      into
         :&mvar separated by ' '
      from
         dictionary.columns
      where
             libname eq upcase("&lib")
         and memname eq upcase("&df")

         %if %upcase(&type) ne ALL %then
         and upcase(type) eq upcase("&type");

      ;
   quit;

   %put &mvar = &&&mvar;
%mend get_cols;

%get_cols(&lib,&df,&mvar,&type);

%do i=2 %to %sysfunc(countw(&vars));
   %let col_num = %scan(&vars, &i);
   %let name2 = %sysfunc(catx(_,&col_num,1));
   %let name3 = %sysfunc(catx(_,&col_num,2));
   proc sql;
   create table &col_num as
   select &time_var, &col_num from &df;
   create table &name2 as 
   select * from &col_num
   where &col_num > &mu0;
   create table &name3 as
   select &time_var,count(*) as frequency
   from &name2
   group by &time_var;
   quit;
   data &name3;
   set &name3;
   ID = "&col_num";
   run;
%end;

/* Vertically Join _2 datasets */

proc sql noprint;
select distinct memname
into :dfnames separated by " "
from dictionary.columns
where libname eq "WORK" and memname contains "_2";
quit; 

data nemt_cusum_file;
set &dfnames;
run;

/* Calculate NEMT-CUSUM Statistics */

data nemt_cusum_file;
set nemt_cusum_file;
Zt = (frequency - %sysevalf(&n*0.50))/(%sysevalf(&n*0.25)**0.50);
run;

proc sql noprint;
create table np_plot as
select &time_var, sum(Zt) as EMT
from nemt_cusum_file
group by &time_var;
quit;

data np_plot;
set np_plot;
retain St;
St+EMT;
St1 = lag(St);
if St1 = . then UL = %sysevalf(&delta*(&&C)**0.50);
else UL = St1 + %sysevalf(&delta*(&&C)**0.50);
if St1 = . then LL =  %sysevalf(-1*&delta*(&&C)**0.50);
else LL = 2*St1 - UL;
/* Any OOC points to identify? */
if St > UL or St < LL then OOC = hour;
run;

/* Generate Plot */

proc sgplot data=np_plot;
series x=&time_var y=St/markers;
series x=&time_var y=UL/markers;
series x=&time_var y=LL/markers;
xaxis integer ;
run;

%mend countr;