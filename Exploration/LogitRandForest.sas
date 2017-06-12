proc import datafile='~/STA 160/Data/OCT12SANDYDIST.csv' out=oct12 dbms=csv;
run;
ods trace on;
proc logistic data=oct12;
	class Nature / param=reference;
	model cancelled(event='1') = distance|wind_WMO_|pres_wmo_|nature / link=probit ctable;
run;
proc hpforest;
	target cancelled / level=nominal;
	input nature / level=nominal;
	input distance wind_wmo_ pres_wmo_ / level=interval;
run;

