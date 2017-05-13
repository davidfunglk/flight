data orddepart ordarrive; 
	set sampleFive; 
	if ORIGIN='ORD' then output orddepart; 
	if DEST='ORD' then output ordarrive; 
run; 
data ordonly; 
	set orddepart ordarrive; 
run; 
proc sort data=ordonly; 
	by descending MONTH YEAR; 
run; 
proc sgplot data=ordonly; 
	heatmap x=YEAR y=MONTH / name="heat" colorResponse=ARR_DELAY colorstat=mean discretey discretex 
	colormodel=(white darkred); 
run; 
proc means data=ordonly; 
	class YEAR MONTH; 
	var ARR_DELAY; 
	output out=ordavg mean=avg; 
run; 
data ordavg; 
	set ordavg; 
	date = input(FL_DATE,YYMMDD10.); 
	format date DATE9.; 
	if YEAR = '.' then delete; 
	if MONTH = '.' then delete; 
run; 
proc sort data=ordonly; 
	by YEAR MONTH; 
run; 
proc sgplot data=ordavg; 
	series x=YEAR y=avg / group=MONTH markers break smoothconnect; 
run; 
proc sgplot data=ordavg; 
	vbar YEAR / response=_freq_; 
	label _freq_='Total Flight Observations'; 
run; *It seems as though that more runways, less flights result in more delays; 
 
 
 
 
