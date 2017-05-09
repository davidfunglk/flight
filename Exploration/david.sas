/*Importing sampled data*/ 
proc import replace datafile="~/STA 160/pointfive.csv" out=sampleFive dbms=csv; 
	getnames=yes; 
	guessingrows=100; 
proc import datafile="~/STA 160/pointone.csv" out=sampleOne dbms=csv; 
	getnames=yes; 
	guessingrows=100; 
run; 
/*Changing ARR_DELAY variable type to numbers*/ 
data sampleFive; 
	set sampleFive; 
	drop VAR1; 
	ARRDELAY = input(ARR_DELAY,3.0); 
	drop ARR_DELAY; 
	rename ARRDELAY = ARR_DELAY; 
/*Plotting arrival delay in minutes, overlay with normal curve*/ 
proc sgplot data=sampleFive; 
	histogram ARR_DELAY; 
	density ARR_DELAY; 
run; 
/*Simple normality tests for arrival delay in minutes*/ 
proc univariate data=sampleFive normal; 
	var ARR_DELAY; 
	ods select TestsForNormality; 
run; 
/*Scatterplot of year vs delay in minutes*/ 
proc sgplot data=sampleFive; 
	scatter x=YEAR y=ARR_DELAY; 
	ellipse x=YEAR y=ARR_DELAY; 
run; 
/*Boxplot of airline vs delay in minutes*/ 
proc sgplot data=sampleFive; 
	hbox ARR_DELAY / category=CARRIER; 
run; 
/*Boxplot of airport vs delay in minutes*/ 
proc sgplot data=sampleFive; 
	hbox ARR_DELAY / category=ORIGIN; 
run; 
/*Histogram of frequency of origin airport*/ 
proc sgplot data=sampleFive; 
	histogram ORIGIN_AIRPORT_ID; 
run; 
/*Find most frequent origin airports*/ 
proc freq data=sampleFive; 
	tables ORIGIN / out=frequentOrigin; 
run; 
proc sort data=frequentOrigin(obs=20); 
	by descending Count; 
run; 
/*Find most frequent destination airports*/ 
proc freq data=sampleFive; 
	tables DEST / out=frequentDestination; 
run; 
proc sort data=frequentDestination(obs=20); 
	by descending Count; 
run; 
/*Find most frequent airlines*/ 
proc sql; 
	select distinct CARRIER from sampleFive; 
run; 
proc freq data=sampleFive; 
	tables CARRIER / out=frequentCarrier; 
run; 
proc sort data=frequentCarrier; 
	by descending Count; 
run; 
/*Linear regression by airline and top 20 origin airports on arrival delay*/ 
data frequentOriginDrop; 
	set frequentOrigin; 
	keep ORIGIN; 
data sampleOne20; 
	set sampleOne; 
	if ORIGIN = 'ATL' or ORIGIN = 'ORD' or ORIGIN = 'DFW' or ORIGIN = 'DEN' or ORIGIN = 'LAX'
		or ORIGIN = 'PHX' or ORIGIN = 'IAH' or ORIGIN = 'SFO' or ORIGIN = 'LAS' 
		or ORIGIN = 'DTW' or ORIGIN = 'MSP' or ORIGIN = 'CLT' or ORIGIN = 'SLC'
		or ORIGIN = 'EWR' or ORIGIN = 'MCO' or ORIGIN = 'BOS' or ORIGIN = 'LGA'
		or ORIGIN = 'JFK' or ORIGIN = 'SEA' or ORIGIN = 'BWI' then output sampleOne20; 
; 
proc glm data=sampleOne20; 
	class ORIGIN CARRIER; 
	model ARR_DELAY=ORIGIN|CARRIER; 
run;
