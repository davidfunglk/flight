/*Importing sampled data*/ 
proc import replace datafile="~/STA 160/pointfive.csv" out=sampleFive dbms=csv; 
	getnames=yes; 
	guessingrows=100; 
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
