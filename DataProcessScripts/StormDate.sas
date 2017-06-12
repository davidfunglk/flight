/* 
 * The following code calculates the percent of flight
 * cancellations per day across all U.S. airports. The idea
 * is so we can identify the day where the storm began making
 * its impact across airports, giving us a basis for analysis
 * and drawing comparisons.
 */
/* Import datafile */
proc import datafile='~/STA 160/Data/OCT16UTC.csv' out=oct16 dbms=csv;
run;
data oct16;
	set oct16(where=('30SEP2016'd<FlightDateUTC<'01NOV2016'd));
run;
/* Count all cancelled flights based on origin and date */
proc freq data=oct16;
	where Cancelled = 1;
	tables Origin*FlightDateUTC / out=cancel;
run;
/* Count all scheduled filghts based on origin and date */
proc freq data=oct16;
	tables Origin*FlightDateUTC / out=total;
run;
/* Sorting the data sets in preparation for merge */
data cancel;
	set cancel;
	rename count = cancelcount;
run;
data total;
	set total;
	rename count = totalcount;
run;
proc sort data=cancel;
	by Origin FlightDateUTC;
run;
proc sort data=total;
	by Origin FlightDateUTC;
run;
/* Combining both cancelled and total counts,
   and calculating the percent of flight cancellations
   over total scheduled departures. */
data combined;
	merge cancel total;
	by Origin FlightDateUTC;
	cancelpct = cancelcount/totalcount;
run;
/* Calculate the mean of cancellation percentages across
   all U.S. airports by day and weighted by schedule departures. */
proc means data=combined;
	class FlightDateUTC;
	var cancelpct;
	weight totalcount;
	output out=means(drop=_type_ _freq_) mean= median= std= q1= q3= / autoname;
run;
/* Normality test and finding extreme observations */
proc univariate data=means normal;
	var cancelpct_mean;
run;
/* Density plot of cancellation percentages */
proc sgplot data=means;
	histogram cancelpct_mean;
	density cancelpct_mean;
run;
proc sort data=means;
	by descending cancelpct_mean;
run;
/* It would appear that October 7, 2016, is the day that
   Hurricane Matthew first made its impact across U.S. airports. */
