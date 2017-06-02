/* ************************************************************** *
 * The following code adds UTC departure and arrival times to     *
 * the airline on-time performance data from BTS. In order to     *
 * calculate the existing UTC offset, information was taken       *
 * from an open-source database on airport attributes. You        *
 * can find the information at https://openflights.org/data.html. *
 * Specifically, we used 'airports.dat' that was made available   *
 * on the site, distributed under Open Database License (ODbL)    *
 * v1.0.     												      *
 * ************************************************************** */
/* NOTE: If batch processing, use a macro function. */
/* Import BTS on-time performance file */
proc import datafile='~/STA 160/Data/OCT16BTS.csv' out=oct16 dbms=csv;
run;
/* Check contents of the newly imported file */
proc contents varnum;
run;
/* The departure and arrival times were imported as characters.
   The following code identifies the time values as HHMM, so 
   SAS knows it is a time value. */
data oct16;
	set oct16;
	DepTimeAdj = input(DepTime,HHMM.);
	ArrTimeAdj = input(ArrTime,HHMM.);
	drop DepTime ArrTime;
	rename DepTimeAdj = DepTime ArrTimeAdj = ArrTime;
run;
/* Import 'airports.dat' from OpenFlights */
data airports;
	infile '~/STA 160/Data/airports.dat' dlm=',' dsd;
	input id name $ city $ country $ origin $ icao $ lat long alt offset $ dst $ dstbase $ type $ source $;
	drop id;
run;
/* Sort both BTS and airports file for merging */
proc sort data=airports;
	by origin;
run;
proc sort data=oct16;
	by origin;
run;
/* Match-merge BTS and airports file, dropping missing airports.
   (The airports file included many airports around the world
   that was not in our BTS file, since BTS only has U.S. airports) */
data oct16;
	merge oct16 (in = inoct16) airports (in = inairports);
	by origin;
	if inoct16 and inairports;
	drop name city country icao dst dstbase type source;
	rename lat=DepLat long=DepLong alt=DepAlt offset=DepOffset;
run;
/* Printing first 15 observations to check if everything was done correctly. */
proc print data=oct16(obs=15);
run;
/* From UTC offset, change local time to UTC time by adding the corresponding hours. */
data oct16;
	set oct16;
	DepOffsetRev = DepOffset * 100 * -1;
	DepUTC = DepTime + DepOffsetRev;
run;
/* Repeating the above process for arrival times */
data airports;
	set airports;
	rename origin = dest;
run;
proc sort data=airports;
	by dest;
run;
proc sort data=oct16;
	by dest;
run;
data oct16;
	merge oct16 (in = inoct16) airports (in = inairports);
	by dest;
	if inoct16 and inairports;
	drop name city country icao dst dstbase type source;
	rename lat=ArrLat long=ArrLong alt=ArrAlt offset=ArrOffset;
run;
data oct16;
	set oct16;
	ArrOffsetRev = ArrOffset * 100 * -1;
	ArrUTC = ArrTime + ArrOffsetRev;
run;
/* Date and time adjustment - previously did not account for date change.
   1. Should rewrite the code to concatenate date and time, then adjust
   for UTC offset.
   2. Discovered the time formatting did not work. Manually adjusted for
   date. Appears to be a SAS Studio problem. */
data oct16;
	set oct16;
	FlightDateUTC = intnx('day',FlightDate,0);
	if DepUTC > 2400 then do;
		DepUTC = DepUTC - 2400;
		FlightDateUTC = intnx('day',FlightDate,1);
		end;
	if DepUTC < 0 then do;
		DepUTC = DepUTC + 2400;
		FlightDateUTC = intnx('day',FlightDate,-1);
		end;
	format FlightDateUTC date.;
run;
/* Export result as comma-separated file */
%ds2csv (data=oct16, runmode=b, csvfile='~/STA 160/Data/oct16UTC.csv');
