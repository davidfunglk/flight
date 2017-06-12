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
/* UPDATE 07/06/2017
 * Code is now much shorter but requires first using Excel to
 * convert DepTime to HH:MM:SS format with this equation:
 * =TIME(LEFT(AE2,LEN(AE2)-2),RIGHT(AE2,2),0) */
/* NOTE: If batch processing, use a macro function. */
/* Import BTS on-time performance file */
proc import datafile='~/STA 160/Data/OCT12BTS.csv' out=oct12 dbms=csv;
run;
/* Check contents of the newly imported file, no longer needed */
* proc contents varnum;
* run;
/* The departure and arrival times were imported as characters.
   The following code identifies the time values as HHMM, so 
   SAS knows it is a time value. */
data oct12;
	set oct12;
	* DepTimeAdj = input(put(DepTime,4. -l),HHMMSS4.);
	* ArrTimeAdj = input(put(ArrTime,4. -l),HHMMSS4.);
	* drop DepTime ArrTime;
	* rename DepTimeAdj = DepTime ArrTimeAdj = ArrTime;
	* format FlightDate date9.;
	* informat DepTime ArrTime time5.;
	DepDateFull = dhms(FlightDate,0,0,DepTimeAdj);
	* DepDateFull = input(put(FlightDate,date.)||put(DepTimeAdj,time.),datetime.);
	format DepDateFull datetime.;
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
proc sort data=oct12;
	by origin;
run;
/* Match-merge BTS and airports file, dropping missing airports.
   (The airports file included many airports around the world
   that was not in our BTS file, since BTS only has U.S. airports) */
data oct12;
	merge oct12 (in = inoct12) airports (in = inairports);
	by origin;
	if inoct12 and inairports;
	drop name city country icao dst dstbase type source;
	rename lat=DepLat long=DepLong alt=DepAlt offset=DepOffset;
run;
/* Printing first 15 observations to check if everything was done correctly. No longer needed. */
* proc print data=oct12(obs=15);
* run;
/* Adjust time using offset */
data oct12;
	set oct12;
	DepDateUTC = intnx('hour',DepDateFull,-1*DepOffset,'SAME');
	format DepDateUTC datetime.;
run;
/* No longer repeating for arrival */
/* Export result as comma-separated file */
%ds2csv (data=oct12, runmode=b, csvfile='~/STA 160/Data/oct12UTC.csv');
