/* Import BTS data with UTC adjusted time. See UTCTimeCorrection.sas. */
proc import datafile='~/STA 160/Data/OCT12UTC.csv' out=oct12 dbms=csv;
run;
/* Import NOAA data. Remove row 1 from raw data before import. */
proc import datafile='~/STA 160/Data/NOAA2012.csv' out=noaa2012 dbms=csv;
	datarow=3; getnames=yes; guessingrows=20;
run;
/* Rounding to nearest 6 hour mark. */
proc sort data=oct12;
	by DepDateUTC;
run;
proc sort data=noaa2012;
	by ISO_time;
run;
data oct12round;
	set oct12;
	DepDateUTCRound = round(DepDateUTC, '6:00:00'T);
	format DepDateUTCRound datetime18.;
	informat DepDateUTCRound ANYDTDTM40.;
run;
data noaa2012;
	set noaa2012;
	rename ISO_time = DepDateUTCRound;
run;
data oct12merged;
	merge oct12round noaa2012;
	by DepDateUTCRound;
run;
/* Temporary dropping everything but Hurricane Sandy */
data oct12sandy;
	set oct12merged;
	if Name ne 'SANDY' then delete;
	if Year ne '2012' then delete;
run;
/* Calculate distance */
data oct12distance;
	set oct12sandy;
	LLdist = sqrt((DepLat-Latitude)**2 + (DepLong-Longitude)**2);
	DistInMiles = LLdist*69;
run;





