
DATA  HRBITBL.ATTRITIONDATA_R3;
	SET all /*(Rename= (Gender=Gender1))*/;
	Length 'Rating Bracket'n $ 4 'Experience Range'n $ 11;

/*Distance2 = compress(DistanceInKms, "km");*/
/*Distance = input(Distance2, 8.);*/

if DistanceInKms ~= '' then Distance = input(compress(DistanceInKms, "km ,"), 8.);

IF DateOfRelieving =. THEN Availability_Filter = "Current Employee";
	ELSE Availability_Filter = "Employee Left";
if DateOfRelieving =. THEN Available = 1;
	else Available = 0;
if DateOfRelieving ~=. THEN Attrition = 1;
	else Attrition= 0;

 if MaritalStatus = 'UnMarried' then MaritalState = 0;
	else if MaritalStatus = 'Married' then MaritalState = 1;
	else MaritalState = .;

	if (LastReviewRating >= 4 and DateOfRelieving >'01jan2010'd ) then 'Top Performer'n = 1;
else 'Top Performer'n = 0;
if ExitType = 'Involuntary' then 'Involuntary Exit'n = 1;
else 'Involuntary Exit'n = 0;
if ExitType = 'Voluntary' then 'Voluntary Exit'n = 1;
else 'Voluntary Exit'n = 0;
if ExitType not in ('Involuntary', 'Voluntary','')
then 'Early Exit'n = 1;
else 'Early Exit'n = 0;
'VolInVol Attrition'n = 'Involuntary Exit'n + 'Voluntary Exit'n;
RelievingMonth = DateOfRelieving;
RelievingYear = DateOfRelieving;
RelievingMonYY = DateOfRelieving;
JoiningMonth = DateofJoin;
JoiningYear = DateofJoin;
JoiningMonYY = DateofJoin;

IF AGSEXPERIENCEINMONTHS =< 1 THEN 'Experience Range'n = "<= 01 ";
ELSE IF 1 < AGSEXPERIENCEINMONTHS =< 3 THEN 'Experience Range'n = "01-03 ";
ELSE IF 3 < AGSEXPERIENCEINMONTHS =< 6 THEN 'Experience Range'n = "03-06 ";
ELSE IF 6 < AGSEXPERIENCEINMONTHS =< 12 THEN 'Experience Range'n = "06-12 ";
ELSE IF 12 < AGSEXPERIENCEINMONTHS =< 24 THEN 'Experience Range'n = "12-24 ";
ELSE IF 24 < AGSEXPERIENCEINMONTHS =< 36 THEN 'Experience Range'n = "24-36 ";
ELSE IF 36 < AGSEXPERIENCEINMONTHS =< 60 THEN 'Experience Range'n = "36-60 ";
ELSE IF AGSEXPERIENCEINMONTHS > 60 THEN 'Experience Range'n = "60 < ";
ELSE 'Experience Range'n = "Check Month";

if JobRole = 'Team Member' then do;
if LastReviewRating = "" then 'Rating Bracket'n = "NA";
	else if LastReviewRating < 2  then 'Rating Bracket'n = "E";
	else if LastReviewRating < 2.70  then 'Rating Bracket'n = "D";
	else if LastReviewRating < 3 then 'Rating Bracket'n = "C";
	else if LastReviewRating < 3.5 then 'Rating Bracket'n = "B";
	else if LastReviewRating < 4 then 'Rating Bracket'n = "B +";
	else if LastReviewRating < 4.5 then 'Rating Bracket'n = "A";
	else if LastReviewRating < 5 then 'Rating Bracket'n = "A +";
	else 'Rating Bracket'n = "NA";
end;
else do;
if LastReviewRating = "" then 'Rating Bracket'n = "NA";
/*	else if LastReviewRating < 2  then 'Rating Bracket'n = "E";*/
	else if LastReviewRating < 3  then 'Rating Bracket'n = "D";
	else if LastReviewRating < 3.5 then 'Rating Bracket'n = "C";
	else if LastReviewRating < 4 then 'Rating Bracket'n = "B";
	else if LastReviewRating < 4.5 then 'Rating Bracket'n = "A";
	else if LastReviewRating < 5 then 'Rating Bracket'n = "A +";
	else 'Rating Bracket'n = "NA";
end;

/*if upcase(Gender1) = "F" then Gender = "Female";*/
/*if upcase(Gender1) = "M" then Gender = "Male";*/

 if DateofResignation ne . then Resigned = 1;
 if RevokedDate ne . then Revoked = 1;

FORMAT 
	JoiningMonth RelievingMonth Monname. 
	JoiningYear RelievingYear year4. 
	JoiningMonYY RelievingMonYY monyy7. ;
run;
