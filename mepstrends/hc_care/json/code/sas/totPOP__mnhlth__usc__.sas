
ods graphics off;

/* Read in dataset and initialize year */
  FILENAME &FYC. "C:\MEPS\&FYC..ssp";
  proc xcopy in = &FYC. out = WORK IMPORT;
  run;

  data MEPS;
    SET &FYC.;
    year = &year.;
    ind = 1;
    count = 1;

    /* Create AGELAST variable */
    if AGE&yy.X >= 0 then AGELAST=AGE&yy.x;
    else if AGE42X >= 0 then AGELAST=AGE42X;
    else if AGE31X >= 0 then AGELAST=AGE31X;
  run;

  proc format;
    value ind 1 = "Total";
  run;

/* Perceived mental health */
  data MEPS; set MEPS;
    ARRAY OLDMNH(2) MNTHLTH1 MNTHLTH2;
    if year = 1996 then do;
      MNHLTH53 = MNTHLTH2;
      MNHLTH42 = MNTHLTH2;
      MNHLTH31 = MNTHLTH1;
    end;

    if MNHLTH53 ge 0 then mnhlth = MNHLTH53;
    else if MNHLTH42 ge 0 then mnhlth = MNHLTH42;
    else if MNHLTH31 ge 0 then mnhlth = MNHLTH31;
    else mnhlth = .;
  run;

  proc format;
    value mnhlth
    1 = "Excellent"
    2 = "Very good"
    3 = "Good"
    4 = "Fair"
    5 = "Poor"
    . = "Missing";
  run;

/* Usual source of care */
data MEPS; set MEPS;
  usc = LOCATN42;
  if HAVEUS42 = 2 then usc = 0;
  domain = (ACCELI42 = 1 & HAVEUS42 >= 0 & LOCATN42 >= -1);
run;

proc format;
  value usc
   0 = "No usual source of health care"
   1 = "Office-based"
   2 = "Hospital (not ER)"
   3 = "Emergency room"
  -1 = "Inapplicable"
  -8 = "Don't know";
run;

ods output CrossTabs = out;
proc surveyfreq data = MEPS missing;
  FORMAT usc usc. mnhlth mnhlth.;
  STRATA VARSTR;
  CLUSTER VARPSU;
  WEIGHT PERWT&yy.F;
  TABLES domain*mnhlth*usc / row;
run;

proc print data = out;
  where domain = 1 and usc ne . and mnhlth ne .;
  var usc mnhlth WgtFreq StdDev Frequency RowPercent RowStdErr;
run;
