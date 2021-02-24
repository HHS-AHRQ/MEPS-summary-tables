
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

/* Census Region */
  data MEPS; set MEPS;
    ARRAY OLDREG(2) REGION1 REGION2;
    if year = 1996 then do;
      REGION42 = REGION2;
      REGION31 = REGION1;
    end;

    if REGION&yy. >= 0 then region = REGION&yy.;
    else if REGION42 >= 0 then region = REGION42;
    else if REGION31 >= 0 then region = REGION31;
    else region = .;
  run;

  proc format;
    value region
    1 = "Northeast"
    2 = "Midwest"
    3 = "South"
    4 = "West"
    . = "Missing";
  run;

/* Adults advised to quit smoking */
data MEPS; set MEPS;
  ARRAY SMKVAR(2) ADDSMK42 ADNSMK42;
  if year <= 2002 then adult_nosmok = ADDSMK42;
  else adult_nosmok = ADNSMK42;

  domain = (ADSMOK42=1);
  if domain = 0 and SAQWT&yy.F = 0 then SAQWT&yy.F = 1;
run;

proc format;
  value adult_nosmok
   1 = "Told to quit"
   2 = "Not told to quit"
   3 = "Had no visits in the last 12 months"
  -9 = "Not ascertained"
  -1 = "Inapplicable";
run;

ods output CrossTabs = out;
proc surveyfreq data = MEPS missing;
  FORMAT adult_nosmok adult_nosmok. region region.;
  STRATA VARSTR;
  CLUSTER VARPSU;
  WEIGHT SAQWT&yy.F;
  TABLES domain*region*adult_nosmok / row;
run;

proc print data = out;
  where domain = 1 and adult_nosmok ne . and region ne .;
  var adult_nosmok region WgtFreq StdDev Frequency RowPercent RowStdErr;
run;
