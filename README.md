# MEPS summary tables

This repository contains the code needed to create the tables behind the interactive [**Medical Expenditure Panel Survey (MEPS) Household Component summary tables**](https://meps.ahrq.gov/mepstrends/home/index.html) found on the [MEPS website](https://meps.ahrq.gov/mepsweb/data_stats/quick_tables.jsp). The tables created from the code in this repository provide frequently used summary statistics at the national level for:
* Health care use, expenditures, and population characteristics
* Health insurance coverage
* Accessibility of care, quality of care, and diabetes care
* Medical conditions, 1996-2015 (based on ICD-9 codes)
* Medical conditions, 2016 and later (based on ICD-10 codes)
* Prescribed drug purchases


The [R Programming language](https://www.r-project.org/) (version 4.0.3) was used as the statistical software to create survey statistics from raw MEPS data files. 


## Updating Household Component tables

Each year, after the MEPS full-year-consolidated (FYC) file is released, the following steps can be used to add data from the new year to the existing tables:

1. If needed, update the R code snippets in the `code` folder. For example, the education variable changed in 2016, so the `education` variable was edited in the `add_subgrps.R` file.

2. If needed, update the spreadsheets in the [dictionaries](dictionaries) folder with new labels.

3. Follow the instructions in [UPDATE.R](UPDATE.R) to create tables for specified years. Run time takes approximately 3 hours to complete. Note that you may need to create local folders to store interim files from this process. These files are not 

## File Structure
This repository contains the following components:
* [code](code): R code snippets for: 
	* loading MEPS data (`load_*.R`)
	* adding subgroups (`add_*.R`)  
	* defining the survey design objects (`dsgn_*.R`). 
	* additional codes for the Use and Expenditures tables that are separated for convenience (`use*.R`)
* [dictionaries](dictionaries): Excel files containing labels, categories, and captions for certain variables
* [formatted_tables](formatted_tables): The final version of the formatted data tables that are used in the MEPS-HC summary tables web page
* [qc](qc): Code and files used to QC new tables, or edits to existing code

* [functions.R](functions.R): Helper functions 
* [functions_format.R](functions_format.R): Main function for formatting tables
* run_*.R: R code for creating summary data tables for each table series
	* Access to care: [run_care_access.R](run_care_access.R)
	* Diabetes care:  [run_care_diab.R](run_care_diab.R)
	* Quality of care:  [run_care_qual.R](run_care_qual.R)
	* Medical conditions:  [run_cond.R](run_cond.R)
	* Health insurance:  [run_ins.R](run_ins.R)
	* Prescribed medicines:  [run_pmed.R](run_pmed.R)
	* Health care use, expenditures, and population characteristics:  [run_use.R](run_use.R)

* [UPDATE.R](UPDATE.R): Main R code used to create, format, and QC tables


## Archived version
This version of the MEPS summary tables repository was focuses on creation and formatting of the summary data tables that underly the MEPS-HC sumary tables tool. A previous version of this repository also included HTML and JavaScript to display the tables on a web server. The previous version has been archived, as it is no longer in use, but can still be accessed at [https://github.com/HHS-AHRQ/MEPS-summary-tables-archive](https://github.com/HHS-AHRQ/MEPS-summary-tables-archive).




## Survey background
The **Medical Expenditure Panel Survey (MEPS)**, which began in 1996, is a set of large-scale surveys of families and individuals, their medical providers (doctors, hospitals, pharmacies, etc.), and employers across the United States. The MEPS Household Component (MEPS-HC) survey collects information from families and individuals pertaining to medical expenditures, conditions, and events; demographics (e.g., age, ethnicity, and income); health insurance coverage; access to care; health status; and jobs held. The MEPS-HC is designed to produce national and regional estimates of the health care use, expenditures, sources of payment, and insurance coverage of the U.S. civilian noninstitutionalized population. The sample design of the survey includes weighting, stratification, clustering, multiple stages of selection, and disproportionate sampling.

## Public Domain Disclaimer

The **MEPS summary tables** application is a U.S. Government work developed by the Agency for Healthcare Research and Quality (AHRQ).  This application is in the public domain and may be used, reproduced, modified, built upon and distributed in the United States without further permission from AHRQ.  Reproduction and distribution for a fee is prohibited.  It is requested that in any subsequent use AHRQ be given appropriate acknowledgment.  The use of the HHS or AHRQ seal or logo without prior written authorization is expressly prohibited by law.  Although these data have been processed successfully on a computer system at AHRQ, no warranty expressed or implied is made regarding the accuracy or utility of the data on any other system or for general or scientific purposes, nor shall the act of distribution constitute any such warranty.  AHRQ has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by AHRQ.  AHRQ reserves the right to assert copyright protection internationally.
