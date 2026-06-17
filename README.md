# calculateSynoptic
Collapse data from a cruise into one average (synoptic) cast for a cruise. This can be useful when you have multiple years of cruises, each cruise having multiple casts.\
Krista Longnecker, 11 June 2026

Multiple people have worked on this over the years, and this repository holds the script needed to distill a series of cruises (each with multiple casts) into one, averaged, 'cast' per cruise. For more details see the header for the script ```makeSynoptic_script_v1.R```. A sample R Markdown script (```sampleScript.Rmd```) is provided to show how this script can be used, it relies on a small dataset saved in the sampleData folder (```sampleData.csv```).

The script should work with data from any location as long as there is a variable that is 'Id', which consists of one number that is the following:
{5 digit cruise}{3 digit cast}{2 digit Niskin}. For example 1003500402 is cruise 10035, cast 004, Niskin 02.

If your data are in a different format, you will need to edit the scripts.
