# What is the Time Series Explorer's intent and purpose?
This application creates standard time series graphs from compatible data sets
on our [data catalogue](http://internal-data.dpaw.wa.gov.au/).

The data and figures are used in Marine Science's 
[annual MPA reporting](http://internal-data.dpaw.wa.gov.au/dataset/mpa-reports).
Using this application has multiple benefits:

* it saves the authors some leg work,
* it provides graphs in the format and quality required for MPA reporting,
* it drives the clean-up and standardisation of data in the catalogue, and
* it makes reporting transparent and reproducible by saving the work flow as R code.

# How can I use it?

* Choose a dataset, choose the CSV resource, and inspect / preview the data. You 
can delete the selected dataset and type a region or indicator to filter the list.
* Pick a numeric variable for the y axis, and a date variable for the x axis. The 
first date and numeric variables are pre-selected by default.
* View the graph in the "plot" tab, modify graphical parameters in the side panel 
as required.
* Download the graph as PDF, plus the R code to reproduce the graph as text.
* Coming soon: paste your CKAN API key to upload the PDF graph and R code back to the data catalogue.

Logged in users can find their API key on their profile page (click on your user name).
If your data doesn't conform to the required standards, the graph may not work.

# How can I use the Time Series Explorer on my own data?

This application will only work with univariate (and optionally grouped) time series data.
Your data need to be on the [internal CKAN data catalogue](http://internal-data.dpaw.wa.gov.au/), 
conforming to a particular standard:

* The CKAN dataset must carry the tag `format_csv_ts` in order to show up in `Choose dataset`.
* The dataset title should include the area ("SBMPA" or "Shark Bay Marine Protected Areas" 
preferred: "Shark Bay MPA") and the asset name (what the data is about, e.g. "Seagrass cover", 
"Finfish abundance", "Boat registrations"). Type into the select box to filter!
* The data must be attached to the dataset as CSV resource in the format described below.
* A figure must be attached to the dataset as PDF resource.
* A text file must be attached to the dataset as TXT resource.
* Data (CSV), figure (PDF) and R script (TXT) should have the file names 
"dataDSTITLE.csv", "figureDSTITLE.pdf", or "scriptDSTITLE.txt", respectively,
and the resource name "Data/Figure/R Script DSTITLE", where *DSTITLE* should be the dataset title.
* Data, figure and code should be the first three resources on the dataset (manage > resources > re-order) so
they will get picked up correctly by default.
* You must be authorised to update the data catalogue if you want to upload figure and script directly.

# What's the required format and structure for the CSV file?
Generally, the [Marine Science standards for observational datasets](
https://confluence.dpaw.wa.gov.au/display/MSIM/Quality+requirements+and+format+standards+for+observational+datasets) apply - especially for file and 
variable names.

For univariate time series, the CSV **must** have:

* one column "datetime" or "Datetime" with a string of an ISO8601 date and time with or without timestamp 
(`2014-12-31T23:56:59+08:00` or `2014-12-31T23:56:59` assuming GMT+08), or 
* one column "date" or "Date" with a quoted string of an ISO8601 date (preferred `2014-12-31` over `31-12-2014`),
* one column with the univariate dependent variable labelled alphanumerically (preferred "value")

Example:
```
date, value
"2014-01-01", 12.234
"2014-01-02", 13.2342
"2014-01-03", 12.63
```

For grouped univariate time series, an additional column for the grouping factor must be given.
The grouping variable name will not be used in the figure and can be simple.
Example:

```
date, value, group
"2014-01-01", 14.1231, "Site A"
"2014-01-01", 12.234, "Site B"
"2014-01-02", 12.234, "Site A"
"2014-01-02", 18.2223, "Site B"
"2014-01-03", 12.62343, "Site A"
"2014-01-03", 13.6323, "Site B"
```

Additionally, desired columns are:

* "latitude", decimal degrees of unprojected WGS84 latitude
* "longitude", decimal degrees of unprojected WGS84 longitude
* "altitude", meters of altitude over mean sea level (negative values for depth)

Any other columns will be ignored and can be included in the CSV as necessary.
