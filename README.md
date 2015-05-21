# Univariate timeseries visualisation

`shiny-timeseries` is a visualisation tool for univariate timeseries data.
Wrapping up the R code doing all the heavy lifting in a user-friendly web app
brings mutual benefit:

* data analysts can inspect and visualise the data without R coding skills;
* some data, analysis and layout standards are enforced by being hard-coded.

## Work flow
From a pre-configured CKAN data catalogue, the app loads data about all 
compatible data sets.


Each compatible dataset has the following features:

* the tag `format_csv_ts` indicating compatibility with `shiny-timeseries`,
* at least one CSV file resource with data (in a certain standard format and structure),
* at least one PDF resource with a graph of the data,
* at least one TXT resource with the R code generating the figure from the data.

Furthermore, the CSV file contains:

* at least one date column with "date" in its name in format `YYYY-mm-dd[T%HH-%MM-%SS[tz]]`,
* at least one numeric column,
* optionally, character columns with factor levels.

The application loads the first CSV from the first dataset and displays summary
information about the data structure, as well as a preview of the actual data.

The user can search for another dataset (deleting the selected dataset name will
allow to filter datasets by typing), or select another CSV file of the same dataset.
The application will automatically reload the data and refresh the summary and 
preview.

In the second tab, "Plot Data", the application will draw a graph of the data, 
using selected columns as x-axis, y-axis, and optionally a grouping variable.
Underneath, the R code to reproduce the graph is shown.

For the x-axis, all valid date variables are offered; for the y-axis, all valid
numerical variables are offerend; for the grouping variable, all valid character
variables are offered.
For each variable, the first one occurring in the data is initially selected.

A range of data subsetting and plotting parameters is presented as input widgets;
any change to them is immediately reflected in both figure and graph.

Finally, the products can be downloaded to disk (graph as PDF, R code as TXT),
or, if a valid CKAN API key is entered, be uploaded to the respective dataset.
Here, the user can choose the respective PDF and TXT resource to be overwritten;
the first one of each is selected initially.

## Help and governance
A detailed help is given in the third tab, "Learn more". The help refers to the
internal Wiki at the Department of Parks and Wildlife for in-depth governance
on data formats and structure.

