
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shsannualreport

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis-CI Build
Status](https://api.travis-ci.org/thomascrines/shsannualreport.svg?branch=master)](https://travis-ci.org/thomascrines/shsannualreport)
<!-- badges: end -->

shsannualreport is an R package to prepare the results of the [Scottish
Household Survey(SHS)](https://www2.gov.scot/Topics/Statistics/16002)
for publication as an R Shiny app. The package allows users to prepare
source data for publication, and to then build an R Shiny app based on
the processed data.

## Installation

You can install the development version of shsannualreport from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("thomascrines/shsannualreport")
```

If you are working on SCOTS, or if the above does not work, you can
install manually:

1.  Go to the shsannualreport repository [master
    branch](https://github.com/thomascrines/shsannualreport) on GitHub
2.  Click **Clone or download** then **Download ZIP**
3.  Save the file locally and unzip
4.  Install with
install.packages():

<!-- end list -->

``` r
install.packages("C:/DownloadDirectory/shsannualreport-master/shsannualreport-master", repos = NULL,
                 type="source", lib = "C:/YourLibraryPath")
```

*Library path can be seen by running `.libPaths()`*

## Prerequisites

The package depends on having source data which is currently stored in a
secure folder on Objective Connect.

If this data has not yet been published, it won’t be available outside
the SHS team.

If the data has been published, please contact <SHS@gov.scot> to request
access to the source data.

To prepare source data prior to running the functions in the package:

  - Download the source dataset and unzip.
  - Download the design factors, question titles, and topic titles Excel
    workbooks.
  - Optionally download previous versions of a column names and/or
    variable names workbook. (This can be used to populate a new version
    later.)
  - Create a local directory with sub-directories named ‘dataset’ and
    ‘metadata’.
  - Move the extracted source dataset to the ‘dataset’ sub-directory,
    and the design factors, question titles, and topic titles workbooks
    to the ‘metatdata’ sub-directory.

## Usage

There are three public functions in the package:

  - `shs_create_names_workbooks` - Creates two Excel workbooks,
    `column_names.xlsx` and `variable_names.xlsx`, with names as they
    appear in the source data and values to rename them to as they will
    appear in the final app.
  - `shs_create_app_data` - Extracts all data and metadata from Excel
    sources, processes the data, and saves in Rds format suitable for
    use in the SHS Data Explorer app.
  - `shs_create_app` - Copies or creates all necessary files for the SHS
    Data Explorer app, including the data output by
    shs\_create\_app\_data. The resulting app can then be run locally or
    deployed.

### shs\_create\_names\_workbooks

`shs_create_names_workbooks` outputs two files, `column_names.xlsx` and
`variable_names.xlsx`, to the specified `destination_directory`. Both
sheets will have the same stucture, with two columns: `source_name` and
`display_name`. The `source_name` column lists all names as they appear
in the source data, the `display_name` column can be populated with the
desired names to appear in the app.

In `column_names.xlsx`, `source_name` will be populated with all column
names in all files in the `source_dataset_directory`, minus any names
specified in `columns_to_remove`.

In `variable_names.xlsx`, `source_name` will be populated with all
values in the second column on all files in the
`source_dataset_directory`. Here is important that the
`columns_to_remove` values are correct, as the source data can contain
unnecessary columns which can result in the output being populated with
values from these columns rather than the desired columns.

In both output files, the `display_name` value can be auto-populated
with previous values, if a path to an existing template file is provided
to the argument `existing_column_names_path` and/or
`existing_variable_names_path`.

In total, `shs_create_names_workbooks` accepts five arguments:

  - `destination_directory`: The path of the directory to write the
    column names and variables names Excel files to. (Must be an
    existing directory.)
  - `source_dataset_directory`: The path of the directory containing
    source dataset in Excel format. (This should be the sub-directory
    named `dataset` following the structure described above.)
  - `columns_to_remove`: A list of unnecessary columns to remove from
    the dataset.
  - `existing_column_names_path` (optional): A path to an existing Excel
    file with the same structure. Any ‘display\_name’ values will be
    copied to the new output file.
  - `existing_variable_names_path` (optional): A path to an existing
    Excel file with the same structure. Any ‘display\_name’ values will
    be copied to the new output file.

Once the output files are created, the `display_name` columns of both
output files can be manually edited to the desired values.

*NOTE: some special characters can run locally, but break when deployed
due to encoding differences, e.g `£`. There is currently no fix in the
package for these characters, so they will need to be changed to
something else when deploying to shinyapps.io.*

### shs\_create\_app\_data

`shs_create_app_data` takes a source dataset and metadata in Excel
format, processes the data to make it suitable for the SHS Data Explorer
app, and saves the output files in Rds format.

The processing done in `shs_create_app_data` includes:

  - extracting all data and metadata contained in the
    `source_data_directory`
  - removing columns specified in `columns_to_remove` from the dataset
  - renaming column names and variable names in the dataset to those
    specified in the `column_names_workbook_path` and
    `variable_names_workbook_path` respectively
  - removing local authority data from tables where sample size is too
    small

There are also a number of other processing functions which are
described in more detail in the vignette (to do), including:

  - collecting tables split over multiple sheets by year into combined
    tables
  - calculating confidence intervals for all tables
  - ordering of tables

In total `shs_create_app_data` accepts five arguments:

  - `destination_directory`: The path of the directory to write the
    output dataset to. (Must be a pre-existing directory.)
  - `source_data_directory`: The path of the directory containing source
    data in Excel format. (This should be the top level directory which
    contains sub-directories `dataset` and `metadata` following the
    structure described above.)
  - `columns_to_remove`: A list of unnecessary columns to remove from
    the dataset.
  - `column_names_workbook_path`: A path to an Excel workbook with
    details of the source names and display names of columns in the
    source dataset. If you don’t have a suitable file,
    `shs_create_names_workbook` can be used to generate this file.
  - `variable_names_workbook_path`: A path to an Excel workbook with
    details of the source names and display names of variables in the
    source dataset. If you don’t have a suitable file,
    `shs_create_names_workbook` can be used to generate this file.

When the data has been output to the `destination_directory` it can then
be used to create a version of the SHS Data Explorer app using
`shs_create_app`.

### shs\_create\_app

`shs_create_app` will take data output by `shs_create_app_data` and
generate a Shiny app based on the contained data. The app will be
written to the specified `destination_directory`, from where it can be
run locally, deployed etc.

In total `shs_create_app_data` accepts five arguments:

  - `destination_directory`: The path of the directory to create the
    final app in.
  - `data_directory`: The path of the directory containing all data and
    metadata created by `shs_create_app_data`.
  - `reports_start_year`: The first year to allow users to generate
    reports for in the ‘Generate Report’ section of the app.
  - `reports_end_year`: The final year to allow users to generate
    reports for in the ‘Generate Report’ section of the app.

## Example

The following example is based on usage in SCOTS, with the source data
downloaded and set up according to the instructions in ‘prerequisites’
in the ‘Downloads’ folder.

To use the examples on other computers, all that needs changed are the
folder paths, which will vary by user.

As the `destination_directory` paths for both
`shs_create_names_workbooks` and `shs_create_app_data` must exist before
running, directories for the example I created directories ‘names’ and
‘appdata’ in ‘Downloads’.

*NOTE: Running functions with `shsannualreport::` prefix alone seems not
to load dependencies, running `library(shsannualreport)` before running
any functions will prevent errors.*

### shs\_create\_names\_workbooks

``` r
library(shsannualreport)

destination_directory <- "C:/Users/[user's U number]/Downloads/names"
source_dataset_directory <- "C:/Users/[user's U number]/Downloads/data/dataset"
columns_to_remove <- c("sort", "_LABEL_", "var", "LABEL")
existing_column_names_path <- "C:/Users/[user's U number]/Downloads/old_column_names.xlsx"
existing_variable_names_path <- "C:/Users/[user's U number]/Downloads/old_variable_names.xlsx"


shs_create_names_workbooks(destination_directory = destination_directory,
                           source_dataset_directory = source_dataset_directory,
                           columns_to_remove = columns_to_remove,
                           existing_column_names_path = existing_column_names_path,
                           existing_variable_names_path = existing_variable_names_path)
```

### shs\_create\_app\_data

*NOTE: Although `shs_create_names_workbooks` can be used to generate
column and variable names re-naming spreadsheets, it doesn’t have to be
run if you already have these sheets. The outputs can be saved off and
the paths passed to `column_names_workbook_path` and
`variable_names_workbook_path`. This example uses the sheets output in
the `shs_create_names_workbooks` example above.*

``` r
library(shsannualreport)

destination_directory <- "C:/Users/[user's U number]/Downloads/appdata"
source_data_directory <- "C:/Users/[user's U number]/Downloads/data"
columns_to_remove <- c("sort", "_LABEL_", "var", "LABEL")
column_names_workbook_path <- "C:/Users/[user's U number]/Downloads/names/column_names.xlsx"
variable_names_workbook_path <- "C:/Users/[user's U number]/Downloads/names/variable_names.xlsx"

shs_create_app_data(destination_directory = destination_directory,
                    source_data_directory = source_data_directory,
                    columns_to_remove = columns_to_remove,
                    column_names_workbook_path = column_names_workbook_path,
                    variable_names_workbook_path = variable_names_workbook_path)
```

### shs\_create\_app

*NOTE: As above, if data has been previously generated by
`shs_create_app_data` and the source data hasn’t changed,
`shs_create_app` can be run without re-running `shs_create_app_data`.*

``` r
library(shsannualreport)

destination_directory <- "C:/Users/[user's U number]/Downloads"
data_directory <- "C:/Users/[user's U number]/Downloads/appdata/data"
reports_start_year <- 2013
reports_end_year <- 2019

shs_create_app(destination_directory = destination_directory,
               data_directory = data_directory,
               reports_start_year = reports_start_year,
               reports_end_year = reports_end_year)
```
