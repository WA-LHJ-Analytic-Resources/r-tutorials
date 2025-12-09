# Exploratory_Data_Analysis
Allie Warren, allison.warren
2025-11-13

<link href="exploratory_data_analysis_files/libs/htmltools-fill-0.5.8.1/fill.css" rel="stylesheet" />
<script src="exploratory_data_analysis_files/libs/htmlwidgets-1.6.4/htmlwidgets.js"></script>
<link href="exploratory_data_analysis_files/libs/datatables-css-0.0.0/datatables-crosstalk.css" rel="stylesheet" />
<script src="exploratory_data_analysis_files/libs/datatables-binding-0.34.0/datatables.js"></script>
<script src="exploratory_data_analysis_files/libs/jquery-3.6.0/jquery-3.6.0.min.js"></script>
<link href="exploratory_data_analysis_files/libs/dt-core-1.13.6/css/jquery.dataTables.min.css" rel="stylesheet" />
<link href="exploratory_data_analysis_files/libs/dt-core-1.13.6/css/jquery.dataTables.extra.css" rel="stylesheet" />
<script src="exploratory_data_analysis_files/libs/dt-core-1.13.6/js/jquery.dataTables.min.js"></script>
<link href="exploratory_data_analysis_files/libs/crosstalk-1.2.2/css/crosstalk.min.css" rel="stylesheet" />
<script src="exploratory_data_analysis_files/libs/crosstalk-1.2.2/js/crosstalk.min.js"></script>

- [Exploratory Data Analysis](#exploratory-data-analysis)
- [Setup](#setup)
- [Reading in the Data](#reading-in-the-data)
- [Data Basics](#data-basics)
- [Identifying Errors in the Data](#identifying-errors-in-the-data)
  - [Missing Data](#missing-data)
- [Cleaning Data](#cleaning-data)
  - [Cleaning Strings](#cleaning-strings)
  - [Replacing Missing Values and Converting to
    Numeric](#replacing-missing-values-and-converting-to-numeric)
  - [Cleaning Address Data](#cleaning-address-data)
  - [Extracting Phone Number](#extracting-phone-number)
- [Joining data](#joining-data)
- [Viewing the Cleaned Data](#viewing-the-cleaned-data)
- [Calculating summary stats](#calculating-summary-stats)
- [Visualizing](#visualizing)
  - [Bar Plots](#bar-plots)
  - [Map Plot](#map-plot)

## Exploratory Data Analysis

This notebook shows an example of starting exploring a data set. The
notebook uses the [tidyverse](https://tidyverse.org/packages) functions
and conventions, which is a collection of packages for data science and
visualization that have a common design. This also, for the most part,
uses modern tidyverse conventions, using R version 4.3+ features. For
example, it uses the pipe `|>` instead of `%>%` to chain together
functions, this guide is a good resource for [modern R
development](https://gist.github.com/sj-io/3828d64d0969f2a0f05297e59e6c15ad).
This following notebook includes:

- observing key characteristics of data

- cleaning data, including identifying missing data

- combining data

- grouping data and calculating summary statistics

- visualizing data

For the purpose of this notebook I have created a synthetic data set
that can be used to illustrate some of the key challenges that may exist
in a data set.

## Setup

``` r
# Loading packages

# the pacman package is useful for managing package install/loading, When using pacman to load a package it will automatically install the package if it is not already installed
if(!require("pacman")) install.packages("pacman") #but first we have to install pacman
```

    Loading required package: pacman

``` r
# This load packages for reading in data, transforming data, reshaping data, summarizing data, identifying missing data, working with dates and string, visualizing data (greating plots, map plots, color schemes), additional text cleaning, and viewing tables
pacman::p_load(readr, dplyr, tidyr, purrr, skimr, naniar, lubridate, stringr, ggplot2, usmap, sf, viridis, textclean, DT)
```

## Reading in the Data

``` r
# Reading in the data
base_file_path <- "../data"

wa_health_data <- read_csv(file.path(base_file_path, "synthetic_wa_records.csv"))
```

    Rows: 500 Columns: 12
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (10): first_name, last_name, phone_number, street, county, email, ID, d...
    dbl   (1): val1
    date  (1): dob

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# loads in an R list containing different forms of street suffixes (e.g. Street, St, Square, Sq, Route, Rte), used for address cleaning
source(file.path(base_file_path, 'street_suffixes.R'))
# view the data
head(wa_health_data)
```

    # A tibble: 6 × 12
      first_name last_name  phone_number  street county email ID    dob        date 
      <chr>      <chr>      <chr>         <chr>  <chr>  <chr> <chr> <date>     <chr>
    1 Hoover     Swift      1-820-530-63… 642 7… Pierc… oo'c… 238-… 1960-06-05 2024…
    2 Jerimy     Hammes     120-107-6198  8085 … Clall… kraj… 163-… 1940-10-31 09/1…
    3 Emilee     Ferry      1-037-411-36… 3776 … Frank… dess… 781-… 1970-08-10 2025…
    4 Brittany   Cremin     133.055.7951  310  … Frank… ksch… 889-… 2010-09-03 2025…
    5 John       Kulas      (964)067-097… 784 W… Pierc… trom… 767-… 1937-08-01 04/1…
    6 Unknown    Jakubowski 1-285-564-32… 024 K… Clark… beni… 331-… 2017-04-10 2025…
    # ℹ 3 more variables: val1 <dbl>, val2 <chr>, val1_CI <chr>

## Data Basics

- data type for each column

- size of the data

- functions for summarizing data, including counting missing data,
  unique counts per column, summary stats

``` r
# can get basic characteristics of the data (data types, columns) and size of the data using base R functions
str(wa_health_data)
```

    spc_tbl_ [500 × 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
     $ first_name  : chr [1:500] "Hoover" "Jerimy" "Emilee" "Brittany" ...
     $ last_name   : chr [1:500] "Swift" "Hammes" "Ferry" "Cremin" ...
     $ phone_number: chr [1:500] "1-820-530-6339" "120-107-6198" "1-037-411-3655x17041" "133.055.7951" ...
     $ street      : chr [1:500] "642 72th Avenue W" "8085 Noemie Way" "3776 5th St. NE" "310  Walsh Parks Drive" ...
     $ county      : chr [1:500] "Pierce County" "Clallam County" "Franklin County" "Franklin County" ...
     $ email       : chr [1:500] "oo'connell@gmail.com" "krajcik.glynn@yahoo.com" "dessa19@llc.org" "kschamberger@gleichner.com" ...
     $ ID          : chr [1:500] "238-81-8708" "163-43-7463" "781-67-7952" "889-26-9781" ...
     $ dob         : Date[1:500], format: "1960-06-05" "1940-10-31" ...
     $ date        : chr [1:500] "2024-02-24" "09/14/24" "2025/01/10" "2025-08-20" ...
     $ val1        : num [1:500] 1 0.34 0.964 0.924 0.825 0.703 0.349 0.06 0.454 0.237 ...
     $ val2        : chr [1:500] "1" "0.018" "0.914" "0.677" ...
     $ val1_CI     : chr [1:500] "0.99-1" "0.34-0.35" "0.88-0.99" "0.9-0.93" ...
     - attr(*, "spec")=
      .. cols(
      ..   first_name = col_character(),
      ..   last_name = col_character(),
      ..   phone_number = col_character(),
      ..   street = col_character(),
      ..   county = col_character(),
      ..   email = col_character(),
      ..   ID = col_character(),
      ..   dob = col_date(format = ""),
      ..   date = col_character(),
      ..   val1 = col_double(),
      ..   val2 = col_character(),
      ..   val1_CI = col_character()
      .. )
     - attr(*, "problems")=<externalptr> 

``` r
dim(wa_health_data)
```

    [1] 500  12

``` r
# for a more complete picture of the data there are useful packages that have summary functions, for example skim() from the skimr package
skim(wa_health_data)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | wa_health_data |
| Number of rows                                   | 500            |
| Number of columns                                | 12             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                |
| Column type frequency:                           |                |
| character                                        | 10             |
| Date                                             | 1              |
| numeric                                          | 1              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                |
| Group variables                                  | None           |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| first_name    |         0 |          1.00 |   2 |  11 |     0 |      479 |          0 |
| last_name     |         0 |          1.00 |   3 |  13 |     0 |      317 |          0 |
| phone_number  |         0 |          1.00 |  11 |  20 |     0 |      500 |          0 |
| street        |         2 |          1.00 |   1 |  56 |     0 |      493 |          0 |
| county        |         0 |          1.00 |  10 |  19 |     0 |       44 |          0 |
| email         |         0 |          1.00 |  12 |  32 |     0 |      500 |          0 |
| ID            |         0 |          1.00 |  11 |  11 |     0 |      500 |          0 |
| date          |         0 |          1.00 |   8 |  10 |     0 |      449 |          0 |
| val2          |         7 |          0.99 |   1 |  14 |     0 |      323 |          0 |
| val1_CI       |         0 |          1.00 |   3 |   9 |     0 |      347 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| dob | 0 | 1 | 1930-01-18 | 2024-05-21 | 1977-09-14 | 493 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean |   sd |  p0 |  p25 |  p50 |  p75 | p100 | hist  |
|:--------------|----------:|--------------:|-----:|-----:|----:|-----:|-----:|-----:|-----:|:------|
| val1          |         0 |             1 | 0.54 | 0.33 |   0 | 0.25 | 0.52 | 0.85 |    1 | ▅▆▅▃▇ |

## Identifying Errors in the Data

``` r
# there are 35 unique values in the County column - suggests a possible error - some additional/missing white space in some entries
n_distinct(wa_health_data$county)
```

    [1] 44

``` r
# try converting to a single date format - observe that many don't parse
length(which(is.na(mdy(wa_health_data$date))))
```

    Warning: 244 failed to parse.

    [1] 244

### Missing Data

``` r
# val2 is character - check for alternate missing data types
# skim shows counts of NA, but won't catch other forms of missing data
check_na <- c("^\\.$", "NA", "-99", "N/A", "Not Applicable", "na", "n/a", "Null", "null")
naniar::miss_scan_count(wa_health_data, check_na)
```

    # A tibble: 12 × 2
       Variable         n
       <chr>        <int>
     1 first_name      38
     2 last_name        7
     3 phone_number     6
     4 street          14
     5 county           0
     6 email           33
     7 ID               7
     8 dob              0
     9 date             0
    10 val1             0
    11 val2            11
    12 val1_CI          0

## Cleaning Data

- filter to relevant subset

- clean up string columns

- clean up date columns

``` r
# formatting dates
wa_health_data_cleaned <- wa_health_data |> 
  mutate(date_format = lubridate::ymd(lubridate::parse_date_time(date, orders = c("%m/%d/%Y", "%Y-%m-%d", "%Y/%m/%d", "%m-%d-%Y", "%m/%d/%y"))))
```

### Cleaning Strings

``` r
# cleaning up County names
wa_health_data_cleaned <- wa_health_data_cleaned |> 
  # remove County from the name and remove extra white space
  mutate(county_name = str_replace_all(county, "County", "")|> str_squish())

n_distinct(wa_health_data_cleaned$county_name)
```

    [1] 39

### Replacing Missing Values and Converting to Numeric

``` r
# replacing alternative missing values with NA
replace_na <- c(".", "NA", "-99", "N/A", "Not Applicable", "na", "n/a", "Null", "null")
wa_health_data_cleaned <- replace_with_na(wa_health_data_cleaned, replace = list(val2 = replace_na))

# and convert data to numeric 
wa_health_data_cleaned <- wa_health_data_cleaned |>
  mutate(val2 = as.numeric(val2))

# split the CI into 2 columns - split the column on '-' then convert new columns to numeric
wa_health_data_cleaned <- wa_health_data_cleaned |>
  separate_wider_delim(val1_CI, delim = "-", names = c('val1_lower_CI',
                                                           'val1_upper_CI')) |>
  mutate(across(ends_with("CI"), ~as.numeric(.)))
```

### Cleaning Address Data

``` r
# identifies if the input string only contains letters
letters_only <- function(x) {
  x |>  
  str_remove_all("[[:blank:]]") |> 
  str_remove_all("[[:punct:]]") |>
  str_detect("^[A-Za-z]+$")
}

# identifies if the input string contain U.S. street suffixes like avenue road, court, and different ways they are shortened.
containsstreetsuffix <- function(x) {
  x <- x |>
  str_remove_all("[[:blank:]]") |> 
  str_remove_all("[[:punct:]]")
  
  any(str_detect(str_to_lower(x), str_to_lower(street_suffix_list)))
}

# identify if the string contains fields that are not useful for geocoding and contain notes about living circumstance or unavailability of an address but which are not an address
containsstreetsuffixremove <- function(x) {
  x <- x |>
  str_remove_all("[[:blank:]]") |> 
  str_remove_all("[[:punct:]]") 
  
  any(str_detect(str_to_lower(x), c("student", "address", "homeless", "unkn", "null", "update"))) 
}

# identifies if the string is only numbers after removing blank spaces and punctuation - i.e. fields that are numbers only.
numbers_only <- function(x) {
  x |> 
  str_remove_all("[[:blank:]]") |> 
  str_remove_all("[[:punct:]]") |> 
  str_detect("^[0-9]+$")
}
 
# fields that are empty after removing blank spaces and punctuation - i.e. field that are punctuation only
punct_only<- function(x) x |> 
  str_remove_all("[[:blank:]]") |> 
  str_remove_all("[[:punct:]]") |> 
  str_detect("^$")


# Function to clean addresses, removing special characters and replacing common
# error inputs with NA
clean_address <- function(address) {
  # beginning cleaning the input string, removing special characters
  cleaned_address <- address |>
    as.character() |> 
    str_replace_all(pattern = "[^\x20-\x7E]", " ") |> 
    textclean::replace_non_ascii() |> 
    textclean::strip(char.keep = c("#", "-"), digit.remove = F, apostrophe.remove = T, lower.case = F)
  
  # if a PO box address replace with NA
  cleaned_address <- case_when(str_detect(str_to_lower(cleaned_address), "^(po|p\\.o\\.|p box|p\\. o) ")  ~ NA_character_,
                               TRUE ~cleaned_address)
  
  # convert to title case
  cleaned_address <- str_to_title(cleaned_address)
  
  # if less than 7 characters (often sign of missing data) replace with NA
  cleaned_address <- case_when(nchar(cleaned_address)<=7  ~ NA_character_,   TRUE ~cleaned_address)
  
  # replace with NA if the address doesn't look like an address because it is all numbers, all letters, missing a street suffix and matches a pattern of other strings
  cleaned_address <- case_when(
    letters_only(cleaned_address)==T ~ NA_character_,
    numbers_only(cleaned_address)==T ~ NA_character_,
    containsstreetsuffix(cleaned_address)==F &
    containsstreetsuffixremove(cleaned_address)==T  ~ NA_character_,
    TRUE ~ cleaned_address)
  
  cleaned_address
  
}

# for each address I will apply the clean_address function using map_chr(), map_chr() always produces a character result (is therefore prefered to sapply which might return a list or vector)
wa_health_data_cleaned$address_clean <- map_chr(wa_health_data_cleaned$street, clean_address) 
```

### Extracting Phone Number

Want to extract at 10 digit phone number in the format xxx-xxx-xxxx from
a string that might include phone extensions, various punctuation,
country codes.

``` r
clean_phone_numbers <- function(phone_number) {
  # convert to character
  phone_number <- as.character(phone_number) |> 
      # first remove extension numbers - x followed by numbers at the end of the string
    str_remove("x[0-9]+$") |> 
    # remove blank space
    str_remove_all("[[:blank:]]") |> 
    # remove punctuation 
    str_remove_all("[[:punct:]]") |> 
    # remove and characters, such as + or any letters
    str_remove_all("\\+") |> 
    str_remove_all("[a-z]+")
  
  # handling country codes - if still longer than 10 digits, only take the last 10
  # concatenating the last 10 characters of the string, using the format xxx-xxx-xxxx
  plen <- nchar(phone_number)
  phone_number_format <- str_c(str_sub(phone_number, plen-9, plen-7), "-", str_sub(phone_number, plen-6, plen-4), "-", str_sub(phone_number, plen-3, plen))
  phone_number_format
}

wa_health_data_cleaned$phone_clean <- clean_phone_numbers(wa_health_data_cleaned$phone_number)
```

## Joining data

Combining multiple data sets - this uses modern tidyverse conventions
for joining, such as using `join_by(a == b`) instead of
`by = c('a' = 'b')`

``` r
# table of ACH regions and their associated counties
ach_mapping <-
  tibble(ACH = c('Olympic Community of Health', 'North Sound', 'Healthier Here', 'Elevate Health', 'Cascade Pacific Action Alliance', 'Southwest Washington', 'Greater Health Now', 'Thriving Together NCW', 'Better Health Together'),
             counties = list(c('Clallam', 'Jefferson', 'Kitsap'), 
                          c('Whatcom', 'Skagit', 'Snohomish', 'San Juan', 'Island'),
                          c('King'),
                          c('Pierce'),
                          c('Grays Harbor', 'Mason', 'Thurston', 'Pacific', 'Lewis', 'Wahkiakum', 'Cowlitz'),
                          c('Clark', 'Skamania', 'Klickitat'),
                          c('Kittitas', 'Yakima', 'Benton', 'Franklin', 'Walla Walla', 'Columbia', 'Garfield', 'Asotin', 'Whitman'),
                          c('Okanogan', 'Chelan', 'Douglas', 'Grant'),
                          c('Adams', 'Lincoln', 'Ferry', 'Stevens', 'Pend Oreille', 'Spokane')))
head(ach_mapping)
```

    # A tibble: 6 × 2
      ACH                             counties 
      <chr>                           <list>   
    1 Olympic Community of Health     <chr [3]>
    2 North Sound                     <chr [5]>
    3 Healthier Here                  <chr [1]>
    4 Elevate Health                  <chr [1]>
    5 Cascade Pacific Action Alliance <chr [7]>
    6 Southwest Washington            <chr [3]>

``` r
# expand the table so that there is one row per county, which will make it easier to join to the other data
ach_mapping_long <- ach_mapping |>
  unnest(counties)

# join the data, keeping all the rows from our data and any counties from the ACH mapping that were found in the data. The data is joined on the column `county_name` from the records data and the column `counties` from the ach mapping table
wa_health_data_cleaned <- left_join(wa_health_data_cleaned, ach_mapping_long, by = join_by(county_name == counties))
```

## Viewing the Cleaned Data

``` r
# View the first 50 rows of the data, 5 rows per page
# with a horizontal scroll bar to see the different columns
DT::datatable(head(wa_health_data_cleaned, n =50), 
              options = list(
                scrollX = TRUE,       # Enable horizontal scroll
                pageLength = 5,       # Show 5 rows per page
                autoWidth = TRUE      # Adjust column widths automatically
  ),
  class = 'cell-border stripe')
```

<div class="datatables html-widget html-fill-item" id="htmlwidget-2bcb0fac7fd86b7d65ca" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-2bcb0fac7fd86b7d65ca">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50"],["Hoover","Jerimy","Emilee","Brittany","John","Unknown","Turner","Tomika","Willaim","Corene","Evelin","Ruben","Camden","Brion","Eve","Haden","Stephanie","Debera","Marquita","Guido","Furman","Hildred","Tyesha","Dejon","Pandora","Ednah","Benjamine","Romello","Trisha","Maximiliano","Julia","Aggie","Effie","Caren","Lesia","Leana","Stefanie","Annalise","Abbigail","Jamil","Alwina","Fernand","Tatyanna","Trae","Debby","Myles","Hymen","Danniel","Contina","Idella"],["Swift","Hammes","Ferry","Cremin","Kulas","Jakubowski","Rodriguez","Marks","Reilly","Lowe","Murazik","Prohaska","Kerluke","Schinner","Hand","Reinger","Torphy","McGlynn","Bashirian","Gerhold","Zieme","DuBuque","Gutkowski","Schinner","Jacobs","Klocko","Feest","Mertz","Schuster","Stark","Muller","Kshlerin","Ward","Towne","Cummings","Leuschke","Effertz","Mills","Gaylord","Dare","Crooks","Schinner","Simonis","McClure","Rogahn","Jacobs","Ziemann","Daugherty","Barrows","Kuhlman"],["1-820-530-6339","120-107-6198","1-037-411-3655x17041","133.055.7951","(964)067-0977x7122","1-285-564-3248x12022","381.581.1622x5862","1-967-598-3721","1-986-101-1561","(794)509-1258","+24(1)2042152467","660-906-7418","443-986-9818","1-297-937-9111x74731","03029639404","098-485-0406x7693","123-509-1981x94200","(576)650-7406x7846","947-002-9503","1-735-803-6969x579","628.634.3576x37021","1-536-801-4341x25491","1-174-391-5396x43230","1-345-199-3341x788","(830)047-1365x7250","(132)613-6161x54171","(628)474-6359x0484","768.965.0410x0338","365.037.7272","255.283.5249","885-440-0869x6978","(205)934-1635","625-074-2638x40610","021.301.1623x781","1-586-682-2892","(563)166-8152x57758","381-929-4684x0875","1-216-894-6343","1-882-860-9256x375","815.600.9715","1-861-122-3193x19822","1-474-819-6144","1-406-169-5989x811","(994)858-1629x34847","1-588-404-7093x013","1-184-895-3288x548","(998)792-3930","+50(2)3784470932","+66(5)0326496786","(094)226-7923"],["642 72th Avenue W","8085 Noemie Way","3776 5th St. NE","310  Walsh Parks Drive","784 West Sporer Ste","024 Kunze Pl SW","338 69th Ave.","155 W First St Apt. 996","4873 Shanika Place South","67568 South Lang Ste","8783 21th Street NE","91799 Torry Vista SouthEast","42369 S Waelchi Ste","87837 SouthWest Skiles Ave",".","23056 Schamberger Highway","0620 12th St","24531 Lionel Dr NE","84964 NorthEast Howe Market","4067 Ruie Way NorthEast","0502 Bartell Place South","471 Gulgowski Hwy Apt. 565","40101 Graciela St W","32555 Pollich Vista NE","112 Wintheiser Vista E","7344 NorthWest Thor Drive","21492 SW Damian St","0258 13th St","247 SouthWest 66th Ave","82404 Parlee Pl Apt. 836","6823 North Reichert Avenue","121414","604 34th St. Apt. 315","33768 Hyatt Road S Apt. 490","887 Gene Highway NorthWest","2054 West Elouise Street","629 Langworth Blvd","84270 NorthWest 73th Street","202 NE 72th Ave","01868 Lott Blvd SW","519 Walter Mount Apt. 558 Pl Apt. 558","student","982 Judah Street NorthEast","7268 William Way","6771 72th Avenue","76114 East Boyle Drive","8009 Gleichner Ste","193 NW Graham Ln","91562 West Virgle Blvd","859 Nellie Highway Apt. 612"],["Pierce County","Clallam County","Franklin County","Franklin County","Pierce County","Clark County","Benton County","King County","Snohomish County","King County","King County","Pierce County","Clark County","Clark County","Pierce County","Pierce County","King County","Spokane County","Skagit County","Benton County","Yakima County","Clark County","Yakima County","Whitman County","King County","Spokane County","King County","King County","Cowlitz County","Pierce County","Snohomish County","King County","King County","Skagit County","KingCounty","Pierce County","Whatcom County","Mason County","King County","Thurston County","Clark County","Snohomish County","Benton County","King County","Adams County","Snohomish County","Island County","Pierce County","Yakima County","Kitsap County"],["oo'connell@gmail.com","krajcik.glynn@yahoo.com","dessa19@llc.org","kschamberger@gleichner.com","tromp.jeryl@schmeler.com","benita54@llc.biz","nitzsche.lue@yahoo.com","david.hegmann@corwin.net","noreta.nader@yahoo.com","evalena.hoppe@hotmail.com","arno.price@gmail.com","olson.dwight@abernathy.info","christopher79@yahoo.com","bkilback@hotmail.com","hassan60@yahoo.com","schuppe.ismael@hotmail.com","kourtney96@dickens.com","peffertz@gmail.com","jasiah67@douglas.org","taylor.dickinson@and.net","auguste81@llc.net","rogers07@gmail.com","ashlynn09@ltd.com","corwin.d'amore@gmail.com","champlin.simmie@koelpin.com","derwin.lakin@hotmail.com","jaymes58@weissnat.org","saundra.stokes@and.com","cbahringer@effertz.com","texie28@ziemann.com","patty.hegmann@hotmail.com","migdalia26@plc.com","dnader@douglas.com","bnitzsche@llc.com","purdy.debrah@and.net","witting.titus@rogahn.biz","wfeil@yahoo.com","dedrick23@yahoo.com","swunsch@hessel.com","marla.hilpert@plc.net","hahn.jordi@llc.com","oaufderhar@shields.com","rgraham@yahoo.com","hobert56@and.com","algernon48@legros.com","schmidt.mindi@hahn.net","nataly.larkin@gmail.com","myrtice06@hotmail.com","corine36@yahoo.com","camilo08@plc.com"],["238-81-8708","163-43-7463","781-67-7952","889-26-9781","767-31-0340","331-49-2985","190-84-3572","453-96-0906","049-09-4282","145-86-8348","711-31-4379","022-82-3463","813-25-2378","110-18-7141","567-95-9399","779-12-8946","625-23-5974","879-46-3580","324-37-5569","408-32-1502","465-53-4141","584-54-1238","378-73-5246","365-69-5143","654-37-2306","151-38-6247","374-37-5667","168-84-7917","434-24-0098","684-25-8178","451-19-6310","245-66-3057","173-93-4926","456-70-5753","120-73-0225","529-87-7631","165-68-2317","423-48-4052","390-70-3473","677-14-7975","405-07-3073","156-68-4141","212-14-1860","632-63-3159","210-92-9715","374-16-8013","830-02-9881","389-26-6724","743-38-3431","018-37-1375"],["1960-06-05","1940-10-31","1970-08-10","2010-09-03","1937-08-01","2017-04-10","2016-02-26","2019-01-31","2004-03-05","2001-08-07","1994-07-17","1985-08-07","2019-06-26","1956-09-07","1982-10-06","2012-04-10","1981-10-17","1934-05-31","2024-02-06","1958-04-17","1950-08-30","2014-06-12","1990-06-10","1997-10-15","1939-03-21","2022-10-27","2020-07-02","2018-01-19","1935-11-26","1981-06-02","1979-06-23","2022-09-15","1960-12-20","1938-05-09","2009-08-20","1981-10-09","2021-05-04","1961-10-05","1938-10-26","1955-12-23","1974-01-07","1935-02-09","2012-04-30","1952-11-28","1970-06-22","1996-01-27","1936-05-08","1939-10-06","1991-06-15","1985-02-08"],["2024-02-24","09/14/24","2025/01/10","2025-08-20","04/14/2024","2025/06/20","04/13/25","04/19/2024","07/11/2025","2025-07-23","01/08/2025","2024/04/20","03/13/25","2025/04/18","12/07/2024","01/16/2024","01/14/2024","03/20/2025","04/13/2024","2024-08-12","2025-07-17","2024-12-18","12/07/2024","01/30/2024","2025/02/17","05/17/2024","2025/04/11","2025-04-29","02/28/25","04/11/2025","05/25/2025","2024/07/07","07/18/2024","2025-03-25","01/01/2024","2025/07/11","2025-04-05","2025-07-09","07/23/25","08/02/2025","08/15/2024","02/06/2025","2024-05-09","03/01/2025","03/28/2024","07/30/25","2024/02/21","2025-04-27","07/20/2025","2025-02-08"],[1,0.34,0.964,0.924,0.825,0.703,0.349,0.06,0.454,0.237,0.193,0.997,0.834,0.575,1,0.984,0.357,0.604,0.5669999999999999,0.213,0.968,0.846,0.781,0.999,0.355,0.261,0.363,0.112,0.008999999999999999,0.763,0.931,0.258,0.442,0.491,0.044,1,0.018,0.712,0.225,0.001,0.701,0.975,0.311,0.068,0.848,0.879,0.721,0.923,0.913,0.55],[1,0.018,0.914,0.677,1,0.621,0.496,0.48,1,0.157,0.608,0.996,0.5649999999999999,0.589,0.911,1,0.171,0.53,0.312,0.421,0.77,0.837,0.945,0.117,0.09,0.151,0.411,0.405,0.122,0.013,0.998,0.603,0.639,0.724,0.261,1,0,0.973,0.526,0.08799999999999999,0.456,1,0.76,0.646,1,0.999,0.753,1,0.664,0.511],[0.99,0.34,0.88,0.9,0.8,0.65,0.34,0.01,0.42,0.21,0.18,0.99,0.8,0.51,0.97,0.95,0.3,0.55,0.52,0.18,0.92,0.8100000000000001,0.77,0.99,0.33,0.25,0.34,0.08,0,0.76,0.86,0.24,0.39,0.4,0.02,0.97,0,0.67,0.14,0,0.67,0.97,0.27,0.05,0.84,0.82,0.65,0.9,0.9,0.53],[1,0.35,0.99,0.93,0.84,0.74,0.37,0.09,0.47,0.24,0.27,1,0.85,0.66,1,0.99,0.37,0.64,0.57,0.24,0.98,0.9399999999999999,0.78,1,0.42,0.26,0.38,0.15,0.02,0.79,0.97,0.27,0.46,0.5,0.1,1,0.03,0.72,0.24,0.06,0.72,1,0.34,0.12,0.9399999999999999,0.88,0.74,0.93,1,0.5600000000000001],["2024-02-24","2024-09-14","2025-01-10","2025-08-20","2024-04-14","2025-06-20","2025-04-13","2024-04-19","2025-07-11","2025-07-23","2025-01-08","2024-04-20","2025-03-13","2025-04-18","2024-12-07","2024-01-16","2024-01-14","2025-03-20","2024-04-13","2024-08-12","2025-07-17","2024-12-18","2024-12-07","2024-01-30","2025-02-17","2024-05-17","2025-04-11","2025-04-29","2025-02-28","2025-04-11","2025-05-25","2024-07-07","2024-07-18","2025-03-25","2024-01-01","2025-07-11","2025-04-05","2025-07-09","2025-07-23","2025-08-02","2024-08-15","2025-02-06","2024-05-09","2025-03-01","2024-03-28","2025-07-30","2024-02-21","2025-04-27","2025-07-20","2025-02-08"],["Pierce","Clallam","Franklin","Franklin","Pierce","Clark","Benton","King","Snohomish","King","King","Pierce","Clark","Clark","Pierce","Pierce","King","Spokane","Skagit","Benton","Yakima","Clark","Yakima","Whitman","King","Spokane","King","King","Cowlitz","Pierce","Snohomish","King","King","Skagit","King","Pierce","Whatcom","Mason","King","Thurston","Clark","Snohomish","Benton","King","Adams","Snohomish","Island","Pierce","Yakima","Kitsap"],["642 72th Avenue W","8085 Noemie Way","3776 5th St Ne","310 Walsh Parks Drive","784 West Sporer Ste","024 Kunze Pl Sw","338 69th Ave","155 W First St Apt 996","4873 Shanika Place South","67568 South Lang Ste","8783 21th Street Ne","91799 Torry Vista Southeast","42369 S Waelchi Ste","87837 Southwest Skiles Ave",null,"23056 Schamberger Highway","0620 12th St","24531 Lionel Dr Ne","84964 Northeast Howe Market","4067 Ruie Way Northeast","0502 Bartell Place South","471 Gulgowski Hwy Apt 565","40101 Graciela St W","32555 Pollich Vista Ne","112 Wintheiser Vista E","7344 Northwest Thor Drive","21492 Sw Damian St","0258 13th St","247 Southwest 66th Ave","82404 Parlee Pl Apt 836","6823 North Reichert Avenue",null,"604 34th St Apt 315","33768 Hyatt Road S Apt 490","887 Gene Highway Northwest","2054 West Elouise Street","629 Langworth Blvd","84270 Northwest 73th Street","202 Ne 72th Ave","01868 Lott Blvd Sw","519 Walter Mount Apt 558 Pl Apt 558",null,"982 Judah Street Northeast","7268 William Way","6771 72th Avenue","76114 East Boyle Drive","8009 Gleichner Ste","193 Nw Graham Ln","91562 West Virgle Blvd","859 Nellie Highway Apt 612"],["820-530-6339","120-107-6198","037-411-3655","133-055-7951","964-067-0977","285-564-3248","381-581-1622","967-598-3721","986-101-1561","794-509-1258","204-215-2467","660-906-7418","443-986-9818","297-937-9111","302-963-9404","098-485-0406","123-509-1981","576-650-7406","947-002-9503","735-803-6969","628-634-3576","536-801-4341","174-391-5396","345-199-3341","830-047-1365","132-613-6161","628-474-6359","768-965-0410","365-037-7272","255-283-5249","885-440-0869","205-934-1635","625-074-2638","021-301-1623","586-682-2892","563-166-8152","381-929-4684","216-894-6343","882-860-9256","815-600-9715","861-122-3193","474-819-6144","406-169-5989","994-858-1629","588-404-7093","184-895-3288","998-792-3930","378-447-0932","032-649-6786","094-226-7923"],["Elevate Health","Olympic Community of Health","Greater Health Now","Greater Health Now","Elevate Health","Southwest Washington","Greater Health Now","Healthier Here","North Sound","Healthier Here","Healthier Here","Elevate Health","Southwest Washington","Southwest Washington","Elevate Health","Elevate Health","Healthier Here","Better Health Together","North Sound","Greater Health Now","Greater Health Now","Southwest Washington","Greater Health Now","Greater Health Now","Healthier Here","Better Health Together","Healthier Here","Healthier Here","Cascade Pacific Action Alliance","Elevate Health","North Sound","Healthier Here","Healthier Here","North Sound","Healthier Here","Elevate Health","North Sound","Cascade Pacific Action Alliance","Healthier Here","Cascade Pacific Action Alliance","Southwest Washington","North Sound","Greater Health Now","Healthier Here","Better Health Together","North Sound","North Sound","Elevate Health","Greater Health Now","Olympic Community of Health"]],"container":"<table class=\"cell-border stripe\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>first_name<\/th>\n      <th>last_name<\/th>\n      <th>phone_number<\/th>\n      <th>street<\/th>\n      <th>county<\/th>\n      <th>email<\/th>\n      <th>ID<\/th>\n      <th>dob<\/th>\n      <th>date<\/th>\n      <th>val1<\/th>\n      <th>val2<\/th>\n      <th>val1_lower_CI<\/th>\n      <th>val1_upper_CI<\/th>\n      <th>date_format<\/th>\n      <th>county_name<\/th>\n      <th>address_clean<\/th>\n      <th>phone_clean<\/th>\n      <th>ACH<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"scrollX":true,"pageLength":5,"autoWidth":true,"columnDefs":[{"className":"dt-right","targets":[10,11,12,13]},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"first_name","targets":1},{"name":"last_name","targets":2},{"name":"phone_number","targets":3},{"name":"street","targets":4},{"name":"county","targets":5},{"name":"email","targets":6},{"name":"ID","targets":7},{"name":"dob","targets":8},{"name":"date","targets":9},{"name":"val1","targets":10},{"name":"val2","targets":11},{"name":"val1_lower_CI","targets":12},{"name":"val1_upper_CI","targets":13},{"name":"date_format","targets":14},{"name":"county_name","targets":15},{"name":"address_clean","targets":16},{"name":"phone_clean","targets":17},{"name":"ACH","targets":18}],"order":[],"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script>

## Calculating summary stats

This uses modern tidyverse conventions for grouping, so rather than
using:

`data |> group_by(grouping_column) |> summarise(mean = mean(value))`, it
uses:

`data |> summarise(mean = mean(value), .by = grouping_column)`

Using .by produces grouping that is not persistent (so you don’t need to
use `ungroup()`), it is just applied to the operation where it is being
used

``` r
# calculating mean, median, sd, quantiles per group (County)
wa_health_by_county <- wa_health_data_cleaned |> 
  summarise(val1_mean = mean(val1),
            val1_median = median(val1),
            val1_sd = sd(val1),
            val1_q25 = quantile(val1, .25), 
            val1_q75 = quantile(val1, .75),
            val2_mean = mean(val2, na.rm = T), # this removes NAs when calculating the mean, otherwise the result would produce NA
            num_records = n(), .by = county_name)
```

## Visualizing

### Bar Plots

``` r
# creating a bar plot of the summarized data
# use reorder on the axis to set the order of the bars and change the color
ggplot(wa_health_by_county,
       aes(x = reorder(county_name, val1_mean, decreasing = T), val1_mean)) +
  geom_col(fill = "#060270") +
  # add text labels of the value of above the bar
  geom_text(aes(label = round(val1_mean, 2)), vjust = -.2, color = 'black', size = 3) +
  theme_bw() + # set a basic black and white theme
  xlab('County') + # change the name of the x-axis label
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) # rotate the x-axis labels so that they fit/can be read
```

![](exploratory_data_analysis_files/figure-commonmark/unnamed-chunk-14-1.png)

``` r
# alternative bar plot
# creates a color scheme where bars alternate in color so it is easier to distinguish, to do this first have to sort the data and add a column grouping the data into two groups
wa_health_by_county <- wa_health_by_county |>
  arrange(desc(val1_mean)) |> # sort in descending order
  mutate(color_group = row_number() %% 2)  # add a column based on the row number
  
wa_health_by_county |> 
  ggplot(aes(x = reorder(county_name, val1_mean), val1_mean)) +
  # create column coloring the bars based on the column created
  geom_col(aes(fill = factor(color_group))) +
  # create a custom color scale for the data
  scale_fill_manual(values = c("0" = "#060270", "1" = "#4a4ab8")) +
  theme_bw() + # set theme to black and white
  guides(fill = 'none') + # remove the legend
  xlab('County') + # change axis label
  coord_flip() # rotate the plot so that it is easier to read the axis labels
```

![](exploratory_data_analysis_files/figure-commonmark/unnamed-chunk-14-2.png)

### Map Plot

``` r
# pull US mapping data
us_county_map <- usmap::us_map('counties')
# subset to WA state data
WA_map_counties <- filter(us_county_map, abbr == 'WA') |>
  # simplify county names
    mutate(county = str_replace(county, " County", ""))
# convert a geometry object to an sfc object for plotting purposes
WA_sfc <- st_as_sfc(WA_map_counties, crs = usmap_csv()@projargs)
# get subset of map object
# creates sf object, which extends data.frame like objects with a  simple feature list column
WA_sf_data <- st_sf(data.frame(fips = unique(WA_map_counties$fips), county = WA_map_counties$county, geometry = WA_sfc))
# join map data with the data you want to plot/fill in each county with
WA_sf_map <- left_join(WA_sf_data, wa_health_by_county, join_by(county == county_name))
# find the centroid of each county
WA_sf_map$centroids <- st_centroid(WA_sf_map$geometry)

wa_map_plot <- ggplot(WA_sf_map) +
  # use geom_sf to create map - pulls from geometry column in the data
  # set fill for each county
    geom_sf(aes(fill =val1_mean), color = 'white') +
  # change fill - alternate color palette
  scale_fill_viridis_c(option = 'viridis') +
  # add county labels
    geom_sf_text(aes(label = county), size = 3, color = 'black') +
  coord_sf(crs = st_crs(4283)) + #needed to make flat/horizontally aligned map for visualizing purposes
  labs(fill = 'value mean') + # adjust legend title
  # set clean theme that removes axis labels
  theme_void()

wa_map_plot
```

    Warning in st_point_on_surface.sfc(sf::st_zm(x)): st_point_on_surface may not
    give correct results for longitude/latitude data

![](exploratory_data_analysis_files/figure-commonmark/unnamed-chunk-15-1.png)
