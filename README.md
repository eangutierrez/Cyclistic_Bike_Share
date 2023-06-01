# Cyclistic Bike Share Study Case
<img src="https://cdn.stocksnap.io/img-thumbs/960w/bridge-people_ONSNHWZTQ9.jpg"
 width=50% height=50% align=right>

## Project Overview
* Created a MySQL script to upload and pre-process the data, as well as build several pivot tables to help leadership better understand customer segments.
* Created an R programming language script to upload and pre-process the data, build several pivot tables, and create visualizations to better present data to leadership.
* Created a [Tableau Storyboard](https://public.tableau.com/app/profile/tony.gutierrez/viz/CyclisticBikeShare_16843493078500/Story1) to present findings to leadership, including several recommendations.
* Wrote a series of [Medium]([https://medium.com/@tonygutierrez_60520](https://medium.com/@tonygutierrez_60520/cyclistic-bike-share-business-case-introduction-f3efb2294687) articles to futher explain teach stage of the data analysis process.

## Code and Resources Used
**R Version:** 4.2.2

**MySQL Version:** 8.0.31

## Data Cleaning
After uploading the data, I performed several pre-processing tasks to prepare it for analysis.  I performed the following changes to the data:

### Added
* ride_length column by subtracting starting time from ending time
* day_of_week column by WEEKDAY() function

### Changed
* Renamed all csv files to follow "Bike__Data_YYYYMM" nomenclature 
* Renamed "rideable_type" column to "bike_type" 
* Renamed "start_station_name" column to "start_sta_name"
* Renamed "start_station_id" column to "start_sta_id"
* Renamed "end_station_name" column to "end_sta_name"
* Renamed "end_station_id" column to "end_sta_id"
* Renamed "member_casual" column to "user_type" column
* Renamed "rideable_type" column to "bike_type" column
* "started_at" column's data type changed to date and time
* "ended_at" column's data type changed to date and time
* Replaced "Clybourne Ave" to "Clybourn Ave" in all columns
* Applied the CamelCase naming convention to the data set 

### Removed
* All null values in the data set
* All trailing, leading, and excess spaces in the data set
* All records with a ride length of less than one minute

## Exploratory Data Analysis
I created several visualizations to better understand the data's distribution, value differences, and categorical variables.  Below are a few highlights from this phase:

![sql_table_12](https://github.com/eangutierrez/Cyclistic_Bike_Share/assets/92600212/d9982365-deed-4c06-a493-29d7b231e1d2)
![r_table_4](https://github.com/eangutierrez/Cyclistic_Bike_Share/assets/92600212/40c292ff-bc50-45d5-98a7-145c7bdecc5e)
![r_table_7](https://github.com/eangutierrez/Cyclistic_Bike_Share/assets/92600212/2c01b149-9bda-464f-b399-2162e5b8fb15)
![r_table_5](https://github.com/eangutierrez/Cyclistic_Bike_Share/assets/92600212/01debb1e-69cc-4efa-844c-3189b6d9bb25)
