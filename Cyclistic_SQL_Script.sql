-- The purpose of this script is to create a data table and load the raw data to the database management system 

-- This code will allow us to upload files from the local computer to the database server
-- SET GLOBAL local_infile=1; 

-- This code will create the table that we will analyze
CREATE TABLE bikes.work
	(
	ride_id VARCHAR(50),
    rideable_type VARCHAR(50), 
    started_at VARCHAR(50), 
    ended_at VARCHAR(50), 
    start_station_name VARCHAR(53), 
    start_station_id VARCHAR(50), 
    end_station_name VARCHAR(53), 
    end_station_id VARCHAR(50), 
    start_lat DECIMAL(12,10), 
    start_lng DECIMAL(12,10),
    end_lat DECIMAL(12,10),
    end_lng DECIMAL(12,10),
    member_casual VARCHAR(50)
    );

-- This code will load the actual data we will analyze to our table  
/*
LOAD DATA LOCAL INFILE 'C:/Users/tonyb/Documents/_Data Analysis/R/R_Projects_Directory/Cyclistic_Bike_Share/data/raw/Bike_Data_202201.csv' 
INTO TABLE bikes.work
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'C:/Users/tonyb/Documents/_Data Analysis/R/R_Projects_Directory/Cyclistic_Bike_Share/data/raw/Bike_Data_202202.csv' 
INTO TABLE bikes.work
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'C:/Users/tonyb/Documents/_Data Analysis/R/R_Projects_Directory/Cyclistic_Bike_Share/data/raw/Bike_Data_202203.csv' 
INTO TABLE bikes.work
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'C:/Users/tonyb/Documents/_Data Analysis/R/R_Projects_Directory/Cyclistic_Bike_Share/data/raw/Bike_Data_202204.csv' 
INTO TABLE bikes.work
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'C:/Users/tonyb/Documents/_Data Analysis/R/R_Projects_Directory/Cyclistic_Bike_Share/data/raw/Bike_Data_202205.csv' 
INTO TABLE bikes.work
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'C:/Users/tonyb/Documents/_Data Analysis/R/R_Projects_Directory/Cyclistic_Bike_Share/data/raw/Bike_Data_202206.csv' 
INTO TABLE bikes.work
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'C:/Users/tonyb/Documents/_Data Analysis/R/R_Projects_Directory/Cyclistic_Bike_Share/data/raw/Bike_Data_202207.csv' 
INTO TABLE bikes.work
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'C:/Users/tonyb/Documents/_Data Analysis/R/R_Projects_Directory/Cyclistic_Bike_Share/data/raw/Bike_Data_202208.csv' 
INTO TABLE bikes.work
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'C:/Users/tonyb/Documents/_Data Analysis/R/R_Projects_Directory/Cyclistic_Bike_Share/data/raw/Bike_Data_202209.csv' 
INTO TABLE bikes.work
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'C:/Users/tonyb/Documents/_Data Analysis/R/R_Projects_Directory/Cyclistic_Bike_Share/data/raw/Bike_Data_202210.csv' 
INTO TABLE bikes.work
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'C:/Users/tonyb/Documents/_Data Analysis/R/R_Projects_Directory/Cyclistic_Bike_Share/data/raw/Bike_Data_202211.csv' 
INTO TABLE bikes.work
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'C:/Users/tonyb/Documents/_Data Analysis/R/R_Projects_Directory/Cyclistic_Bike_Share/data/raw/Bike_Data_202212.csv' 
INTO TABLE bikes.work
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS;
*/
   
-- This code will rename ambiguous columns to more appropriate data labels
ALTER TABLE bikes.work
RENAME COLUMN rideable_type TO bike_type;

ALTER TABLE bikes.work
RENAME COLUMN start_station_name TO start_sta_name;

ALTER TABLE bikes.work
RENAME COLUMN start_station_id TO start_sta_id;

ALTER TABLE bikes.work
RENAME COLUMN end_station_name TO end_sta_name;

ALTER TABLE bikes.work
RENAME COLUMN end_station_id TO end_sta_id;

ALTER TABLE bikes.work
RENAME COLUMN member_casual TO user_type;

-- This code will allow records to be deleted
SET SQL_SAFE_UPDATES = 0;

-- This code will delete all rows with NULL values  
-- I used a Common Table Expression to circumvent MySQL's table editing policies
-- The CTE finds all NULL records across the entire table
-- And the delete statement deletes all the records in the CTE
WITH cte_delete_nulls AS
(
    SELECT ride_id
    FROM bikes.work
    WHERE ride_id IS NULL OR ride_id = ''
        UNION ALL
    SELECT ride_id
    FROM bikes.work
    WHERE bike_type IS NULL OR bike_type = ''
        UNION ALL    
    SELECT ride_id
    FROM bikes.work
    WHERE started_at IS NULL OR started_at = ''
        UNION ALL
    SELECT ride_id
    FROM bikes.work
    WHERE ended_at IS NULL OR ended_at = ''
        UNION ALL
    SELECT ride_id
    FROM bikes.work
    WHERE start_sta_name IS NULL OR start_sta_name = ''
        UNION ALL
    SELECT ride_id
    FROM bikes.work
    WHERE start_sta_id IS NULL OR start_sta_id = ''
        UNION ALL
    SELECT ride_id
    FROM bikes.work
    WHERE end_sta_name IS NULL OR end_sta_name = ''
        UNION ALL
    SELECT ride_id
    FROM bikes.work
    WHERE end_sta_id IS NULL OR end_sta_id = ''
        UNION ALL
    SELECT ride_id
    FROM bikes.work
    WHERE start_lat IS NULL
        UNION ALL
    SELECT ride_id
    FROM bikes.work
    WHERE start_lng IS NULL
        UNION ALL
    SELECT ride_id
    FROM bikes.work
    WHERE end_lat IS NULL
        UNION ALL
    SELECT ride_id
    FROM bikes.work
    WHERE end_lng IS NULL   
        UNION ALL
    SELECT ride_id
    FROM bikes.work
    WHERE user_type IS NULL OR user_type = '' 
)
DELETE FROM bikes.WORK 
WHERE ride_id IN (SELECT * FROM cte_delete_nulls);

-- This code will transform the data to the correct data types  
ALTER TABLE bikes.work 
ADD COLUMN new_start DATETIME;

UPDATE bikes.work
SET new_start = STR_TO_DATE(started_at, '%m/%d/%Y %H:%i');

ALTER TABLE bikes.work 
ADD COLUMN new_end DATETIME;

UPDATE bikes.work
SET new_end = STR_TO_DATE(ended_at, '%m/%d/%Y %H:%i');

ALTER TABLE bikes.work 
DROP COLUMN started_at;

ALTER TABLE bikes.work 
RENAME COLUMN new_start TO started_at;

ALTER TABLE bikes.work
MODIFY COLUMN started_at DATETIME AFTER bike_type;

ALTER TABLE bikes.work 
DROP COLUMN ended_at;

ALTER TABLE bikes.work 
RENAME COLUMN new_end TO ended_at;

ALTER TABLE bikes.work 
MODIFY COLUMN ended_at DATETIME AFTER started_at;

-- This code will find all duplicate record instances.  No further action needed as there were no duplicates
SELECT COUNT(*) AS Test_Duplicates
FROM bikes.work 
GROUP BY ride_id, bike_type, started_at, ended_at, start_sta_name
HAVING COUNT(*) > 1;

-- This code will remove trailing and leading spaces on all strings
UPDATE bikes.work 
SET ride_id = TRIM(ride_id); 

UPDATE bikes.work 
SET bike_type = TRIM(bike_type); 

UPDATE bikes.work 
SET start_sta_name  = TRIM(start_sta_name); 

UPDATE bikes.work 
SET start_sta_id  = TRIM(start_sta_id); 

UPDATE bikes.work 
SET end_sta_name  = TRIM(end_sta_name); 

UPDATE bikes.work 
SET end_sta_id  = TRIM(end_sta_id); 

UPDATE bikes.work 
SET user_type = TRIM(user_type); 

-- This code will delete all records with a ride length < 1
WITH cte_delete_less_than_one AS
(
    SELECT ride_id FROM bikes.work
    WHERE TIMESTAMPDIFF(MINUTE, started_at, ended_at) < 1 
)
DELETE FROM bikes.work 
WHERE ride_id IN (SELECT * FROM cte_delete_less_than_one);

-- This code will create a view so our stakeholders can look at the data themselves while protecting the database from inexperienced users, and without providing customer identifying information
USE bikes;

CREATE VIEW explore_vw AS
	SELECT
		bike_type,
		started_at,
		ended_at,
		start_sta_name,
		start_sta_id,
		end_sta_name,
		end_sta_id,
		start_lat,
		start_lng,
		end_lat,
		end_lng,
		user_type,
		TIMESTAMPDIFF(MINUTE, started_at, ended_at) AS ride_length,
		DAYNAME(started_at) AS day_of_week
	FROM bikes.work;

-- Data Analysis ----------------------------------------------------------------
-- This code will find the average ride length in minutes by user type
SELECT 
	COALESCE(user_type, 'combined') AS user_type,
	ROUND(AVG(TIMESTAMPDIFF(MINUTE, started_at, ended_at)), 2) AS avg_ride_length_min
FROM 
	bikes.work
GROUP BY
	user_type WITH ROLLUP;

-- This code will find the minimum ride length in minutes
SELECT 
	MIN(TIMESTAMPDIFF(MINUTE, started_at, ended_at)) AS min_ride_length_min
FROM 
	bikes.work;

-- This code will find the maximum ride length in minutes
SELECT 
	MAX(TIMESTAMPDIFF(MINUTE, started_at, ended_at)) AS max_ride_length_min
FROM 
	bikes.work;

-- This code will find the average ride length in minutes per user type per day of the week
SELECT 
	COALESCE(user_type,'combined') AS user_type,
	AVG(CASE WHEN DAYNAME(started_at) = 'Monday' THEN TIMESTAMPDIFF(MINUTE,started_at, ended_at) ELSE NULL END) AS avg_ride_length_monday,
	AVG(CASE WHEN DAYNAME(started_at) = 'Tuesday' THEN TIMESTAMPDIFF(MINUTE,started_at, ended_at) ELSE NULL END) AS avg_ride_length_tuesday,
	AVG(CASE WHEN DAYNAME(started_at) = 'Wednesday' THEN TIMESTAMPDIFF(MINUTE,started_at, ended_at) ELSE NULL END) AS avg_ride_length_wednesday,
	AVG(CASE WHEN DAYNAME(started_at) = 'Thursday' THEN TIMESTAMPDIFF(MINUTE,started_at, ended_at) ELSE NULL END) AS avg_ride_length_thursday,
	AVG(CASE WHEN DAYNAME(started_at) = 'Friday' THEN TIMESTAMPDIFF(MINUTE,started_at, ended_at) ELSE NULL END) AS avg_ride_length_friday,
	AVG(CASE WHEN DAYNAME(started_at) = 'Saturday' THEN TIMESTAMPDIFF(MINUTE,started_at, ended_at) ELSE NULL END) AS avg_ride_length_saturday,
	AVG(CASE WHEN DAYNAME(started_at) = 'Sunday' THEN TIMESTAMPDIFF(MINUTE,started_at, ended_at) ELSE NULL END) AS avg_ride_length_sunday,
	AVG(TIMESTAMPDIFF(MINUTE,started_at, ended_at)) AS avg_ride_length_total
FROM 
	bikes.work
GROUP BY 
	user_type WITH ROLLUP;

-- This code will find the count number of rides per user type per day of the week
SELECT 
	COALESCE(user_type,'combined') AS user_type,
	COUNT(CASE WHEN DAYNAME(started_at) = 'Monday' THEN 1 ELSE NULL END) AS num_rides_monday,
	COUNT(CASE WHEN DAYNAME(started_at) = 'Tuesday' THEN 1 ELSE NULL END) AS num_rides_tuesday,
	COUNT(CASE WHEN DAYNAME(started_at) = 'Wednesday' THEN 1 ELSE NULL END) AS num_rides_wednesday,
	COUNT(CASE WHEN DAYNAME(started_at) = 'Thursday' THEN 1 ELSE NULL END) AS Nnum_rides_thursday,
	COUNT(CASE WHEN DAYNAME(started_at) = 'Friday' THEN 1 ELSE NULL END) AS num_rides_friday,
	COUNT(CASE WHEN DAYNAME(started_at) = 'Saturday' THEN 1 ELSE NULL END) AS num_rides_saturday,
	COUNT(CASE WHEN DAYNAME(started_at) = 'Sunday' THEN 1 ELSE NULL END) AS num_rides_sunday,
	COUNT(*) AS grand_total
FROM 
	bikes.work
GROUP BY user_type WITH ROLLUP;

-- This code will group all users into three categories based on ride_length, and calculate the number of users in that group per user type
SELECT 
	IF(GROUPING(user_categories.name), 'all rides', user_categories.name) AS category_name,
	IF(GROUPING(users.user_type), 'combined', users.user_type) AS user_types,
	COUNT(total_minutes) AS number_of_users
FROM (
    SELECT 
	    user_type, 
	    TIMESTAMPDIFF(MINUTE, started_at, ended_at
	    ) AS total_minutes
    FROM 
    	bikes.work) AS users
        INNER JOIN (
			SELECT 
				'short rides' AS 'name', 1 AS 'low_limit', 10 AS 'high_limit'
			UNION ALL
			SELECT 
				'medium rides' AS 'name', 11 AS 'low_limit', 20 AS 'high_limit'
			UNION ALL
			SELECT 
				'long rides' AS 'name', 21 AS 'low_limit', 99999 AS 'high_limit'
            ) AS user_categories
        ON 
        	users.total_minutes
            BETWEEN 
            	user_categories.low_limit AND user_categories.high_limit
GROUP BY 
	user_categories.name, users.user_type WITH ROLLUP
ORDER BY 
	category_name DESC;

-- This code will count the number of rides per user type per month 
SELECT 
	IF(GROUPING(months.month_name), 'all year', months.month_name) AS month,
	IF(GROUPING(months.user_type), 'combined', months.user_type) AS user_types,
	COUNT(*) AS number_of_users
FROM (
    SELECT 
	    *,
	    MONTHNAME(started_at) AS month_name
    FROM bikes.work
    ) AS months
GROUP BY 
	months.month_name, months.user_type WITH ROLLUP
ORDER BY 
	FIELD(month, 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December', 'all year'), FIELD(user_types, 'casual', 'member', 'combined');

-- This code will find the top 10 most popular bike rides based on the start and end station names
SELECT 
	start_sta_name,
	end_sta_name,
	COUNT(start_sta_name) AS total_rides_occurred
FROM 
	bikes.work
GROUP BY 
	start_sta_name, 
	end_sta_name
ORDER BY 
	total_rides_occurred DESC
LIMIT 10;

-- This code will count the number of rides per hour of the day 
SELECT
	IF(GROUPING(time.hour), 'total', time.hour) AS hour_of_the_day,
	COUNT(*) AS total_rides
FROM (
	SELECT 
		*,
		DATE_FORMAT(started_at, '%l %p') AS hour
	FROM bikes.work
	) AS time
GROUP BY 
	time.hour WITH ROLLUP
ORDER BY 
	FIELD(hour_of_the_day, '12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM', '12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM', 'total');
	