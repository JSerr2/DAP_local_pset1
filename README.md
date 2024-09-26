# Data Skills 2 - R
## Fall Quarter 2024

## Homework 1
## Due: October 12, 2024 before midnight on Gradescope

__Question 1 (40%):__ The two datasets included in the assignment repo are downloaded directly from the BEA.  The file labeled "total" has the total employment per state for the years 2000 and 2017.  The file labeled "by industry" has employment per industry in each of 10 industries per state for the same years.

Load and merge the data into a panel dataframe, with the columns: "state", "year", and one for each of the 10 industries.  Every state-year combination should uniquely identify a row.  No more and no less than 12 columns should remain.  Do any necessary cleaning for the data to be easily usable.

The values should be given as the share of the total employment in that place and time, e.g. if total employment in a place and time was 100, and the employment in one industry was 10, then the value shown for that state-year industry should be 0.1.  The "total" values should not be a part of the final dataframe.  

Output this dataframe to a csv document named "data.csv" and sync it to your homework repo with your code.

__Question 2 (30%):__ Question 2: _(Note: this was edited on 1/6/2024 to clarify the question. The objective of the question has not changed from the original version.)_

Write a function that finds the top states with the highest state-specific industry shares for an industry in a given year. This function should take as input 3 parameters: the number of states reported in the output, the industry, and the year. It should return a vector or list of states.
* 2a. Use this function to find the 5 states with the largest within-state manufacturing shares (i.e., manufacturing employment / total employment in that state) in 2000.
* 2b. For each of the 5 states that you just identified, find the state's manufacturing share in each year from 2000 and 2017.
* 2c. Plot how each of those states' manufacturing employment share changed from 2000 and 2017. 
* 2d. Place the code for steps 2a-2c within a single run of a for-loop. Then, modify the for-loop so that it also repeats steps 2a-2c for the 10 states with the largest within-state farming share in 2000, and for the 15 states with the largest within-state information share in 2017.


__Question 3 (30%)__ Write a function that plots where a given state is in the distribution of employment shares for a given industry in 2000 and 2017, as well as the distribution of the change in employment shares from 2000 to 2017. This function should take as input 2 parameters: the state and the industry. For each year as well as for the difference, it should plot a histogram of employment shares for that industry, and then include a vertical line for the mean of the overall distribution and a vertical line for the value for the given state. It should be clearly denoted which is the mean and which is that state's value. Note that since the shares here are already in percent terms, the change is just calculated as the difference in shares.
