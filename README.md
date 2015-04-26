# getting-and-cleaning-data-programming
Repo for the programming assignment on Coursera

how to get started
	1	mount your workspace by using setwd()
	2	download and ’run_analysis.R'
	3	download raw data stored in .zip-file (use the download.file() routine in 'run_analysis.R')
	4	unzip the downloaded .zip-file manually into your workspace. I suggest naming the unzipped folder for the files
	  "UCI HAR Dataset", otherwise the code will have to be adjusted to your specifications
	5	run the script 'run_analysis.R'
	6	look into your set workspace directory to find "cleaned_nonaggregated_data.txt" as the cleaned dataset, as well as
	  "averaged_data.txt" as the second, tidy data set (step 5).

Note: All steps are commented in the 'run_analysis.R' code. Due to lack of proper understanding of Task #4 I didn't change
      too many of the variable names. Basically, it's the same as the names given in the original dataset.
