The probability of a marriage ending in divorce in Mexico
=========================================================
This program calculates the probability that a marriage in Mexico ends in divorce based on data obtained from the INEGI. Basically reproduce some of the charts in [Marriage and Divorce: Changes and their Driving Forces][1] [PDF] by Betsey Stevenson and Justin Wolfers with data from Mexico, with some projections added in and an attempt to ascertain whether there is a 7 year itch (more like 5 year itch).

Requirements
------------
* R
* Run "divorce-probabilities.r"

Data
-----
* divorce-data.csv: An csv file with data on the number of divorces in a given year as downloaded from the INEGI
* marriage-data.csv: An csv file with data on the number of marriages in a given year as downloaded from the INEGI
* marriage-rate.csv: Marriage rate per 1000 people with population data from CONAPO

Data Source: Website of the [INEGI](http://www.inegi.org.mx)


Output
-------
* A png chart of the marriage rate and divorce rate per 1000
* A png chart of the marriage rate and divorce rate per 1000 adults for historical comparison purposes
* A png chart with the proportion of marriages that end in divorce
![proportion of marriages that end in divorce](http://github.com/diegovalle/Divorce-Probabilities-MX/raw/master/output/Marriages%20Ending%20in%20Divorce,%20by%20Year%20of%20Marriage.png)
* A png chart with projections of the proportion of marriages that end in divorce
* A csv file (divorce-probability-lme.csv) that contains the proportion of marriages that end in divorce after a number of years
* A png chart of the average length of marriages that ended in divorce

Author
-----
[Diego Valle-Jones](www.diegovalle.net)

[1]: http://bpp.wharton.upenn.edu/jwolfers/Papers/MarriageandDivorce(JEP).pdf