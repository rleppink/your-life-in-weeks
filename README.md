# Your Life In Weeks generator

**Still under construction**

Generate a ['your life in weeks'](https://waitbutwhy.com/2014/05/life-weeks.html)
image for your life by providing your birthday. Optionally provide the end date,
total years or weeks.

## Flags
### --start-date, -s
The start date to calculate the weeks from. [Mandatory. No default.]

### --end-date, -e
The end date to calculate the weeks from. [Optional. Default: weeks to be 90 years of age.]

### --years, -y
The total number of years to calculate the weeks from. [Optional. Default: weeks to be 90 years of age.]

### --weeks, -w
The total number of weeks to calculate the weeks from. [Optional. Default: weeks to be 90 years of age.]

### --lineLength, -l
The amount of weeks to display per line. [Optional. Default: 52, a year]

### --yearStart, -y
Start the diagram at the start of the start date's year, instead of at the start date. [Optional. Default: False]


## Usage examples
`yliw "12-03-1989"` 
Generate the image using default settings for birthdate 12-03-1989.

**TODO**
