# R-Tafel-s-inclinations
R script to generate Tafel graphs and slopes



This script imports Excel, .csv, and .txt files; offers data pre-processing options, calculates Tafel slopes, and imports results into .tiff and text tables.

Warnings:
1) You must have the R language installed on your computer and an Internet connection, in case you need to install the necessary Rcran packages (readr and readxl);
2) To run Tafel for reduction processes, you need to make a precise selection of that region within the script;
3) At some point in the future, I may fix some features, such as the option to save graphs derived from Tafel, adding more intuitive menus, and performing Tafel as a function.

In any case, the script is functional as is and provides Tafel graphs with 'n' linear regressions along with their parameters;
it does not require any prior data processing and handles voltammograms with multiple cycles well.

Using RStudio makes it easier, but:

## Short tutorial on how to run in shell script:
```$ R -f <path of this script>```

## How to run in R enviroment
```source("<path of script>")```


The files saved by the script are stored in the working directory.
