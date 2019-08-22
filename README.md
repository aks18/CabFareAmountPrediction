# CabFareAmountPrediction
Project to predict cab fare amount based on pickup &amp; drop coordinates &amp; number of passengers. Number of models are built in this. LR, DT, RF, EXTRA, KNN, SVM, NN.

## Must include your own gmaps_api_key into the code to run it.

# Instructions to run the code
1. The files will be given in a zipped format having
> Python Project folder
> R Project folder
> Project report (PDF)
> This ReadMe file (PDF)
2. For Python (5 files are mandatory to run successfully)
The folder should & must have the following files, which are mandatory for the code to successfully
run
> ​ Cab Rental Project - Python - Full.ipynb
> ​ train_cab.csv
> ​ test.csv
> ​ gmaps_data.csv​ - contains GoogleMaps Distance Matrix API responses for processed training
dataset stored in this file. You can run the code mentioned in the notebook by uncommenting it, but
you will need an API key and it will take over 1 hour to fetch all the 15,500+ coordinates queries.
> ​ gmaps_data_test.csv​ - contains GoogleMaps Distance Matrix API responses for processed test
dataset stored in this file.
All these files should be present at the same file path (in the same folder) to avoid file path errors
while reading.
All the necessary libraries are coded into the Jupyter notebook and will be installed as the code runs.
Built on ​ Python 3
3. For R (5 files are mandatory to run successfully)
The folder should & must have the following files, which are mandatory for the code to successfully
run
> ​ Cab Rental Project.R
> ​ train_cab.csv
> ​ test.csv
> ​ gmaps_data.csv​ - contains GoogleMaps Distance Matrix API responses for processed training
dataset stored in this file. You can run the code mentioned in the notebook by uncommenting it, but
you will need an API key and it will take over 1 hour to fetch all the 15,500+ coordinates queries.
> ​ gmaps_data_test.csv​ - contains GoogleMaps Distance Matrix API responses for processed test
dataset stored in this file.
All these files should be present at the same file path (in the same folder) to avoid file path errors
while reading.
All the necessary libraries are coded into the R script and will be installed as the code runs. Set the
working directory correctly before running the script.
Built on ​ R 3.4.3
There should be no errors as the code has been tested at least 5 times. If any error arises, it has to be
because of incorrect file paths.
