# SOFA Components' Predictive Impact on In-hospital Mortality

## How to run this project?

### 1. Get the Data!
Both MIMIC and eICU data can be found in [PhysioNet](https://physionet.org/), a repository of freely-available medical research data, managed by the MIT Laboratory for Computational Physiology. Due to its sensitive nature, credentialing is required to access both datasets.

Documentation for MIMIC-IV's can be found [here](https://mimic.mit.edu/) and for eICU [here](https://eicu-crd.mit.edu/).

#### Integration with Google Cloud Platform (GCP)

In this section, we explain how to set up GCP and your environment in order to run SQL queries through GCP right from your local Python setting. Follow these steps: 

1) Create a Google account if you don't have one and go to [Google Cloud Platform](https://console.cloud.google.com/bigquery)

2) Enable the [BigQuery API](https://console.cloud.google.com/apis/api/bigquery.googleapis.com)

3) Create a [Service Account](https://console.cloud.google.com/iam-admin/serviceaccounts), where you can download your JSON keys

4) Place your JSON keys in the parent folder (for example) of your project

5) Create a .env file with the command `cp env.example env `

6) Update your .env file with your ***JSON keys*** path and the ***id*** of your project in BigQuery

#### MIMIC-IV

After getting credentialing at PhysioNet, you must sign the data use agreement and connect the database with GCP, either asking for permission or uploading the data to your project. Please note that only MIMIC v2.0 is available at GCP.

Having all the necessary tables for the cohort generation query in your project, run the following command to fetch the data as a dataframe that will be saved as CSV in your local project. Make sure you have all required files and folders.


```sh
python3 src/py/get_data.py --dataset "MIMIC"
```

This will create the file `data/MIMIC_data.csv`

With the following command, you can get the same cohort we used for the study:

```sh
source("src/r/cohort_MIMIC.R")
```

This will create the files `data/cohorts/MIMIC_24.csv` and `data/cohorts/MIMIC_168.csv`.

#### eICU-CRD

The rationale for eICU-CRD is similar. Run the following commands:

```sh
python3 src/py/get_data.py --dataset "eICU"
```

This creates the file `data/eICU_data.csv`

```sh
source("src/r/cohort_eICU.R")
```
This creates the files `data/cohorts/eICU_24.csv` and `data/cohorts/eICU_168.csv`.

### 2. Run the Logistic Regression

We made it really easy for you in this part. All you have to do is:

```sh
source("src/r/model.R")
```

And you'll get the resulting odds ratios both for MIMIC and eICU, for both timepoints and all sensitivity analysis here: `results/glm`

## How to contribute?
We are actively working on this project.
Feel free to raise questions opening an issue, to fork this project and submit pull requests!
And here's my email if you prefer: jcmatos@mit.edu