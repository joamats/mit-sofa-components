import argparse
from tqdm import tqdm
import pandas as pd
import numpy as np
tqdm.pandas()

def parse_args():
    parser = argparse.ArgumentParser()

    parser.add_argument("--dataset",
                    default="eICU",
                    help="Insert the dataset to work with")

    return parser.parse_args()


def run_query(sql_query_path, destination_path):

    # Access data using Google BigQuery.
    import os
    from dotenv import load_dotenv

    # Load env file 
    load_dotenv()

    # Get GCP's secrets
    KEYS_FILE = os.getenv("KEYS_FILE")
    PROJECT_ID = os.getenv("PROJECT_ID")

    # Set environment variables
    os.environ['GOOGLE_APPLICATION_CREDENTIALS'] = KEYS_FILE

    # Establish connection with BigQuery
    from google.cloud import bigquery
    BigQuery_client = bigquery.Client()

    # Read query
    with open(sql_query_path, 'r') as fd:
        query = fd.read()

    # Replace the project id by the coder's project id in GCP
    my_query = query.replace("physionet-data", PROJECT_ID).replace("db_name", PROJECT_ID, -1)

    # Make request to BigQuery with our query
    df = BigQuery_client.query(my_query).to_dataframe()

    # Save to CSV
    df.to_csv(destination_path)

    return df


if __name__ == '__main__':

    args = parse_args()

    # First, let's fetch the data

    if args.dataset == "MIMIC":
        run_query("src/sql/MIMIC_cohort.sql",
                  "data/MIMIC_data.csv")

    elif args.dataset == "eICU":
        run_query("src/sql/eICU_cohort.sql",
                  "data/eICU_data.csv")