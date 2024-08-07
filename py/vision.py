"""
Image Analysis Script

This script performs image analysis using the Google Vision API on a list of image URLs provided in a CSV file.
The analysis results are logged and saved in multiple CSV files. The script performs the following steps:

1. Loads configuration settings from a YAML file.
2. Sets up logging to track the script's progress and errors.
3. Reads image URLs from a CSV file specified in the configuration.
4. Processes images in batches, sending requests to the Google Vision API to analyze each image.
5. Handles retries and waits between requests to avoid exceeding API quotas.
6. Saves intermediate batch results and final results to CSV files.
7. Reshapes the results to include separate columns for each label and score, and saves the final results to a CSV file.

The file paths and configuration settings are loaded from environment variables to enhance security and flexibility.

Author: Ecostack Innovations
Date: July 2024
"""

import os
import pandas as pd
import logging
import time
import yaml
import requests

# Load configuration from YAML file
config_path = os.getenv('CONFIG_PATH', 'config.yaml')
with open(config_path, "r") as config_file:
    config = yaml.safe_load(config_file)

# Set up logging
log_file_path = os.path.join(config['output_dir'], 'image_analysis.log')
logging.basicConfig(
    filename=log_file_path,
    level=logging.INFO,
    format='%(asctime)s %(levelname)s:%(message)s'
)

# Read the API key from the configuration
api_key = config['api_key']

# Read the CSV file
input_csv_path = os.getenv('INPUT_CSV_PATH', 'input_images.csv')
df = pd.read_csv(input_csv_path)

# Ensure the output directory exists
os.makedirs(config['output_dir'], exist_ok=True)

def analyze_image_url(image_url):
    """Analyze the image at the given URL using Google Vision API and return labels with scores."""
    try:
        vision_api_url = f"https://vision.googleapis.com/v1/images:annotate?key={api_key}"
        request_body = {
            "requests": [
                {
                    "image": {
                        "source": {
                            "imageUri": image_url
                        }
                    },
                    "features": [
                        {
                            "type": "LABEL_DETECTION"
                        }
                    ]
                }
            ]
        }
        response = requests.post(vision_api_url, json=request_body)
        response_json = response.json()

        if 'error' in response_json:
            error_message = response_json['error'].get('message', 'Unknown error')
            logging.error(f"Error in API response for {image_url}: {error_message}")
            return []

        labels = response_json['responses'][0].get('labelAnnotations', [])
        labels_data = [(label['description'], label['score']) for label in labels]
        return labels_data
    except Exception as e:
        logging.error(f"Exception analyzing image URL {image_url}: {e}")
        return []

def process_batch(batch):
    """Process a batch of image URLs and return results with labels and scores."""
    results = []
    for index, row in batch.iterrows():
        image_url = row['image_url']
        retries = 0
        labels_data = []
        while retries < config['max_retries']:
            labels_data = analyze_image_url(image_url)
            if labels_data:
                break
            retries += 1
            time.sleep(config['sleep_time'])

        results.append({'image_url': image_url, 'labels': labels_data})
    return results

# Process URLs in batches
batch_size = config['batch_size']
num_batches = len(df) // batch_size + (1 if len(df) % batch_size > 0 else 0)

all_results = []

for batch_num in range(num_batches):
    start_index = batch_num * batch_size
    end_index = min((batch_num + 1) * batch_size, len(df))
    batch = df[start_index:end_index]
    batch_results = process_batch(batch)
    all_results.extend(batch_results)

    # Save intermediate results
    intermediate_output_file = os.path.join(config['output_dir'], f"image_analysis_results_batch_{batch_num}.csv")
    pd.DataFrame(batch_results).to_csv(intermediate_output_file, index=False)
    logging.info(f"Batch {batch_num} results saved to {intermediate_output_file}")

def reshape_results(all_results):
    """Reshape results to desired format with separate columns for each label and score."""
    final_results = []
    max_labels = 0  # Track maximum number of labels encountered
    for result in all_results:
        image_url = result['image_url']
        labels = [label for label, _ in result['labels']]  # Extract just labels
        scores = [str(score) for _, score in result['labels']]  # Extract and convert scores to strings
        max_labels = max(max_labels, len(labels))  # Update maximum labels

    # Create column names dynamically based on max_labels
    label_cols = [f'label_{i+1}' for i in range(max_labels)]
    score_cols = [f'score_{i+1}' for i in range(max_labels)]
    all_cols = ['image_url'] + label_cols + score_cols

    for result in all_results:
        image_url = result['image_url']
        labels = [label for label, _ in result['labels']]  # Extract just labels
        scores = [str(score) for _, score in result['labels']]  # Extract and convert scores to strings
        # Pad labels and scores with empty strings to match max_labels
        labels += [''] * (max_labels - len(labels))
        scores += [''] * (max_labels - len(scores))
        final_results.append(dict(zip(all_cols, [image_url] + labels + scores)))
    return final_results

# Reshape data
all_results = reshape_results(all_results)

# Convert to DataFrame and save
results_df = pd.DataFrame(all_results)
final_output_file = os.path.join(config['output_dir'], "image_analysis_results.csv")
results_df.to_csv(final_output_file, index=False)
logging.info(f"All results saved to {final_output_file}")

print(f"Analysis completed. Results saved to {final_output_file}")