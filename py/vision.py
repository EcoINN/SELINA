import os
import pandas as pd
from google.cloud import vision
from google.cloud.vision_v1 import types
import time

# Set the environment variable for Google Cloud credentials
os.environ["GOOGLE_APPLICATION_CREDENTIALS"] = "C:/Ecostack/Projects/01_Selina/selina/py/selinadp-88bc54370d2c.json"

# Initialize the Vision API client
client = vision.ImageAnnotatorClient()

# Read the CSV file
csv_file = "C:/Ecostack/Projects/01_Selina/selina/extracted_image_urls.csv"
df = pd.read_csv(csv_file)

# Directory to save the output
output_dir = "C:/Ecostack/Projects/01_Selina/selina/output/URLS"
os.makedirs(output_dir, exist_ok=True)

# Function to analyze an image URL
def analyze_image_url(image_url):
    """Analyze the image at the given URL using Google Vision API and return the labels.

    Args:
        image_url (str): URL of the image to be analyzed.

    Returns:
        list: List of labels detected in the image along with their scores.
    """
    image = vision.Image()
    image.source.image_uri = image_url
    response = client.label_detection(image=image)
    labels = response.label_annotations
    return labels

# Analyze each image URL and save the results
results = []

for index, row in df.iterrows():
    image_url = row['image_url']
    try:
        print(f"Analyzing image URL: {image_url}")
        labels = analyze_image_url(image_url)
        labels_data = [(label.description, label.score) for label in labels]
        results.append({'image_url': image_url, 'labels': labels_data})
    except Exception as e:
        print(f"Error analyzing image URL {image_url}: {e}")
        results.append({'image_url': image_url, 'labels': []})
    time.sleep(0.5)  # Sleep to handle API rate limits

# Save the results to a new CSV file
output_file = os.path.join(output_dir, "image_analysis_results.csv")
results_df = pd.DataFrame(results)
results_df.to_csv(output_file, index=False)
print(f"Analysis results saved to {output_file}")
