# Operational Research and Optimization Models Algorithms

## Deploy to GCP

___________

`gcloud config get-value project` 

___________


`gcloud auth login`

`gcloud config set project NAME-PROJECT`

`gcloud builds submit --tag gcr.io/NAME-PROJECT/NAME-PROJECT`

`gcloud beta run deploy NAME-PROJECT --image gcr.io/NAME-PROJECT/NAME-PROJECT --region us-central1 --platform managed`
