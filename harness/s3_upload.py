import os
import boto3


def upload_directory(local_directory, bucket_name, destination):

    client = boto3.client('s3')

    if not os.path.exists(local_directory):
        print(f"Upload directory {os.path.abspath(local_directory)} does not exist. Not uploading.")
        return

    for root, dirs, files in os.walk(local_directory):
        for filename in files:
            local_path = os.path.join(root, filename)
            relative_path = os.path.relpath(local_path, local_directory)
            s3_path = os.path.join(destination, relative_path)
            print(f'Uploading {s3_path} to bucket {bucket_name}, path {destination}')
            client.upload_file(local_path, bucket_name, s3_path)


if __name__ == "__main__":
    upload_directory('PoC-for-FSKX-API-server/fskxserver/fskxserver/results', 'fskx-simulation-results-dev', '1234test')