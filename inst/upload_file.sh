#!/bin/bash

############################################################
# UPLOAD_FILE
#
# Upload objects to database. A workaround as I'm having
# issues with Python versions and installed modules.
#
############################################################

# Load Python version with necessary packages installed
module purge
ml Python/3.9.5-GCCcore-10.3.0

# Extract inputs
py_file=$1  # Python file to execute
up_file=$2  # File to upload
sv_file=$3  # File name to save to
creds=$4    # Database credentials

# Call appropriate version of Python
python $py_file $up_file $sv_file $creds

