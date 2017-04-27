#! /bin/bash
# This script will download the files used by STA 160 Flight Project.

clear

echo "Hi, $USER!"
echo "This script will download the files used by STA 160 Flight Project."
echo "The files total approximately 1.2GiB and will download to your current directory."
read -p "Are you sure you want to continue? <y/N> " prompt
if [[ $prompt == "y" || $prompt == "Y" || $prompt == "yes" || $prompt == "Yes" ]]
then
	echo "Beginning files transfer..."
else
  exit 0
fi

echo "Transferring 1 of 3 files."
curl -S -# http://point.fungservices.com/git/flight/data.zip > data.zip
echo "Transferring 2 of 3 files."
curl -S -# http://point.fungservices.com/git/flight/pointone.csv > pointone.csv
echo "Transferring 3 of 3 files."
curl -S -# http://point.fungservices.com/git/flight/pointfive.csv > pointfive.csv
echo "Extracting files..."
unzip data.zip
echo "Cleaning up..."
rm data.zip
echo "Done!"



