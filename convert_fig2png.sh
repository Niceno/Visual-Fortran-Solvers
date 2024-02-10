#!/bin/bash

echo "Converting .fig to .png files"

# Count the number of .fig files
total_files=$(ls g*.fig | wc -l)

# Start processing each file, and for each file processed, echo a dot
for file in g*.fig; do
  fig2dev -L png "$file" "${file%.fig}.png"
  echo -n '.'
done | pv -p -s ${total_files} > /dev/null

echo "Conversion complete."

echo "Creating filelist.txt for ffmpeg"
ls *.png | sort > filelist.txt
sed -i "s/.*/file '&'/g" filelist.txt

