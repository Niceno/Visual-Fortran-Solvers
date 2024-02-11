#!/bin/bash

echo "Converting .fig to .png files"

# Count the number of .fig files
total_files=$(ls factorization*.fig | wc -l)

# Start processing each file, and for each file processed, echo a dot
for file in factorization*.fig; do
  fig2dev -L png "$file" "${file%.fig}.png"
  echo -n '.'
done | pv -p -s ${total_files} > /dev/null

echo "Conversion complete."

echo "This command worked once:"

echo "ffmpeg -framerate 25 -i factorization_%05d.png -c:v libx264 -preset slow output.avi"

