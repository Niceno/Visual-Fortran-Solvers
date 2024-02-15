#!/bin/bash

# Check if an argument was provided
if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <prefix>"
  exit 1
fi

# Use the first command-line argument as the prefix
prefix="$1"

echo "Converting .fig to .png files with prefix: $prefix"

# Count the number of .fig files using the provided prefix
total_files=$(ls ${prefix}*.fig | wc -l)

# Start processing each file, and for each file processed, echo a dot
for file in ${prefix}*.fig; do
  fig2dev -L png "$file" "${file%.fig}.png"
  echo -n '.'
done | pv -p -s ${total_files} > /dev/null

echo "Conversion complete."

echo "This command worked once:"

# Use the prefix in the ffmpeg command as well
echo "ffmpeg -framerate 5 -i ${prefix}_%05d.png -c:v libx264 -preset slow ${prefix}.avi"

# If vlc doesn't work, this might help the issue:
# Find and change setting
#   -> Tools
#     -> Preferences
#       -> Input/Codecs
#         -> Hardware-accelerated decoding from the default "Automatic" to
#            either "VA-API decoder" or "VA-API decoder via DRM" or "Disabled"
