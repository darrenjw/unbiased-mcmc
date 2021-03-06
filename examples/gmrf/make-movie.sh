#!/bin/sh
# make-movie.sh

rm -f frame-????-s.png

for name in frame-????.png
do
  short="${name%.*}"
  echo $short
  #pngtopnm "$name" | pnmscale 20 | pnmtopng > "${short}-s.png"
  #convert "$name" -scale 1200x600 -define png:color-type=2 "${short}-s.png"
  convert "$name" -define png:color-type=2 "${short}-s.png"
done

rm -f movie.mp4

#avconv -r 20 -i siv-%06d-s.png movie.mp4
#ffmpeg -f image2 -r 10 -pattern_type glob -i 'frame-*-s.png' movie.mp4
ffmpeg -f image2 -r 5 -pattern_type glob -i 'frame-????.png' movie.mp4

# make a version that should play on Android devices...
ffmpeg -i movie.mp4 -codec:v libx264 -profile:v main -preset slow -b:v 400k -maxrate 400k -bufsize 800k -vf scale=-1:480 -threads 0 -codec:a libfdk_aac -b:a 128k -pix_fmt yuv420p movie-a.mp4

# Animated GIF...
# ffmpeg -i movie.mp4 -s 200x100 movie.gif


# eof



