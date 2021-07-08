#!/usr/bin/env bash
ROOT=`stack path --project-root`

RENDER_OPTIONS='--compile --fps 60 --width 640'
ANIMATIONS='tut_glue_svg
  tut_glue_latex
  tut_glue_animate
  tut_glue_keyframe
  tut_glue_blender
  tut_glue_fourier
  tut_glue_physics
  tut_glue_potrace
  tut_glue_povray
  tut_glue_povray_ortho'

B2_DIR=$ROOT/.b2
B2_LISTING=$B2_DIR/directory.json
mkdir --parents $B2_DIR

b2 ls --json --long reanimate > $B2_LISTING
cd $ROOT/examples/
for BASE in $ANIMATIONS; do
  SRC=$ROOT/examples/$BASE.hs
  DST=$B2_DIR/$BASE.mp4
  echo $DST $SRC
  if [[ "$SRC" -nt "$DST" ]]; then
    #echo "stack $src render -o $DST $RENDER_OPTIONS"
    stack $SRC render -o $DST $RENDER_OPTIONS
  fi
  SHA=($(sha1sum $DST))
  FILE_INFO=$(jq '.[] | select(.contentSha1=="'$SHA'") | .fileName' $B2_LISTING)
  if [ -z "$FILE_INFO" ]; then
    echo "Uploading $BASE..."
    b2 upload-file --quiet --sha1 $SHA reanimate $DST $BASE.mp4
  fi
done
