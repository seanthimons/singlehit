#!/usr/bin/env bash
# Render every preferred-model overview variant: data-raw/plot-preferred-models-all.sh workbook.xlsx out_dir
set -euo pipefail
w=$1; out=${2:-dev/preferred-models}; mkdir -p "$out"
r() { Rscript data-raw/plot-preferred-models.R "$w" "$out/$1.png" "${@:2}"; }
r 01-all-pathogens
for n in 3 4; do
  r "02-by-outcome-$n"       --facet-outcome       --outcomes=$n
  r "03-group-x-outcome-$n"  --facet-group-outcome --outcomes=$n
  r "04-taxa-x-outcome-$n"   --facet-taxa-outcome  --outcomes=$n
  r "05-taxa-by-genus-$n"    --facet-taxa-genus    --outcomes=$n
done
