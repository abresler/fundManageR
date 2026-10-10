#!/usr/bin/env zsh
# Daily ERISA Schedule of Assets PDF processing
# Run by launchd or cron — processes 100 NEW filings per run, resume-safe
# Year cycles through recent years on each invocation

set -euo pipefail

LOGDIR="$HOME/Desktop/data/_raw/dol_5500"
mkdir -p "$LOGDIR"
LOG="$LOGDIR/sched_of_assets_daily_$(date +%Y%m%d).log"

# Year rotation: latest first, fall back to older years if processed
YEARS=(2023 2022 2021 2020 2019 2018 2017)
MAX_FILINGS_PER_RUN=100

export PATH="/Library/Frameworks/R.framework/Versions/Current/Resources/bin:$PATH"

{
  echo "[$(date)] Daily ERISA Schedule of Assets run"
  for YEAR in "${YEARS[@]}"; do
    echo "--- Year $YEAR ---"
    YEAR=$YEAR MAX_FILINGS=$MAX_FILINGS_PER_RUN \
      Rscript "$HOME/Desktop/r_packages/fundManageR/scripts/run_schedule_of_assets_batch.R" 2>&1
  done
  echo "[$(date)] Done."
} | tee -a "$LOG"
