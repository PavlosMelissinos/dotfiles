#!/bin/bash
# U2F Monitoring Script - Can be run periodically to check U2F health
# Usage: u2f-monitor.sh [--cron] [--notify]

set -euo pipefail

SCRIPT_DIR="$(dirname "$0")"
CRON_MODE=false
NOTIFY_MODE=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --cron)
            CRON_MODE=true
            shift
            ;;
        --notify)
            NOTIFY_MODE=true
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [--cron] [--notify]"
            echo "  --cron    Run in cron mode (minimal output)"
            echo "  --notify  Send desktop notifications for failures"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Run the test suite
if [[ "$CRON_MODE" == true ]]; then
    # In cron mode, capture output and only show failures
    TEMP_LOG=$(mktemp)
    if "$SCRIPT_DIR/u2f-test-suite.sh" > "$TEMP_LOG" 2>&1; then
        # Tests passed - only log timestamp
        echo "$(date): U2F tests passed" >> "$HOME/.cache/u2f-monitor.log"
    else
        # Tests failed - log details
        echo "$(date): U2F tests FAILED" >> "$HOME/.cache/u2f-monitor.log"
        cat "$TEMP_LOG" >> "$HOME/.cache/u2f-monitor.log"
        
        if [[ "$NOTIFY_MODE" == true ]] && command -v notify-send >/dev/null 2>&1; then
            notify-send "U2F Authentication Issue" "U2F tests failed. Check ~/.cache/u2f-monitor.log" --urgency=critical
        fi
        
        exit 1
    fi
    rm -f "$TEMP_LOG"
else
    # Interactive mode - show full output
    "$SCRIPT_DIR/u2f-test-suite.sh"
fi