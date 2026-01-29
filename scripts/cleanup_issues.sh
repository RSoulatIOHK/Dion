#!/bin/bash
# Clean up all existing GitHub issues before recreating them

set -e

echo "========================================"
echo "Cleanup: Close All Existing Issues"
echo "========================================"
echo ""

# Get all open issues
open_issues=$(gh issue list --state open --json number --jq '.[].number')

if [ -z "$open_issues" ]; then
    echo "No open issues found."
else
    issue_count=$(echo "$open_issues" | wc -l | tr -d ' ')
    echo "Found $issue_count open issues to close"
    echo ""

    read -p "Close all these issues? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted"
        exit 1
    fi

    echo ""
    echo "Closing issues..."
    for issue in $open_issues; do
        echo "  Closing #$issue..."
        gh issue close "$issue" -c "Closing to recreate with comprehensive descriptions and proper hierarchy" || true
        sleep 0.3
    done
    echo ""
    echo "✓ Closed $issue_count issues"
fi

echo ""
echo "Cleanup complete!"
echo ""
