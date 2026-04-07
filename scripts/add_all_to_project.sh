#!/bin/bash
# Add all issues to project 14

echo "Adding all issues to project..."

count=0
total=$(gh issue list --state open --limit 300 --json number --jq 'length')

echo "Found $total issues to add"

gh issue list --state open --limit 300 --json number --jq '.[].number' | while read issue; do
  count=$((count + 1))
  echo "[$count/$total] Adding issue #$issue..."
  gh project item-add 14 --owner RSoulatIOHK --url "https://github.com/RSoulatIOHK/Dion/issues/$issue" 2>&1 | grep -q "already exists" && echo "  (already exists)" || sleep 0.2
done

echo ""
echo "✅ Done!"
