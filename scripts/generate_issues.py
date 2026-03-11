#!/usr/bin/env python3
"""
Generate GitHub issues from PROJECT_BREAKDOWN.md with full descriptions
"""

import re
import subprocess
import time
import sys

def rate_limit():
    """Small delay to avoid API rate limiting"""
    time.sleep(0.6)

def create_issue(title, body, labels):
    """Create a GitHub issue and return its number"""
    rate_limit()

    try:
        result = subprocess.run(
            ['gh', 'issue', 'create', '--title', title, '--body', body, '--label', labels],
            capture_output=True,
            text=True,
            check=True
        )
        issue_url = result.stdout.strip()
        issue_number = issue_url.split('/')[-1]
        return issue_number
    except subprocess.CalledProcessError as e:
        print(f"Error creating issue: {e}")
        print(f"Stderr: {e.stderr}")
        return None

def parse_project_breakdown(filename='PROJECT_BREAKDOWN.md'):
    """Parse the PROJECT_BREAKDOWN.md file and extract structure"""

    with open(filename, 'r') as f:
        content = f.read()

    structure = {
        'pis': [],
        'epics': [],
        'stories': [],
        'tasks': []
    }

    # Parse PI Objectives
    pi_pattern = r'## PI OBJECTIVE (\d+): (.+?)\n\*\*Goal:\*\* (.+?)(?=\n###)'
    for match in re.finditer(pi_pattern, content, re.DOTALL):
        pi_num = match.group(1)
        pi_title = match.group(2).strip()
        pi_goal = match.group(3).strip()

        structure['pis'].append({
            'number': pi_num,
            'title': pi_title,
            'goal': pi_goal,
            'epics': []
        })

    # Parse Epics
    epic_pattern = r'### Epic (\d+\.\d+): (.+?)\n\*\*Description:\*\* (.+?)(?=\n####|### Epic|\n## PI|$)'
    for match in re.finditer(epic_pattern, content, re.DOTALL):
        epic_num = match.group(1)
        epic_title = match.group(2).strip()
        epic_desc = match.group(3).strip()

        pi_num = epic_num.split('.')[0]

        structure['epics'].append({
            'number': epic_num,
            'title': epic_title,
            'description': epic_desc,
            'pi': pi_num,
            'stories': []
        })

    # Parse Stories
    story_pattern = r'#### Story (\d+\.\d+\.\d+): (.+?)\n\*\*As a\*\* (.+?)\n\*\*I want\*\* (.+?)\n\*\*So that\*\* (.+?)(?=\n- \[ \]|\n####|### Epic|$)'
    for match in re.finditer(story_pattern, content, re.DOTALL):
        story_num = match.group(1)
        story_title = match.group(2).strip()
        user = match.group(3).strip()
        want = match.group(4).strip()
        so_that = match.group(5).strip()

        parts = story_num.split('.')
        epic_num = f"{parts[0]}.{parts[1]}"

        structure['stories'].append({
            'number': story_num,
            'title': story_title,
            'epic': epic_num,
            'user_story': {
                'as_a': user,
                'i_want': want,
                'so_that': so_that
            },
            'tasks': []
        })

    # Parse Tasks
    task_pattern = r'- \[ \] \*\*Task (\d+\.\d+\.\d+\.\d+):\*\* (.+?)(?=\n- \[ \] \*\*Task|\n####|\n###|\n##|$)'
    for match in re.finditer(task_pattern, content, re.DOTALL):
        task_num = match.group(1)
        task_desc = match.group(2).strip()

        parts = task_num.split('.')
        story_num = f"{parts[0]}.{parts[1]}.{parts[2]}"

        structure['tasks'].append({
            'number': task_num,
            'description': task_desc,
            'story': story_num
        })

    return structure

def get_labels_for_item(item_type, number, title, desc=''):
    """Determine labels based on item properties"""
    labels = [item_type]

    # Phase labels
    pi_num = number.split('.')[0]
    labels.append(f"phase-{pi_num}")

    # Component labels
    lower_text = (title + ' ' + desc).lower()
    if any(word in lower_text for word in ['crypto', 'blake', 'ed25519', 'vrf', 'kes']):
        labels.append('crypto')
    if any(word in lower_text for word in ['network', 'socket', 'mux', 'protocol', 'peer']):
        labels.append('network')
    if any(word in lower_text for word in ['ledger', 'transaction', 'utxo', 'block body']):
        labels.append('ledger')
    if any(word in lower_text for word in ['consensus', 'praos', 'leader', 'stake pool']):
        labels.append('consensus')

    # Special labels
    if any(word in lower_text for word in ['prove', 'proof', 'verify', 'formal']):
        labels.append('formal-verification')
    if any(word in lower_text for word in ['documentation', 'document']):
        labels.append('documentation')

    return ','.join(labels)

def generate_pi_body(pi):
    """Generate rich description for PI Objective"""
    # Find epics for this PI
    epic_list = []
    for epic in structure['epics']:
        if epic['pi'] == pi['number']:
            epic_list.append(f"- Epic {epic['number']}: {epic['title']}")

    epics_text = '\n'.join(epic_list) if epic_list else "- (Epics to be defined)"

    return f"""**Goal:** {pi['goal']}

## Epics in this PI Objective
{epics_text}

## Success Criteria
This PI objective will be complete when all epics are finished and verified.

## References
- [Project Breakdown](PROJECT_BREAKDOWN.md)
- [Cardano Blueprint](https://cardano-scaling.github.io/cardano-blueprint/)
- [Ouroboros Network Spec](https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf)
"""

def generate_epic_body(epic, pi_issue):
    """Generate rich description for Epic"""
    # Find stories for this epic
    story_list = []
    for story in structure['stories']:
        if story['epic'] == epic['number']:
            story_list.append(f"- Story {story['number']}: {story['title']}")

    stories_text = '\n'.join(story_list) if story_list else "- (Stories to be defined)"

    return f"""**Parent:** PI-{epic['pi']} (#{pi_issue})

## Description
{epic['description']}

## Stories in this Epic
{stories_text}

## Acceptance Criteria
This epic is complete when all stories are implemented and tested.

## References
- [Project Breakdown](PROJECT_BREAKDOWN.md)
"""

def generate_story_body(story, epic_issue, pi_issue):
    """Generate rich description for Story"""
    us = story['user_story']

    # Find tasks for this story
    task_list = []
    for task in structure['tasks']:
        if task['story'] == story['number']:
            task_list.append(f"- [ ] Task {task['number']}: {task['description']}")

    tasks_text = '\n'.join(task_list) if task_list else "- [ ] (Tasks to be defined)"

    return f"""**Parent:** Epic {story['epic']} (#{epic_issue}) | PI-{story['epic'].split('.')[0]} (#{pi_issue})

**As a** {us['as_a']}
**I want** {us['i_want']}
**So that** {us['so_that']}

## Tasks
{tasks_text}

## Acceptance Criteria
This story is complete when all tasks are finished and acceptance criteria are met.

## Definition of Done
- All tasks completed and checked off
- Code reviewed
- Tests passing
- Documentation updated
"""

def generate_task_body(task, story_issue, epic_issue, pi_issue):
    """Generate rich description for Task"""
    return f"""**Parent:** Story {task['story']} (#{story_issue}) | Epic {task['story'].rsplit('.', 1)[0]} (#{epic_issue}) | PI-{task['story'].split('.')[0]} (#{pi_issue})

## Description
{task['description']}

## Acceptance Criteria
This task is complete when the described work is implemented, tested, and reviewed.

## Implementation Notes
- Follow Lean 4 best practices
- Include appropriate tests
- Document public APIs
- Consider formal verification where applicable

## References
- [Project Breakdown](PROJECT_BREAKDOWN.md) - Task {task['number']}
"""

def main():
    print("=" * 60)
    print("Cleanode: Generate All GitHub Issues")
    print("=" * 60)
    print()

    # Parse PROJECT_BREAKDOWN.md
    print("Parsing PROJECT_BREAKDOWN.md...")
    global structure
    structure = parse_project_breakdown()

    print(f"Found:")
    print(f"  - {len(structure['pis'])} PI Objectives")
    print(f"  - {len(structure['epics'])} Epics")
    print(f"  - {len(structure['stories'])} Stories")
    print(f"  - {len(structure['tasks'])} Tasks")
    print()

    response = input("Create all these issues? (y/n): ")
    if response.lower() != 'y':
        print("Aborted")
        return

    print()
    print("Creating issues...")
    print()

    # Track created issues
    created_issues = {}

    # Create PI Objectives
    print("Creating PI Objectives...")
    for pi in structure['pis']:
        title = f"[PI-{pi['number']}] {pi['title']}"
        body = generate_pi_body(pi)
        labels = get_labels_for_item('pi-objective', pi['number'], pi['title'], pi['goal'])

        issue_num = create_issue(title, body, labels)
        created_issues[f"PI-{pi['number']}"] = issue_num
        print(f"  ✓ Created PI-{pi['number']}: #{issue_num}")

    print()

    # Create Epics
    print("Creating Epics...")
    for epic in structure['epics']:
        title = f"[Epic {epic['number']}] {epic['title']}"
        pi_issue = created_issues.get(f"PI-{epic['pi']}", '?')
        body = generate_epic_body(epic, pi_issue)
        labels = get_labels_for_item('epic', epic['number'], epic['title'], epic['description'])

        issue_num = create_issue(title, body, labels)
        created_issues[f"Epic-{epic['number']}"] = issue_num
        print(f"  ✓ Created Epic {epic['number']}: #{issue_num}")

    print()

    # Create Stories
    print("Creating Stories...")
    for story in structure['stories']:
        title = f"[Story {story['number']}] {story['title']}"
        epic_issue = created_issues.get(f"Epic-{story['epic']}", '?')
        pi_num = story['epic'].split('.')[0]
        pi_issue = created_issues.get(f"PI-{pi_num}", '?')
        body = generate_story_body(story, epic_issue, pi_issue)
        labels = get_labels_for_item('story', story['number'], story['title'])

        issue_num = create_issue(title, body, labels)
        created_issues[f"Story-{story['number']}"] = issue_num
        print(f"  ✓ Created Story {story['number']}: #{issue_num}")

    print()

    # Create Tasks
    print("Creating Tasks...")
    for i, task in enumerate(structure['tasks']):
        title = f"[Task {task['number']}] {task['description'][:60]}..."
        story_issue = created_issues.get(f"Story-{task['story']}", '?')
        epic_num = '.'.join(task['story'].split('.')[:2])
        epic_issue = created_issues.get(f"Epic-{epic_num}", '?')
        pi_num = task['story'].split('.')[0]
        pi_issue = created_issues.get(f"PI-{pi_num}", '?')
        body = generate_task_body(task, story_issue, epic_issue, pi_issue)
        labels = get_labels_for_item('task', task['number'], task['description'])

        issue_num = create_issue(title, body, labels)
        created_issues[f"Task-{task['number']}"] = issue_num
        print(f"  ✓ Created Task {task['number']}: #{issue_num} [{i+1}/{len(structure['tasks'])}]")

    print()
    print("=" * 60)
    print("✅ All issues created successfully!")
    print("=" * 60)
    print()
    print(f"Total issues created: {len(created_issues)}")
    print()
    print("View issues: https://github.com/RSoulatIOHK/Cleanode/issues")
    print()
    print("Next steps:")
    print("1. Link sub-issues in GitHub UI")
    print("2. Review and prioritize")
    print("3. Start implementation!")

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        print("\n\nAborted by user")
        sys.exit(1)
    except Exception as e:
        print(f"\nError: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
