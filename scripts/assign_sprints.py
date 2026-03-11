#!/usr/bin/env python3
"""
Assign Cleanode issues to 2-week sprints based on phase and dependencies
"""

import subprocess
import json
import time
import re

# Sprint mapping based on PROJECT_BREAKDOWN.md structure
SPRINT_MAPPING = {
    # Phase 1: Foundation & Network Layer (Sprints 1-6)
    "Sprint 1": [
        # Epic 1.1: Project Infrastructure & Cryptographic Primitives
        "PI-1", "Epic 1.1", "Story 1.1.1", "Story 1.1.2", "Story 1.1.3",
        "Task 1.1.1.", "Task 1.1.2.", "Task 1.1.3."
    ],
    "Sprint 2": [
        # Epic 1.2: CBOR Encoding/Decoding with Proofs
        "Epic 1.2", "Story 1.2.1", "Story 1.2.2", "Story 1.2.3",
        "Task 1.2.1.", "Task 1.2.2.", "Task 1.2.3."
    ],
    "Sprint 3": [
        # Epic 1.3: Network Multiplexer (TCP & Mux)
        "Epic 1.3", "Story 1.3.1", "Story 1.3.2",
        "Task 1.3.1.", "Task 1.3.2."
    ],
    "Sprint 4": [
        # Epic 1.3: Network Multiplexer (Handshake & KeepAlive)
        "Story 1.3.3", "Story 1.3.4",
        "Task 1.3.3.", "Task 1.3.4."
    ],
    "Sprint 5": [
        # Epic 1.4: Chain Sync Mini-Protocol (Protocol & State Machine)
        "Epic 1.4", "Story 1.4.1", "Story 1.4.2",
        "Task 1.4.1.", "Task 1.4.2."
    ],
    "Sprint 6": [
        # Epic 1.4: Chain Sync Mini-Protocol (Block Headers)
        "Story 1.4.3",
        "Task 1.4.3."
    ],

    # Phase 2: Block Processing (Sprints 7-10)
    "Sprint 7": [
        # Epic 2.1: Chain Observer Application
        "PI-2", "Epic 2.1",
        "Story 2.1.1", "Story 2.1.2", "Story 2.1.3",
        "Task 2.1.1.", "Task 2.1.2.", "Task 2.1.3."
    ],
    "Sprint 8": [
        # Epic 2.2: Block Fetch Mini-Protocol
        "Epic 2.2",
        "Story 2.2.1", "Story 2.2.2",
        "Task 2.2.1.", "Task 2.2.2."
    ],
    "Sprint 9": [
        # Epic 2.3: Transaction Parsing & Validation (Part 1)
        "Epic 2.3",
        "Story 2.3.1", "Story 2.3.2",
        "Task 2.3.1.", "Task 2.3.2."
    ],
    "Sprint 10": [
        # Epic 2.3: Transaction Parsing & Validation (Part 2)
        # Epic 2.4: Ledger State Management
        "Story 2.3.3", "Story 2.3.4",
        "Task 2.3.3.", "Task 2.3.4.",
        "Epic 2.4",
        "Story 2.4.1", "Story 2.4.2", "Story 2.4.3",
        "Task 2.4.1.", "Task 2.4.2.", "Task 2.4.3."
    ],

    # Phase 3: Relay Node (Sprints 11-12)
    "Sprint 11": [
        # Epic 3.1: Transaction Submission Mini-Protocol
        "PI-3", "Epic 3.1",
        "Story 3.1.1", "Story 3.1.2",
        "Task 3.1.1.", "Task 3.1.2."
    ],
    "Sprint 12": [
        # Epic 3.2: Peer-to-Peer Networking
        "Epic 3.2",
        "Story 3.2.1", "Story 3.2.2",
        "Task 3.2.1.", "Task 3.2.2."
    ],

    # Phase 4: Block Production (Sprints 13-15)
    "Sprint 13": [
        # Epic 4.1: Ouroboros Praos Consensus (VRF)
        "PI-4", "Epic 4.1",
        "Story 4.1.1",
        "Task 4.1.1."
    ],
    "Sprint 14": [
        # Epic 4.1: Ouroboros Praos Consensus (KES & Forging)
        "Story 4.1.2", "Story 4.1.3",
        "Task 4.1.2.", "Task 4.1.3."
    ],
    "Sprint 15": [
        # Epic 4.2: Stake Pool Operations
        "Epic 4.2",
        "Story 4.2.1", "Story 4.2.2",
        "Task 4.2.1.", "Task 4.2.2."
    ],

    # Phase 5: Advanced Features (Sprints 16-17)
    "Sprint 16": [
        # Epic 5.1: Mithril Integration
        # Epic 5.2: Performance Optimization
        "PI-5", "Epic 5.1", "Epic 5.2",
        "Story 5.1.1", "Story 5.2.1",
        "Task 5.1.1.", "Task 5.2.1."
    ],
    "Sprint 17": [
        # Epic 5.3: Monitoring & Operations
        "Epic 5.3",
        "Story 5.3.1", "Story 5.3.2",
        "Task 5.3.1.", "Task 5.3.2."
    ]
}

def get_all_issues():
    """Fetch all open issues from the repository"""
    result = subprocess.run(
        ['gh', 'issue', 'list', '--state', 'open', '--limit', '300', '--json', 'number,title'],
        capture_output=True,
        text=True,
        check=True
    )
    return json.loads(result.stdout)

def determine_sprint(title):
    """Determine which sprint an issue belongs to based on its title"""
    for sprint, patterns in SPRINT_MAPPING.items():
        for pattern in patterns:
            if pattern in title:
                return sprint
    return None

def get_project_items():
    """Get all items in the project with their IDs (with pagination)"""
    items = {}
    has_next_page = True
    end_cursor = None

    while has_next_page:
        # Build query with pagination
        cursor_param = f', after: "{end_cursor}"' if end_cursor else ''
        query = f"""
        query {{
          user(login: "RSoulatIOHK") {{
            projectV2(number: 14) {{
              id
              items(first: 100{cursor_param}) {{
                nodes {{
                  id
                  content {{
                    ... on Issue {{
                      number
                      title
                    }}
                  }}
                }}
                pageInfo {{
                  hasNextPage
                  endCursor
                }}
              }}
            }}
          }}
        }}
        """

        result = subprocess.run(
            ['gh', 'api', 'graphql', '-f', f'query={query}'],
            capture_output=True,
            text=True,
            check=True
        )

        data = json.loads(result.stdout)

        if data.get('data', {}).get('user', {}).get('projectV2'):
            project = data['data']['user']['projectV2']

            # Add items from this page
            for item in project['items']['nodes']:
                if item.get('content'):
                    issue_num = item['content'].get('number')
                    if issue_num:
                        items[issue_num] = {
                            'id': item['id'],
                            'title': item['content'].get('title', '')
                        }

            # Check pagination
            page_info = project['items']['pageInfo']
            has_next_page = page_info['hasNextPage']
            end_cursor = page_info['endCursor']
        else:
            break

    return items

def get_sprint_field_id():
    """Get the Sprint field ID from the project"""
    result = subprocess.run(
        ['gh', 'project', 'field-list', '14', '--owner', 'RSoulatIOHK', '--format', 'json'],
        capture_output=True,
        text=True,
        check=True
    )

    fields = json.loads(result.stdout)
    for field in fields.get('fields', []):
        if field.get('name') == 'Sprint':
            return field.get('id')

    return None

def get_sprint_iterations(field_id):
    """Get available sprint iterations"""
    # GraphQL query to get iterations
    query = """
    query($owner: String!, $number: Int!) {
      user(login: $owner) {
        projectV2(number: $number) {
          field(name: "Sprint") {
            ... on ProjectV2IterationField {
              id
              configuration {
                iterations {
                  id
                  title
                  startDate
                  duration
                }
              }
            }
          }
        }
      }
    }
    """

    result = subprocess.run(
        ['gh', 'api', 'graphql', '-f', f'query={query}',
         '-F', 'owner=RSoulatIOHK', '-F', 'number=14'],
        capture_output=True,
        text=True,
        check=True
    )

    data = json.loads(result.stdout)
    iterations = {}

    field_data = data.get('data', {}).get('user', {}).get('projectV2', {}).get('field')
    if field_data and field_data.get('configuration', {}).get('iterations'):
        for iteration in field_data['configuration']['iterations']:
            iterations[iteration['title']] = iteration['id']

    return iterations

def update_sprint(project_item_id, field_id, iteration_id, project_id):
    """Update the sprint field for a project item"""
    mutation = """
    mutation($projectId: ID!, $itemId: ID!, $fieldId: ID!, $iterationId: String!) {
      updateProjectV2ItemFieldValue(input: {
        projectId: $projectId
        itemId: $itemId
        fieldId: $fieldId
        value: {
          iterationId: $iterationId
        }
      }) {
        projectV2Item {
          id
        }
      }
    }
    """

    # Update the item
    result = subprocess.run(
        ['gh', 'api', 'graphql', '-f', f'query={mutation}',
         '-F', f'projectId={project_id}',
         '-F', f'itemId={project_item_id}',
         '-F', f'fieldId={field_id}',
         '-F', f'iterationId={iteration_id}'],
        capture_output=True,
        text=True
    )

    return result.returncode == 0

def main():
    print("=" * 70)
    print("Cleanode: Assign Issues to Sprints")
    print("=" * 70)
    print()

    # Get project ID
    print("Getting project ID...")
    project_query = """
    query($owner: String!, $number: Int!) {
      user(login: $owner) {
        projectV2(number: $number) {
          id
        }
      }
    }
    """
    result = subprocess.run(
        ['gh', 'api', 'graphql', '-f', f'query={project_query}',
         '-F', 'owner=RSoulatIOHK', '-F', 'number=14'],
        capture_output=True,
        text=True,
        check=True
    )
    project_id = json.loads(result.stdout)['data']['user']['projectV2']['id']
    print(f"Project ID: {project_id}")
    print()

    # Get all issues
    print("Fetching issues...")
    issues = get_all_issues()
    print(f"Found {len(issues)} issues")
    print()

    # Get project items
    print("Fetching project items...")
    project_items = get_project_items()
    print(f"Found {len(project_items)} items in project")
    print()

    # Get sprint field ID
    print("Getting Sprint field...")
    field_id = get_sprint_field_id()
    if not field_id:
        print("ERROR: Sprint field not found!")
        return
    print(f"Sprint field ID: {field_id}")
    print()

    # Get sprint iterations
    print("Getting sprint iterations...")
    iterations = get_sprint_iterations(field_id)
    if not iterations:
        print("ERROR: No sprint iterations found!")
        print("Please create sprints in the GitHub project UI first.")
        print()
        print("Recommended sprints to create:")
        for sprint in sorted(SPRINT_MAPPING.keys(), key=lambda x: int(x.split()[1])):
            print(f"  - {sprint}")
        return

    print(f"Found {len(iterations)} sprint iterations:")
    for name in sorted(iterations.keys(), key=lambda x: int(x.split()[1]) if 'Sprint' in x else 0):
        print(f"  - {name}")
    print()

    # Assign issues to sprints
    print("Assigning issues to sprints...")
    print()

    stats = {sprint: 0 for sprint in SPRINT_MAPPING.keys()}
    stats['No Sprint'] = 0

    for issue in issues:
        issue_num = issue['number']
        title = issue['title']

        # Determine sprint
        sprint_name = determine_sprint(title)

        if not sprint_name:
            stats['No Sprint'] += 1
            print(f"  ⚠️  #{issue_num}: {title[:60]}... (No sprint assigned)")
            continue

        if sprint_name not in iterations:
            print(f"  ⚠️  #{issue_num}: {title[:60]}... ({sprint_name} not created yet)")
            stats['No Sprint'] += 1
            continue

        # Get project item ID
        if issue_num not in project_items:
            print(f"  ⚠️  #{issue_num}: {title[:60]}... (Not in project)")
            continue

        project_item_id = project_items[issue_num]['id']
        iteration_id = iterations[sprint_name]

        # Update sprint
        success = update_sprint(project_item_id, field_id, iteration_id, project_id)

        if success:
            stats[sprint_name] += 1
            print(f"  ✓ #{issue_num}: {title[:50]}... → {sprint_name}")
        else:
            print(f"  ✗ #{issue_num}: {title[:50]}... (Update failed)")

        time.sleep(0.3)  # Rate limiting

    print()
    print("=" * 70)
    print("Summary")
    print("=" * 70)
    for sprint in sorted(SPRINT_MAPPING.keys(), key=lambda x: int(x.split()[1])):
        count = stats.get(sprint, 0)
        print(f"  {sprint}: {count} issues")

    if stats['No Sprint'] > 0:
        print(f"  No Sprint: {stats['No Sprint']} issues")

    print()
    print("✅ Sprint assignment complete!")
    print()

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        print("\n\nAborted by user")
    except Exception as e:
        print(f"\nError: {e}")
        import traceback
        traceback.print_exc()
