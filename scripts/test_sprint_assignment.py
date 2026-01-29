#!/usr/bin/env python3
"""
Test sprint assignment on a single issue
"""

import subprocess
import json

def test_sprint_assignment():
    print("Testing sprint assignment on one issue...")
    print()

    # Step 1: Get project ID
    print("Step 1: Getting project ID...")
    project_query = """
    query($owner: String!, $number: Int!) {
      user(login: $owner) {
        projectV2(number: $number) {
          id
          title
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

    project_data = json.loads(result.stdout)
    project_id = project_data['data']['user']['projectV2']['id']
    project_title = project_data['data']['user']['projectV2']['title']
    print(f"  Project: {project_title}")
    print(f"  Project ID: {project_id}")
    print()

    # Step 2: Get Sprint field ID and iterations
    print("Step 2: Getting Sprint field...")
    field_query = """
    query($owner: String!, $number: Int!) {
      user(login: $owner) {
        projectV2(number: $number) {
          field(name: "Sprint") {
            ... on ProjectV2IterationField {
              id
              name
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
        ['gh', 'api', 'graphql', '-f', f'query={field_query}',
         '-F', 'owner=RSoulatIOHK', '-F', 'number=14'],
        capture_output=True,
        text=True,
        check=True
    )

    field_data = json.loads(result.stdout)
    field_info = field_data['data']['user']['projectV2']['field']
    field_id = field_info['id']
    iterations = field_info['configuration']['iterations']

    print(f"  Field ID: {field_id}")
    print(f"  Available sprints:")
    for it in iterations:
        print(f"    - {it['title']} (ID: {it['id']})")
    print()

    if not iterations:
        print("❌ No sprint iterations found! Please create them in GitHub UI first.")
        return

    # Step 3: Get first issue from project
    print("Step 3: Getting a test issue from project...")
    items_query = """
    query($owner: String!, $number: Int!) {
      user(login: $owner) {
        projectV2(number: $number) {
          items(first: 1) {
            nodes {
              id
              content {
                ... on Issue {
                  number
                  title
                }
              }
            }
          }
        }
      }
    }
    """

    result = subprocess.run(
        ['gh', 'api', 'graphql', '-f', f'query={items_query}',
         '-F', 'owner=RSoulatIOHK', '-F', 'number=14'],
        capture_output=True,
        text=True,
        check=True
    )

    items_data = json.loads(result.stdout)
    if not items_data['data']['user']['projectV2']['items']['nodes']:
        print("❌ No items found in project!")
        return

    test_item = items_data['data']['user']['projectV2']['items']['nodes'][0]
    item_id = test_item['id']
    issue_number = test_item['content']['number']
    issue_title = test_item['content']['title']

    print(f"  Test issue: #{issue_number}")
    print(f"  Title: {issue_title}")
    print(f"  Item ID: {item_id}")
    print()

    # Step 4: Assign to first sprint
    first_sprint = iterations[0]
    print(f"Step 4: Assigning to sprint '{first_sprint['title']}'...")

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

    result = subprocess.run(
        ['gh', 'api', 'graphql', '-f', f'query={mutation}',
         '-F', f'projectId={project_id}',
         '-F', f'itemId={item_id}',
         '-F', f'fieldId={field_id}',
         '-F', f'iterationId={first_sprint["id"]}'],
        capture_output=True,
        text=True
    )

    if result.returncode == 0:
        print(f"  ✅ Successfully assigned issue #{issue_number} to {first_sprint['title']}")
        print()
        print("Check the project board to verify the sprint was set!")
    else:
        print(f"  ❌ Failed to assign sprint")
        print(f"  Error: {result.stderr}")

    print()

if __name__ == '__main__':
    try:
        test_sprint_assignment()
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
