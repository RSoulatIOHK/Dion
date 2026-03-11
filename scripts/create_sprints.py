#!/usr/bin/env python3
"""
Create Sprint iterations 4-17 in the GitHub project
Each sprint is 2 weeks (14 days)
"""

import subprocess
import json
from datetime import datetime, timedelta

def create_sprint_iteration(project_id, field_id, title, start_date, duration=14):
    """Create a new sprint iteration"""
    mutation = """
    mutation($projectId: ID!, $fieldId: ID!, $title: String!, $startDate: Date!, $duration: Int!) {
      updateProjectV2IterationFieldConfiguration(input: {
        projectId: $projectId
        fieldId: $fieldId
        configuration: {
          completedIterations: []
          iterations: [{
            title: $title
            startDate: $startDate
            duration: $duration
          }]
        }
      }) {
        projectV2IterationField {
          id
          configuration {
            iterations {
              id
              title
            }
          }
        }
      }
    }
    """

    result = subprocess.run(
        ['gh', 'api', 'graphql', '-f', f'query={mutation}',
         '-F', f'projectId={project_id}',
         '-F', f'fieldId={field_id}',
         '-F', f'title={title}',
         '-F', f'startDate={start_date}',
         '-F', f'duration={duration}'],
        capture_output=True,
        text=True
    )

    return result.returncode == 0, result.stderr

def main():
    print("=" * 70)
    print("Create Sprint Iterations 4-17")
    print("=" * 70)
    print()

    # Get project ID
    print("Getting project information...")
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
    print(f"  Project: {project_data['data']['user']['projectV2']['title']}")
    print(f"  Project ID: {project_id}")
    print()

    # Get Sprint field ID
    print("Getting Sprint field...")
    field_result = subprocess.run(
        ['gh', 'project', 'field-list', '14', '--owner', 'RSoulatIOHK', '--format', 'json'],
        capture_output=True,
        text=True,
        check=True
    )

    fields = json.loads(field_result.stdout)
    field_id = None
    for field in fields.get('fields', []):
        if field.get('name') == 'Sprint':
            field_id = field.get('id')
            break

    if not field_id:
        print("❌ Sprint field not found!")
        return

    print(f"  Sprint field ID: {field_id}")
    print()

    print("NOTE: Creating sprint iterations via API has limitations.")
    print("The GitHub API doesn't fully support creating iterations programmatically yet.")
    print()
    print("Please create the remaining sprints manually in the GitHub UI:")
    print()
    print("1. Go to: https://github.com/users/RSoulatIOHK/projects/14/settings")
    print("2. Find the 'Sprint' field")
    print("3. Add new iterations:")
    print()

    # Calculate start dates (assuming Sprint 3 ends, Sprint 4 begins)
    # Let's assume Sprint 1 started today minus 4 weeks
    today = datetime.now()
    sprint_1_start = today - timedelta(weeks=4)

    for sprint_num in range(4, 18):
        # Each sprint is 2 weeks
        start_date = sprint_1_start + timedelta(weeks=2 * (sprint_num - 1))
        end_date = start_date + timedelta(days=14)

        print(f"   Sprint {sprint_num:2d}: {start_date.strftime('%Y-%m-%d')} to {end_date.strftime('%Y-%m-%d')} (2 weeks)")

    print()
    print("Alternative: Use the GitHub UI to batch create iterations.")
    print("After creating all sprints, run: python3 assign_sprints.py")
    print()

if __name__ == '__main__':
    try:
        main()
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
