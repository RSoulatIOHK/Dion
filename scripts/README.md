# Cleanode Scripts

This directory contains utility scripts for managing the Cleanode project's GitHub issues and project board.

## Issue Management Scripts

### `generate_issues.py`
**Main script for creating all GitHub issues from PROJECT_BREAKDOWN.md**

Creates all 242 issues (5 PI Objectives, 15 Epics, 38 Stories, 184 Tasks) with full descriptions, acceptance criteria, and proper labels.

```bash
python3 scripts/generate_issues.py
```

### `cleanup_issues.sh`
Closes all open GitHub issues before regenerating them.

```bash
./scripts/cleanup_issues.sh
```

### `create_labels.sh`
Creates all necessary GitHub labels for the project:
- Hierarchy: `pi-objective`, `epic`, `story`, `task`
- Phases: `phase-1` through `phase-5`
- Components: `crypto`, `network`, `ledger`, `consensus`
- Special: `formal-verification`, `documentation`

```bash
./scripts/create_labels.sh
```

## Project Board Scripts

### `assign_sprints.py`
**Main script for assigning all issues to sprints**

Assigns all 266 issues to their appropriate 2-week sprints (Sprints 1-17) based on dependencies and the project roadmap.

```bash
python3 scripts/assign_sprints.py
```

**Prerequisites:**
- All 17 sprint iterations must be created in the GitHub project first
- All issues must be added to the project

### `test_sprint_assignment.py`
Tests sprint assignment on a single issue to verify the API connection works.

```bash
python3 scripts/test_sprint_assignment.py
```

### `add_all_to_project.sh`
Adds all open issues to the GitHub project board.

```bash
./scripts/add_all_to_project.sh
```

### `create_sprints.py`
Provides guidance for creating sprint iterations in the GitHub project UI.

```bash
python3 scripts/create_sprints.py
```

## Legacy/Archive Scripts

These scripts were used during development and are kept for reference:

- `create_github_issues.sh` - Initial attempt at shell-based issue creation
- `create_remaining_issues.sh` - Continuation script for partial issue creation
- `create_all_issues.sh` - Early comprehensive issue creation script

## Typical Workflow

### Initial Setup
1. Create labels: `./scripts/create_labels.sh`
2. Generate all issues: `python3 scripts/generate_issues.py`
3. Create 17 sprint iterations in GitHub project UI
4. Assign issues to sprints: `python3 scripts/assign_sprints.py`

### Regenerating Issues
1. Clean up existing issues: `./scripts/cleanup_issues.sh`
2. Regenerate: `python3 scripts/generate_issues.py`
3. Re-assign to sprints: `python3 scripts/assign_sprints.py`

## Dependencies

All scripts require:
- `gh` (GitHub CLI) - authenticated and configured
- Python 3.7+ (for .py scripts)
- Bash (for .sh scripts)

## Project Structure

Issues are organized hierarchically:
- **PI Objectives** (5) - Program Increment goals spanning multiple sprints
- **Epics** (15) - Major bodies of work within a PI
- **Stories** (38) - User-facing features within an epic
- **Tasks** (184) - Granular implementation work for stories

## Sprint Organization

17 sprints × 2 weeks each = 34 weeks (~8 months)

- **Sprints 1-6**: Phase 1 - Foundation & Network Layer
- **Sprints 7-10**: Phase 2 - Block Processing
- **Sprints 11-12**: Phase 3 - Relay Node
- **Sprints 13-15**: Phase 4 - Block Production
- **Sprints 16-17**: Phase 5 - Advanced Features
