# GitLab CI/CD Variables Configuration

## Required Variables

Set these in GitLab: **Settings > CI/CD > Variables**

### Deployment Variables
- `KUBECONFIG_STAGING` - Kubernetes config for staging (Type: File, Protected)
- `KUBECONFIG_PRODUCTION` - Kubernetes config for production (Type: File, Protected)
- `STAGING_URL` - Staging environment URL (e.g., https://staging.tree-navigator.io)
- `PRODUCTION_URL` - Production environment URL (e.g., https://tree-navigator.io)

### Optional Variables
- `SLACK_WEBHOOK` - Slack webhook for notifications
- `DATADOG_API_KEY` - Datadog API key for monitoring
- `NEW_RELIC_LICENSE_KEY` - New Relic license key

## GitLab Container Registry

The pipeline automatically pushes to GitLab Container Registry:
```
registry.gitlab.com/your-group/tree-navigator
```

Images are tagged with:
- `latest` (main branch)
- `${CI_COMMIT_SHORT_SHA}` (every commit)
- `${CI_COMMIT_TAG}` (tags only)

## Scheduled Pipelines

Create scheduled pipelines in **CI/CD > Schedules**:

### Nightly Build
- Description: Nightly build and test
- Interval: 0 2 * * * (2 AM daily)
- Target branch: main
- Variables: none

### Weekly Dependency Audit
- Description: Check for dependency updates
- Interval: 0 0 * * 0 (Sunday midnight)
- Target branch: main
- Variables: none

## Protected Branches

Configure in **Settings > Repository > Protected branches**:
- `main` - Maintainers can push, Developers can merge
- `production` - No one can push, Maintainers can merge

## Environments

Configure in **Deployments > Environments**:
- **staging** - Auto-stop after 1 week
- **production** - Protected, requires approval
