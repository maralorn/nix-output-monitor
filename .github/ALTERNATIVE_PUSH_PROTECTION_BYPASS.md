# Alternative GitHub Actions Configuration for Push Protection Bypass

If the main solution in `pull-from-forgejo.yaml` doesn't work due to strict branch protection rules, here's an alternative approach using a GitHub App or Personal Access Token.

## Option 1: Using GitHub App (Most Secure)

1. Create a GitHub App with `Contents: Write` permission
2. Install the app on the repository
3. Create a private key for the app
4. Add the following secrets to the repository:
   - `APP_ID`: The GitHub App ID
   - `APP_PRIVATE_KEY`: The private key content

Then use this workflow configuration:

```yaml
name: Pull from code.maralorn.de (GitHub App)
on:
  schedule:
    - cron: '0 * * * *'
  workflow_dispatch:

jobs:
  update:
    runs-on: ubuntu-latest

    steps:
      - name: Generate token
        id: generate_token
        uses: tibdex/github-app-token@v1
        with:
          app_id: ${{ secrets.APP_ID }}
          private_key: ${{ secrets.APP_PRIVATE_KEY }}

      - uses: actions/checkout@v4
        with:
          token: ${{ steps.generate_token.outputs.token }}
          fetch-depth: 0

      - name: Configure Git
        run: |
          git config user.name "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"

      - name: Pull main if fast-forward
        run: |
          echo "Current branch: $(git branch --show-current)"
          echo "Current commit: $(git rev-parse HEAD)"
          
          # Add remote if it doesn't exist
          if ! git remote get-url forgejo >/dev/null 2>&1; then
            git remote add forgejo https://code.maralorn.de/maralorn/nix-output-monitor.git
          fi
          
          # Fetch latest changes from forgejo
          git fetch forgejo main
          
          # Check if fast-forward is possible
          if git merge-base --is-ancestor HEAD forgejo/main; then
            echo "Fast-forward merge possible"
            git merge --ff-only forgejo/main
            git push origin main
            echo "Successfully updated main branch"
          else
            echo "Fast-forward not possible - manual intervention required"
            exit 1
          fi
```

## Option 2: Using Personal Access Token

1. Create a Personal Access Token with `repo` scope
2. Add it as a repository secret named `BYPASS_TOKEN`
3. Update the checkout action:

```yaml
- uses: actions/checkout@v4
  with:
    token: ${{ secrets.BYPASS_TOKEN }}
    fetch-depth: 0
```

## Repository Settings for Any Approach

In addition to the workflow changes, configure the repository settings:

1. Go to **Settings** → **Branches**
2. Find the branch protection rule for `main`
3. Under **Restrict pushes that create files**, add exceptions for:
   - `github-actions[bot]` (for GITHUB_TOKEN approach)
   - Your GitHub App name (for GitHub App approach)
   - Your username (for PAT approach)

This ensures the automated workflow can bypass protection rules while maintaining security for human contributors.