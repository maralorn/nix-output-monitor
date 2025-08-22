# GitHub Actions Push Protection Bypass Configuration

This document explains how to configure the repository to allow the `pull-from-forgejo.yaml` GitHub Actions workflow to bypass push protection rules on the main branch.

## Problem

When branch protection rules are enabled on the `main` branch, the automated workflow that pulls changes from `code.maralorn.de` and pushes them to GitHub may fail because the default `GITHUB_TOKEN` doesn't have sufficient permissions to bypass protection rules.

## Solution Options

### Option 1: Allow GitHub Actions to Bypass Protection (Recommended)

1. Go to **Settings** → **Branches** in the GitHub repository
2. Find the branch protection rule for `main`
3. Check the option **"Allow specified actors to bypass required pull requests"**
4. Add `github-actions[bot]` to the list of actors that can bypass protection
5. Alternatively, check **"Do not restrict pushes that create files"** if you only want to allow file additions

### Option 2: Use Enhanced Permissions in Workflow

The workflow has been updated with:
- `permissions: contents: write` to ensure proper write access
- Proper git configuration for the GitHub Actions bot
- Full fetch depth to handle complex git operations

### Option 3: Use a Personal Access Token (Alternative)

If the above options don't work, you can create a Personal Access Token (PAT):

1. Create a PAT with `repo` scope and admin permissions
2. Add it as a repository secret (e.g., `ADMIN_TOKEN`)
3. Update the workflow to use this token:
   ```yaml
   - uses: actions/checkout@v4
     with:
       token: ${{ secrets.ADMIN_TOKEN }}
   ```

## Current Workflow Configuration

The `pull-from-forgejo.yaml` workflow has been updated to:
- Include explicit `contents: write` permission
- Use proper git configuration for GitHub Actions bot
- Fetch complete git history for complex operations

## Testing

To test if the configuration works:
1. Enable branch protection rules on `main`
2. Trigger the workflow manually via **Actions** → **Pull from code.maralorn.de** → **Run workflow**
3. Check that the workflow completes successfully without permission errors

## Additional Notes

- The workflow uses `--ff-only` to ensure only fast-forward merges
- Git user configuration is set to `github-actions[bot]` for proper attribution
- The solution maintains security while allowing necessary automation