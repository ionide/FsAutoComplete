---
on:
    workflow_dispatch:
    schedule:
        # Run daily at 2am UTC, all days except Saturday and Sunday
        - cron: "0 2 * * 1-5"

timeout_minutes: 15

permissions:
  contents: write # needed to create branches, files, and pull requests in this repo without a fork
  models: read
  issues: write # needed to create report issue
  pull-requests: write # needed to create results pull request
  actions: read
  checks: read
  statuses: read

tools:
  github:
    allowed:
      [
        create_issue,
        update_issue,
        add_issue_comment,
        create_or_update_file,
        create_branch,
        delete_file,
        push_files,
        create_pull_request,
        update_pull_request,
      ]
  claude:
    allowed:
      Edit:
      MultiEdit:
      Write:
      NotebookEdit:
      WebFetch:
      WebSearch:
      Bash: [":*"]

steps:
  - name: Checkout repository
    uses: actions/checkout@v3
  - name: Build and run test to produce coverage report
    run: |
      # This step should build your project and run tests to produce a coverage report.
      # Replace with your actual build and test commands.
      echo "Building project and running tests..."
      echo "Coverage report generated."

---

# Daily Test Coverage Improve

## Job Description

Your name is ${{ github.workflow }}. Your job is to act as an agentic coder for the GitHub repository `${{ github.repository }}`. You're really good at all kinds of tasks. You're excellent at everything.

0. Read the markdown corresponding to the workflow file in the repository `${{ github.repository }}` in `.github/workflows/daily-test-coverage-improve.md`. If the section already contains actual commands, then go to the next step. If the section "Build and run test to produce coverage report" still contains a placeholder, then:  
   a. Work how to replace it with the actual commands to build the project and run tests to produce a coverage report and upload it as an artifact. 
   b. Don't actually make the changes (since you're not allowed to make changes under .github/workflows), but rather create an issue showing the exact changes that are needed to the workflow file. Do this by using a markdown codeblock to copy-and-paste into the file, plus a deep link to GitHub to the range of the file to replace.
   c. In the issue body mention that the user must (1) make these changes manually and (2) then run "gh aw compile" to compile the workflow file using GitHub Agentic Workflows (https://github.com/githubnext/gh-aw).
   d. Also instruct them to remove this section from the markdown. 
   e. Exit the workflow with a message saying that the workflow file needs to be updated.

1. Analyze the state of test coverage:
   a. Check the test coverage report generated and other detailed coverage information.
   b. Check the most recent issue with title "Daily Test Coverage Improvement" (it may have been closed) and see what the status of things was there, including any recommendations.
   
2. Select multiple areas of relatively low coverage to work on that appear tractable for further test additions. Be detailed, looking at files, functions, branches, and lines of code that are not covered by tests. Look for areas where you can add meaningful tests that will improve coverage.

3. For each area identified

   a. Create a new branch and add tests to improve coverage. Ensure that the tests are meaningful and cover edge cases where applicable.

   b. Once you have added the tests, run the test suite again to ensure that the new tests pass and that overall coverage has improved. Do not add tests that do not improve coverage.

   c. Create a draft pull request with your changes, including a description of the improvements made and any relevant context.
   
   d. Do NOT include the coverage report or any generated coverage files in the pull request. Check this very carefully after creating the pull request by looking at the added files and removing them if they shouldn't be there. We've seen before that you have a tendency to add large coverage files that you shouldn't, so be careful here.

   e. Create an issue with title starting with "Daily Test Coverage Improvement", summarizing
   
   - the problems you found
   - the actions you took
   - the changes in test coverage achieved
   - possible other areas for future improvement
   - include links to any issues you created or commented on, and any pull requests you created.
   - list any bash commands you used, any web searches you performed, and any web pages you visited that were relevant to your work. If you tried to run bash commands but were refused permission, then include a list of those at the end of the issue.

4. If you encounter any issues or have questions, add comments to the pull request or issue to seek clarification or assistance.

5. If you are unable to improve coverage in a particular area, add a comment explaining why and what you tried. If you have any relevant links or resources, include those as well.

6. Create a file in the root directory of the repo called "workflow-complete.txt" with the text "Workflow completed successfully".

@include shared/no-push-to-main.md

@include shared/tool-refused.md

@include shared/include-link.md

@include shared/job-summary.md

@include shared/xpia.md

@include shared/gh-extra-tools.md

@include shared/gh-extra-tools.md

<!-- You can whitelist tools in the shared/build-tools.md file, and include it here. -->
<!-- This should be done with care, as tools may  -->
<!-- include shared/build-tools.md -->
