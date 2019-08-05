# Asana

Tools for interacting with the Asana API

## Usage

1. Grab the Project Id from the URL when viewing it
1. Visit *Settings > Apps > Developer apps*, create a Personal Access Token

```console
export ASANA_API_KEY=<access-token>

stack build
stack exec -- <tool> --project <project-id>
```

### ENV

You can persist your API key in `~/.env.asana` if you'd rather not set it on each run.

```
echo "ASANA_API_KEY=<access-token>" >> ~/.env.asana
```

## Tools

### `start-iteration`

- List Stories in the project
- Warn for unexpected things like already-completed stories
- Show the points totals to document for starting the iteration

### `close-iteration`

- List Stories in the project
- Warn for unexpected things like missing carry
- Show the points totals to document for closing the iteration

### `debt-evaluation`

- Collects cost/impact/virality of debt tasks
- Calculates actionability
- Sets actionability on tasks

### `bug-reproduction`

- List Bugs completed since we started tracking this information
- Warn for Bugs lacking this information
- Report totals for reproduced vs not
