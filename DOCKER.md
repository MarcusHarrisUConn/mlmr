# Docker

`mlmr` includes a Docker setup for demos, workshops, and deployment testing.
The Docker files are kept out of the CRAN source package, but they are tracked
in the GitHub repository.

## Build Locally

From the repository root:

```bash
docker build -t mlmr:local .
```

## Run Locally

```bash
docker run --rm -p 3838:3838 mlmr:local
```

Then open:

<http://localhost:3838>

## Docker Compose

```bash
docker compose up --build
```

Stop with:

```bash
docker compose down
```

## GitHub Container Registry

The repository includes a GitHub Actions workflow that builds the Docker image
on pull requests and pushes to `ghcr.io/marcusharrisuconn/mlmr` only when the
workflow is run manually or when a version tag is pushed.

After publishing, users will be able to run:

```bash
docker pull ghcr.io/marcusharrisuconn/mlmr:latest
docker run --rm -p 3838:3838 ghcr.io/marcusharrisuconn/mlmr:latest
```

## Notes

- The image launches the installed package app, not a loose development copy.
- The container listens on port `3838`.
- Set `PORT` only if you need a different internal container port.
