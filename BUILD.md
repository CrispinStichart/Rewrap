# Build & Test

This repo contains three pieces:
- `core/`: F# core library compiled via Fable and bundled with Parcel.
- `vscode/`: VS Code extension (TypeScript + Parcel bundle).
- `vs/`: Visual Studio extension (separate build).

Most workflows are driven by the `./do` script in the repo root.

## Prerequisites

- Node.js (>= 14.x for core tests; `vscode/package.json` says >= 12.x for the extension).
- npm
- .NET SDK (for F# + Fable)

The repo includes a .NET tool manifest; `./do` will run `dotnet tool restore` automatically.
This repo pins the SDK to .NET 10 via `global.json` (uses the system SDK on PATH).
If you need a different SDK version, update `global.json` accordingly.

## Install

From the repo root:

```bash
npm install
```

`./do` will also install dependencies if they are missing.

## Build (Development)

```bash
./do build
```

Builds both `core` and `vscode` in dev mode.

Component-specific builds:

```bash
./do build core
./do build vscode
```

## Build (Production)

```bash
./do prod
```

Runs tests and produces production bundles for `core` and `vscode`.

## Watch Mode

```bash
./do watch
```

Watches core and VS Code sources, rebuilding and re-running tests as files change.

## Tests

```bash
./do test
```

Runs:
- Core tests (`node core/test`) after building with Fable.
- VS Code integration tests (`node vscode/test/run.cjs`) unless running inside VS Code.

You can also use the npm script:

```bash
npm test
```

## Clean

```bash
./do clean
```

Removes build artifacts and caches for `core` and `vscode`.

## VS Code Extension Package

```bash
./do package
```

Creates a VSIX at `.obj/Rewrap-VSCode.vsix`.

## Visual Studio Extension (vs/)

The Visual Studio extension lives in `vs/` and is not built by `./do`. Build it via Visual Studio/MSBuild using `vs/VS.csproj`.
