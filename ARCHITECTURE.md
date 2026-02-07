# Architecture

This document explains how Rewrap is structured and how the pieces fit together so you can add or extend features quickly.

## Overview
Rewrap Revived is a comment/text re-wrapping engine with two editors on top:
- `core/` is the F# library that parses documents into blocks and rewrites selected blocks into wrapped text.
- `vscode/` is the VS Code extension that calls the core library (compiled to JavaScript via Fable).
- `vs/` is the Visual Studio extension (built separately).

The core library is the source of truth for parsing, wrapping, and selection behavior. The editor extensions are thin adapters that translate editor APIs to the core types and apply edits.

## Repo Layout
- `core/` F# library, compiled to JS with Fable and bundled for extensions.
  - Entry point: `core/Main.fs`
  - Parsing and language definitions: `core/Parsing.*.fs`, `core/Parsers*.fs`
  - Wrapping algorithm: `core/Wrapping.fs`
  - Selection handling and edit generation: `core/Selections.fs`
  - Fundamental types/utilities: `core/Types.fs`, `core/Line.fs`, `core/Block.fs`, `core/Prelude.fs`
  - Tests: `core/Tests.fs`, `core/test/`
- `vscode/` VS Code extension (TypeScript)
  - Entry: `vscode/src/Extension.ts`
  - Core wrapper: `vscode/src/Core.ts`
  - Settings, auto-wrap: `vscode/src/Settings.ts`, `vscode/src/AutoWrap.ts`
- `vs/` Visual Studio extension (C# project, separate build)
- `docs/` documentation content for the docs site
- `build/`, `do`, `BUILD.md` build/test scripts and guidance

## Core Data Flow (Rewrap Command)
1. **Editor collects context**
   - VS Code collects selections and settings.
   - It calls `rewrap` in `core/Main.fs` (via `vscode/src/Core.ts`).
2. **Document processor selection**
   - `Parsing.Documents.select` chooses the appropriate `DocumentProcessor` based on language id and file path.
   - Language configuration lives in `core/Parsing.Documents.fs` (list of `Language.create` entries).
3. **Parsing into blocks**
   - The processor parses lines into `Block` values using parsers in `core/Parsing.*.fs` and `core/Parsers*.fs`.
   - Blocks are `Wrap`, `NoWrap`, or `Comment` containers (`core/Parsing.fs`).
4. **Selection-aware processing**
   - `Selections.wrapSelected` walks blocks, applies selection rules, and decides which blocks to wrap or leave unchanged (`core/Selections.fs`).
5. **Wrap algorithm**
   - For wrapped blocks, `Wrapping.breakUpString` concatenates content, applies whitespace rules, and breaks lines based on the wrapping column, tab width, and punctuation rules (`core/Wrapping.fs`).
6. **Edit generation**
   - The core outputs an `Edit` with start/end line and replacement lines. VS Code applies it.

## Core Data Flow (Auto Wrap)
- `Main.maybeAutoWrap` checks the inserted text and cursor position.
- If the cursor goes past the column and conditions are met (space/enter), it performs a wrap for that line and returns an edit (`core/Main.fs`).

## Key Domain Types
- `Settings`: wrapping column, tab width, double sentence spacing, reformat, wholeComment (`core/Types.fs`, used via `vscode/src/Settings.ts`).
- `Edit`: describes the replacement range and new lines (`core/Types.fs`).
- `Selection`: editor selection positions; VS Code uses `vscode/src/Core.ts` to convert selections to POJOs for the core library.
- `Block`: parsed units of content with type `Wrap`, `NoWrap`, or `Comment` (`core/Parsing.fs`, `core/Block.fs`).

## Parsing Model
Parsing is split into two layers:
- **Document processors** that turn a document into a sequence of blocks (`DocumentProcessor` in `core/Parsing.fs`).
- **Line/Block parsers** that interpret comments, doc comments, and embedded markup (markdown, rst, etc.).

Important files:
- `core/Parsing.fs`: the modern “new parser” model (block types, line results, `processContent`).
- `core/Parsing.Core.fs`: helpers for composing parsers and splitting input.
- `core/Parsers.fs`, `core/Parsers_Markdown.fs`, `core/Parsers_RST.fs`: specific markup parsers.
- `core/Parsing.Documents.fs`: language registry and document processor selection.

The language registry is the extension point for adding or changing language/comment support. Each `Language` entry links file extensions and aliases to a document processor. Many languages share common processor constructors like `sourceCode` or comment marker helpers.

## Wrapping Rules and Behavior
Wrapping behavior is centralized in `core/Wrapping.fs`:
- Concatenate lines with whitespace rules and optional double sentence spacing.
- Use CJK-aware line breaking logic.
- Calculate line width with tab expansion (`core/Line.fs`).
- Handle prefixes (comment leaders, bullets) and reformatting.

Selection handling in `core/Selections.fs` decides when a block is fully or partially wrapped, and how empty selections behave (e.g., whole comment wrapping).

## Extension Integration (VS Code)
- `vscode/src/Core.ts` is the boundary between TypeScript and the compiled Fable output in `core/dist/dev/Main.js`.
- It adapts `Position` and `Selection` objects and re-exports core functions (`rewrap`, `maybeAutoWrap`, etc.).
- `vscode/src/Extension.ts` wires up commands and on-save behavior, and applies edits.

The VS Code extension intentionally keeps logic minimal; most behavioral changes belong in `core/`.

## How to Add or Extend Features
### Add a new language or tweak comment markers
1. Update `core/Parsing.Documents.fs` and add or modify a `Language.create` entry.
2. Reuse existing comment markers/parsers or add a new parser in `core/Parsers*.fs` or `core/Parsing.SourceCode.fs`.
3. Build and test with `./do test`.

### Change wrapping behavior
1. Update `core/Wrapping.fs` for line breaking, spacing, or CJK behavior.
2. If selection behavior changes, adjust `core/Selections.fs`.
3. Add/adjust tests in `core/Tests.fs` and `core/test/`.

### Add a new command or setting
1. Add UI wiring in `vscode/src/Extension.ts`.
2. Map settings in `vscode/src/Settings.ts`.
3. If the core needs new inputs, extend `Settings` or new parameters in `core/Types.fs` and thread through `Main.fs`.

## Build and Test
- See `BUILD.md` for details.
- Primary flow: `./do build`, `./do test`, `./do watch`.
- The Visual Studio extension under `vs/` builds separately.

## Quick Orientation (Entry Points)
- Core API: `core/Main.fs`
- Language registry: `core/Parsing.Documents.fs`
- Parser infrastructure: `core/Parsing.fs`, `core/Parsing.Core.fs`
- Wrapping algorithm: `core/Wrapping.fs`
- Selection and edit generation: `core/Selections.fs`
- VS Code entry: `vscode/src/Extension.ts`
- VS Code core adapter: `vscode/src/Core.ts`
