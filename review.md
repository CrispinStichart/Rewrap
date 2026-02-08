# Code Review

## Summary
Focused review of the core parsing/wrapping pipeline and selection handling. The most impactful issue is empty-file handling, which can currently throw before any wrapping logic runs. I also flagged a selection-ordering assumption that can mis-handle multi-cursor selections, and a concatenation edge case that can drop content if blank lines slip into a wrap block.

## Findings

### High
1. Empty files can crash `rewrap` and the core parser.
   - `rewrap` builds `strLines` via `Nonempty.fromSeqUnsafe` without guarding for an empty file. If `getLine(0)` returns `null`, `Nonempty.fromSeqUnsafe` will throw before any edit is produced. `processContent` also uses `Seq.head`/`Seq.tail` with no empty guard, so even if `rewrap` is fixed, empty inputs can still fail later.
   - Impact: Wrapping on an empty file (or a file where the editor returns no lines) should be a no-op but currently can raise an exception.
   - References: `core/Main.fs:26-34`, `core/Parsing.fs:166-186`.

### Medium
1. Selection ranges are normalized without sorting, despite the function contract.
   - `normalizeRanges` assumes ordered ranges (“requires the ranges to already be in order”), but `wrapSelected` passes selections as-is. Editors do not always guarantee ordered selections (e.g., multi-cursor insertion order). This can cause missed or incorrect wrapping when selections are out of order.
   - Impact: Non-deterministic or incorrect wrap results for multi-selection workflows.
   - Reference: `core/Selections.fs:221-223` (and `normalizeRanges` contract earlier in file).

### Low
1. `concatLines` silently drops content if any input line is empty.
   - `concatLines`’s reducer returns `acc` unchanged when `line = String.Empty` or `acc = String.Empty`. If a wrap block ever contains an empty string (even as the first line), the rest of the block gets discarded.
   - Impact: Potential content loss if an empty line makes it into a wrap block (edge case but possible via parsers or unexpected input).
   - Reference: `core/Wrapping.fs:45-54`.

## Tests
Not run in this review.
