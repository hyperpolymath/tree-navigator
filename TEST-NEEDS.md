# TEST-NEEDS.md — tree-navigator

## CRG Grade: C — ACHIEVED 2026-04-04

## Current Test State

| Category | Count | Notes |
|----------|-------|-------|
| Zig FFI tests | 1 | `ffi/zig/test/integration_test.zig` |
| Test infrastructure | Present | `tests/` directory structure |

## What's Covered

- [x] Zig FFI integration tests
- [x] Test framework infrastructure

## Still Missing (for CRG B+)

- [ ] Tree traversal unit tests
- [ ] Navigation algorithm tests
- [ ] Property-based tree generation
- [ ] Performance benchmarks
- [ ] Edge case handling tests

## Run Tests

```bash
cd /var/mnt/eclipse/repos/tree-navigator && cargo test
```
