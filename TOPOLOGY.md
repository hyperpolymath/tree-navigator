<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-19 -->

# Tree Navigator — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              DEVELOPER / USER           │
                        │        (CLI Interface / Interactive)    │
                        └───────────────────┬─────────────────────┘
                                            │ Command / Navigate
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           TREE NAVIGATOR CORE (ADA)     │
                        │    (Recursive Traversal, Filter Engine) │
                        └──────────┬───────────────────┬──────────┘
                                   │                   │
                                   ▼                   ▼
                        ┌───────────────────────┐  ┌────────────────────────────────┐
                        │ EXPORT ENGINE         │  │ NAVIGATOR ENGINE               │
                        │ - Smart Filtering     │  │ - Interactive TUI              │
                        │ - File Categorization │  │ - Bookmark Management          │
                        │ - Tree Printer        │  │ - Path Navigation              │
                        └──────────┬────────────┘  └──────────┬─────────────────────┘
                                   │                          │
                                   └────────────┬─────────────┘
                                                ▼
                        ┌─────────────────────────────────────────┐
                        │             DATA LAYER                  │
                        │  ┌───────────┐  ┌───────────────────┐  │
                        │  │ local FS  │  │ ~/.config/tree-   │  │
                        │  │ (Target)  │  │ navigator/        │  │
                        │  └───────────┘  └───────────────────┘  │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  Justfile Automation  .machine_readable/  │
                        │  GPRBuild / Ada 2022  0-AI-MANIFEST.a2ml  │
                        └─────────────────────────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
CORE ENGINE (ADA)
  Tree Printer Engine               ██████████ 100%    Recursive traversal stable
  File Type Detection               ██████████ 100%    Categorization verified
  Filtering Engine                  ██████████ 100%    Exclude dirs/files active
  CLI Interface                     ██████████ 100%    All 10+ flags functional

NAVIGATION & UI
  Interactive TUI                   ██████████ 100%    Depth control & UI verified
  Bookmark Management               ██████████ 100%    Persistent storage active
  Statistics Module                 ██████████ 100%    File/Size counts verified

REPO INFRASTRUCTURE
  Justfile Automation               ██████████ 100%    Standard build/run tasks
  .machine_readable/                ██████████ 100%    STATE tracking active
  0-AI-MANIFEST.a2ml                ██████████ 100%    AI entry point verified

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ██████████ 100%    Production tool stable
```

## Key Dependencies

```
CLI Command ──────► Filter Engine ──────► Tree Printer ──────► Export File
     │                   │                   │                    │
     ▼                   ▼                   ▼                    ▼
Interactive UI ──► Path Navigator ───► Categorization ──► Terminal Output
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
