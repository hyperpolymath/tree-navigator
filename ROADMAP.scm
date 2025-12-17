;;; ROADMAP.scm — tree-navigator
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; Updated: 2025-12-17

(define-module (tree-navigator roadmap)
  #:export (roadmap milestones priorities))

;;; ============================================================================
;;; MILESTONE 0.1 — Foundation (Current) [~40% Complete]
;;; ============================================================================

(define milestone-0.1
  '((name . "Foundation")
    (version . "0.1.0")
    (status . "in-progress")
    (completion . 40)

    (completed
     (("RSR compliance setup" . "SPDX headers, SHA-pinned actions")
      ("GitHub/GitLab CI workflows" . "CodeQL, Scorecard, SLSA, quality checks")
      ("Guix package definition" . "guix.scm with proper Ada deps")
      ("Nix fallback" . "flake.nix for Nix users")
      ("Security policy" . "SECURITY.md with proper content")
      ("Core Ada skeleton" . "main.adb, navigator, config, bookmarks, terminal")
      ("Project files" . "tree_navigator.gpr, justfile")))

    (remaining
     (("tree_printer module" . "Export engine (tree_printer.ads/adb)")
      ("Unit tests" . "Basic test coverage for core modules")
      ("Man page content" . "Complete man/tree-navigator.1")
      ("Build verification" . "CI builds passing on all platforms")))))

;;; ============================================================================
;;; MILESTONE 0.2 — Core Export Features
;;; ============================================================================

(define milestone-0.2
  '((name . "Core Export Features")
    (version . "0.2.0")
    (status . "planned")
    (completion . 0)

    (goals
     (("--export FILE" . "Save tree output to file")
      ("--max-depth N" . "Limit traversal depth")
      ("--exclude-dirs LIST" . "Comma-separated directory exclusions")
      ("--exclude-files LIST" . "Comma-separated file pattern exclusions")
      ("--show-size" . "Display file sizes")
      ("--show-hidden" . "Include dotfiles")
      ("Statistics footer" . "File count, dir count, total size, excluded count")))))

;;; ============================================================================
;;; MILESTONE 0.3 — File Type Filtering
;;; ============================================================================

(define milestone-0.3
  '((name . "File Type Filtering")
    (version . "0.3.0")
    (status . "planned")
    (completion . 0)

    (goals
     (("--only-type TYPE" . "Filter by file type")
      ("Type: executable" . ".exe, .bin, +x permission")
      ("Type: config" . ".conf, .cfg, .ini, .toml, .yaml")
      ("Type: script" . ".sh, .bash, .zsh, .fish")
      ("Type: data" . ".json, .xml, .yaml, .yml, .csv")
      ("Type: hidden" . "Files starting with .")
      ("--no-files / --no-dirs" . "Directory or file only modes")))))

;;; ============================================================================
;;; MILESTONE 0.4 — Interactive Mode Enhancement
;;; ============================================================================

(define milestone-0.4
  '((name . "Interactive Mode Enhancement")
    (version . "0.4.0")
    (status . "planned")
    (completion . 0)

    (goals
     (("Bookmark persistence" . "Save/load favorite directories")
      ("History tracking" . "Remember recently visited paths")
      ("Search within tree" . "Find files/dirs by name pattern")
      ("Quick navigation" . "Fuzzy path completion")
      ("TUI improvements" . "Better visual feedback and colors")))))

;;; ============================================================================
;;; MILESTONE 1.0 — Stable Release
;;; ============================================================================

(define milestone-1.0
  '((name . "Stable Release")
    (version . "1.0.0")
    (status . "planned")
    (completion . 0)

    (goals
     (("Full documentation" . "Complete README, man pages, wiki")
      ("Package releases" . "DEB, RPM, Flatpak, AppImage")
      ("Cross-platform CI" . "Linux, macOS, Windows builds")
      ("Performance optimization" . "Handle large directory trees efficiently")
      ("Comprehensive tests" . "80%+ code coverage")
      ("API stability" . "Stable CLI interface commitment")))))

;;; ============================================================================
;;; FUTURE — Post-1.0 Ideas
;;; ============================================================================

(define future-ideas
  '(("Git integration" . "Show git status indicators in tree")
    ("Remote support" . "SSH/SFTP directory browsing")
    ("JSON/XML output" . "Machine-readable export formats")
    ("Plugin system" . "Custom file type handlers")
    ("Watch mode" . "Live-updating tree display")
    ("Diff mode" . "Compare two directory trees")))

;;; ============================================================================
;;; PRIORITIES (Current Sprint)
;;; ============================================================================

(define priorities
  '((immediate
     (("Implement tree_printer.ads/adb" . "Critical for export functionality")
      ("Add basic unit tests" . "Verify core modules work")
      ("Complete man page" . "User documentation")))

    (short-term
     (("--export implementation" . "Primary use case")
      ("--exclude-dirs/files" . "Essential filtering")
      ("Statistics output" . "Useful feedback")))

    (medium-term
     (("File type detection" . "Enhanced categorization")
      ("Interactive improvements" . "Better UX")
      ("Package builds" . "Distribution")))))

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(define roadmap
  (list milestone-0.1 milestone-0.2 milestone-0.3 milestone-0.4 milestone-1.0))

(define milestones
  '("0.1 Foundation" "0.2 Export" "0.3 Filtering" "0.4 Interactive" "1.0 Stable"))
