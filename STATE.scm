;; SPDX-License-Identifier: AGPL-3.0-or-later
;; STATE.scm - Tree Navigator project state
;; Updated: 2025-12-31

(define state
  `((metadata
     (version . "2.1.0")
     (schema-version . "1.0")
     (created . "2024-11-06")
     (updated . "2025-12-31")
     (project . "tree-navigator")
     (repo . "https://github.com/hyperpolymath/tree-navigator"))

    (project-context
     (name . "Tree Navigator")
     (tagline . "Directory tree visualization and export tool")
     (tech-stack . (ada-2022 gnat gprbuild)))

    (current-position
     (phase . "mvp-completion")
     (overall-completion . 75)
     (components
      ((interactive-mode
        (status . complete)
        (completion . 100))
       (export-mode
        (status . just-implemented)
        (completion . 90))
       (filtering
        (status . implemented)
        (completion . 85))
       (tui-interface
        (status . planned)
        (completion . 0))
       (printer-support
        (status . planned)
        (completion . 0))))
     (working-features
      interactive-navigation
      bookmarks
      file-type-detection
      directory-exclusion
      file-exclusion
      tree-export
      statistics))

    (route-to-mvp
     ((milestone . "v2.1.0-export")
      (status . in-progress)
      (items
       ((item . "Create tree_printer module")
        (status . done))
       ((item . "Add --export flag to main.adb")
        (status . done))
       ((item . "Add treenav alias")
        (status . done))
       ((item . "Test build and export")
        (status . pending))
       ((item . "Update CHANGELOG")
        (status . pending))))
     ((milestone . "v2.2.0-tui")
      (status . planned)
      (items
       ((item . "Ada/SPARK TUI framework")
        (status . planned))
       ((item . "ncurses or custom terminal UI")
        (status . planned))
       ((item . "Real-time tree navigation display")
        (status . planned)))))

    (blockers-and-issues
     (critical . ())
     (high
      ((issue . "Build not yet tested with new tree_printer module")
       (impact . "Cannot verify export functionality works")))
     (medium
      ((issue . "README documents features not yet implemented")
       (impact . "User expectations mismatch")))
     (low . ()))

    (critical-next-actions
     (immediate
      ("Test gprbuild with new tree_printer module"
       "Verify export functionality works"
       "Update version to 2.1.0"))
     (this-week
      ("Add SPDX headers to all Ada source files"
       "Update CHANGELOG.md for v2.1.0"
       "Create v2.1.0 release"))
     (this-month
      ("Research Ada/SPARK TUI options"
       "Plan printer integration"
       "Consider winget submission like bunsenite")))

    (session-history
     ((date . "2025-12-31")
      (accomplishments
       "Created tree_printer.ads/adb export engine"
       "Updated main.adb with --export flag handling"
       "Added treenav alias to justfile"
       "Created STATE.scm")))))
