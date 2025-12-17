;;; STATE.scm — tree-navigator
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

(define metadata
  '((version . "0.1.0") (updated . "2025-12-17") (project . "tree-navigator")))

(define current-position
  '((phase . "v0.1 - Security & SCM Review")
    (overall-completion . 40)
    (components
     ((rsr-compliance ((status . "complete") (completion . 100)))
      (guix-package ((status . "complete") (completion . 100)))
      (nix-fallback ((status . "complete") (completion . 100)))
      (security-policy ((status . "complete") (completion . 100)))
      (ci-workflows ((status . "complete") (completion . 100)))
      (core-features ((status . "in-progress") (completion . 30)))))))

(define blockers-and-issues '((critical ()) (high-priority ())))

(define critical-next-actions
  '((immediate
     (("Implement tree_printer.ads/adb" . high)
      ("Add unit tests" . high)))
    (this-week
     (("Complete file_types module" . medium)
      ("Add man page content" . medium)))))

(define session-history
  '((snapshots
     ((date . "2025-12-17") (session . "scm-security-review")
      (notes . "Fixed SECURITY.md, updated guix.scm with proper Ada deps, added flake.nix fallback"))
     ((date . "2025-12-15") (session . "initial")
      (notes . "SCM files added")))))

(define state-summary
  '((project . "tree-navigator") (completion . 40) (blockers . 0) (updated . "2025-12-17")))
