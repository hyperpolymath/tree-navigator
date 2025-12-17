;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; tree-navigator - Guix Package Definition
;; Run: guix shell -D -f guix.scm

(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages base)
             (gnu packages ada)
             (gnu packages certs))

(define-public tree_navigator
  (package
    (name "tree-navigator")
    (version "0.1.0")
    (source (local-file "." "tree-navigator-checkout"
                        #:recursive? #t
                        #:select? (git-predicate ".")))
    (build-system gnu-build-system)
    (native-inputs
     (list gnat gprbuild))
    (inputs
     (list nss-certs))  ; For HTTPS certificate validation
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (invoke "gprbuild" "-P" "tree_navigator.gpr" "-j0" "-p")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (install-file "bin/main" bin)
                (rename-file (string-append bin "/main")
                             (string-append bin "/tree-navigator")))))
          (delete 'check))))  ; Tests require built binary
    (synopsis "Type-safe directory tree navigator written in Ada 2022")
    (description
     "Tree Navigator is a powerful, type-safe directory tree export and
navigation tool written in Ada 2022.  It provides smart filtering, type-based
file selection, statistics, and clean output for documentation purposes.
Part of the RSR (Rhodium Standard Repository) ecosystem.")
    (home-page "https://github.com/hyperpolymath/tree-navigator")
    (license (list license:expat license:agpl3+))))  ; MIT OR AGPL-3.0-or-later

;; Return package for guix shell
tree_navigator
