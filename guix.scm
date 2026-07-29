; SPDX-License-Identifier: MPL-2.0
;; guix.scm — GNU Guix package definition for tree-navigator
;; Usage: guix shell -f guix.scm

(use-modules (guix packages)
             (guix build-system gnu)
             (guix licenses))

(package
  (name "tree-navigator")
  (version "0.1.0")
  (source #f)
  (build-system gnu-build-system)
  (synopsis "tree-navigator")
  (description "tree-navigator — part of the hyperpolymath ecosystem.")
  (home-page "https://github.com/hyperpolymath/tree-navigator")
  (license mpl2.0))
