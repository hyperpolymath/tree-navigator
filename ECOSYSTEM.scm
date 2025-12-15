;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — tree-navigator

(ecosystem
  (version "1.0.0")
  (name "tree-navigator")
  (type "project")
  (purpose "> A powerful, type-safe directory tree export and navigation tool written in Ada 2022")

  (position-in-ecosystem
    "Part of hyperpolymath ecosystem. Follows RSR guidelines.")

  (related-projects
    (project (name "rhodium-standard-repositories")
             (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
             (relationship "standard")))

  (what-this-is "> A powerful, type-safe directory tree export and navigation tool written in Ada 2022")
  (what-this-is-not "- NOT exempt from RSR compliance"))
