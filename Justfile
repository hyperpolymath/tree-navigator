# Tree Navigator Justfile
# Modern command runner - https://github.com/casey/just
#
# Install: cargo install just
# or: brew install just
# or: sudo dnf install just

# Set shell
set shell := ["bash", "-uc"]

# Default recipe (runs when you type 'just')
default:
    @just --list

# ============================================================================
# Building
# ============================================================================

# Build the project (debug mode)
build:
    @echo "🔨 Building Tree Navigator (debug)..."
    gprbuild -P tree_navigator.gpr
    @echo "✅ Build complete: ./bin/main"

# Build optimized release version
release:
    @echo "🔨 Building Tree Navigator (release)..."
    gprbuild -P tree_navigator.gpr -XBuild_Mode=release
    strip bin/main
    @echo "✅ Release build complete: ./bin/main"

# Clean build artifacts
clean:
    @echo "🧹 Cleaning build artifacts..."
    gprclean -P tree_navigator.gpr
    rm -rf obj/ bin/ *.o *.ali
    @echo "✅ Clean complete"

# Clean and rebuild
rebuild: clean build

# Clean and rebuild release
rebuild-release: clean release

# ============================================================================
# Testing
# ============================================================================

# Run basic tests
test: build
    @echo "🧪 Running tests..."
    @./bin/main --version
    @./bin/main --help
    @mkdir -p test_output
    @./bin/main --export test_output/test1.txt --max-depth 3
    @./bin/main --export test_output/test2.txt --exclude-dirs "obj,bin" --max-depth 2
    @echo "✅ Tests passed"

# Run comprehensive test suite
test-full: build
    @echo "🧪 Running full test suite..."
    @./scripts/run-all-tests.sh
    @echo "✅ Full tests complete"

# Run performance benchmarks
bench: release
    @echo "⚡ Running benchmarks..."
    @mkdir -p bench_data
    @for i in {1..100}; do mkdir -p bench_data/dir_$$i; done
    @/usr/bin/time -v ./bin/main --export bench_output.txt --dir bench_data
    @rm -rf bench_data bench_output.txt
    @echo "✅ Benchmark complete"

# Check for warnings
check:
    @echo "🔍 Checking code quality..."
    @for file in src/*.ad[sb]; do \
        echo "Checking $$file..."; \
        gcc -c -gnat2022 -gnats $$file || exit 1; \
    done
    @echo "✅ Code quality check passed"

# ============================================================================
# Running
# ============================================================================

# Run the application
run *ARGS: build
    ./bin/main {{ARGS}}

# Run with example: export current directory
example-export: build
    ./bin/main --export tree-output.txt --max-depth 5
    @echo "📄 Output written to: tree-output.txt"
    @cat tree-output.txt

# Run with example: exclude common directories
example-clean: build
    ./bin/main --export clean-tree.txt \
        --exclude-dirs "obj,bin,node_modules,target,.git" \
        --max-depth 4
    @cat clean-tree.txt

# Run with example: show file sizes
example-sizes: build
    ./bin/main --export sized-tree.txt --show-size --max-depth 3
    @cat sized-tree.txt

# Run interactive mode
interactive: build
    ./bin/main

# ============================================================================
# Installation
# ============================================================================

# Install to /usr/local (requires sudo)
install: release
    @echo "📦 Installing Tree Navigator..."
    sudo cp bin/main /usr/local/bin/tree-navigator
    sudo chmod 755 /usr/local/bin/tree-navigator
    sudo mkdir -p /usr/local/share/man/man1
    sudo cp man/tree-navigator.1 /usr/local/share/man/man1/
    sudo gzip -f /usr/local/share/man/man1/tree-navigator.1
    sudo mandb 2>/dev/null || true
    @echo "✅ Installed to /usr/local/bin/tree-navigator"
    @tree-navigator --version

# Uninstall from system
uninstall:
    @echo "🗑️  Uninstalling Tree Navigator..."
    sudo rm -f /usr/local/bin/tree-navigator
    sudo rm -f /usr/local/share/man/man1/tree-navigator.1.gz
    sudo mandb 2>/dev/null || true
    @echo "✅ Uninstalled"

# Install using provided script
install-script: release
    sudo ./scripts/install.sh

# ============================================================================
# Packaging
# ============================================================================

# Build all packages
package-all:
    @echo "📦 Building all packages..."
    ./scripts/packaging/build-all-packages.sh
    @echo "✅ All packages built in dist/"

# Build DEB package
package-deb: release
    @echo "📦 Building DEB package..."
    cd packaging/debian && debuild -us -uc -b
    @echo "✅ DEB package built"

# Build RPM package
package-rpm: release
    @echo "📦 Building RPM package..."
    rpmbuild -ba packaging/fedora/tree-navigator.spec
    @echo "✅ RPM package built"

# Build Flatpak
package-flatpak: release
    @echo "📦 Building Flatpak..."
    flatpak-builder --repo=build/flatpak-repo build/flatpak-build \
        packaging/flatpak/com.gitlab.hyperpolymath.TreeNavigator.yaml
    @echo "✅ Flatpak built"

# Build AppImage
package-appimage: release
    @echo "📦 Building AppImage..."
    cd packaging/appimage && ./build-appimage.sh
    @echo "✅ AppImage built"

# Build source tarball
package-tarball: clean
    @echo "📦 Creating source tarball..."
    tar czf tree-navigator-2.0.0.tar.gz \
        --exclude=.git \
        --exclude=obj \
        --exclude=bin \
        --exclude=build \
        --exclude=dist \
        --exclude=wiki \
        .
    @echo "✅ Tarball created: tree-navigator-2.0.0.tar.gz"

# Generate checksums for all packages
package-checksums:
    @echo "🔐 Generating checksums..."
    cd dist && sha256sum * > SHA256SUMS
    @echo "✅ Checksums generated in dist/SHA256SUMS"

# ============================================================================
# Documentation
# ============================================================================

# View man page
man: install
    man tree-navigator

# Generate HTML documentation from markdown
docs-html:
    @echo "📚 Generating HTML documentation..."
    @command -v pandoc >/dev/null 2>&1 || { echo "❌ pandoc not found"; exit 1; }
    pandoc README.md -o docs/README.html
    pandoc CONTRIBUTING.md -o docs/CONTRIBUTING.html
    pandoc CHANGELOG.md -o docs/CHANGELOG.html
    @echo "✅ HTML docs generated in docs/"

# Preview README in browser
docs-preview:
    @command -v glow >/dev/null 2>&1 && glow README.md || cat README.md

# Build wiki locally (requires gollum)
wiki:
    @echo "📖 Starting wiki server..."
    @command -v gollum >/dev/null 2>&1 || { echo "❌ Install: gem install gollum"; exit 1; }
    cd wiki && gollum
    @echo "🌐 Wiki available at http://localhost:4567"

# ============================================================================
# Development
# ============================================================================

# Format check (verify no tabs, line endings)
fmt-check:
    @echo "🔍 Checking formatting..."
    @! find src -name "*.ad[sb]" -exec grep -l $$'\t' {} \; | grep -q . || \
        { echo "❌ Tabs found"; exit 1; }
    @! find src -name "*.ad[sb]" -exec file {} \; | grep -q CRLF || \
        { echo "❌ CRLF line endings found"; exit 1; }
    @echo "✅ Formatting is good"

# Count lines of code
loc:
    @echo "📊 Lines of code:"
    @find src -name "*.ad[sb]" -exec wc -l {} + | tail -1
    @echo ""
    @echo "By file:"
    @wc -l src/*.ad[sb]

# Show project statistics
stats:
    @echo "📊 Project Statistics"
    @echo "===================="
    @echo ""
    @echo "Source files:"
    @find src -name "*.ad[sb]" | wc -l
    @echo ""
    @echo "Lines of code:"
    @find src -name "*.ad[sb]" -exec cat {} \; | wc -l
    @echo ""
    @echo "Blank lines:"
    @find src -name "*.ad[sb]" -exec grep -c '^[[:space:]]*$$' {} + | awk '{s+=$1} END {print s}'
    @echo ""
    @echo "Comments:"
    @find src -name "*.ad[sb]" -exec grep -c '^\s*--' {} + | awk '{s+=$1} END {print s}'
    @echo ""
    @echo "Package files:"
    @ls -1 packaging/*/ 2>/dev/null | wc -l

# Watch files and rebuild on change (requires entr)
watch:
    @echo "👀 Watching for changes..."
    @command -v entr >/dev/null 2>&1 || { echo "❌ Install: sudo dnf install entr"; exit 1; }
    find src -name "*.ad[sb]" tree_navigator.gpr | entr -c just build

# ============================================================================
# Git & Release
# ============================================================================

# Prepare for commit (format, test, check)
pre-commit: fmt-check check test
    @echo "✅ Ready to commit"

# Create a new release (tag and build)
release-create VERSION:
    @echo "🚀 Creating release {{VERSION}}..."
    @git tag -a v{{VERSION}} -m "Release {{VERSION}}"
    @echo "✅ Tagged v{{VERSION}}"
    @echo ""
    @echo "Next steps:"
    @echo "  1. git push origin v{{VERSION}}"
    @echo "  2. just package-all"
    @echo "  3. Upload packages to GitLab releases"

# Show git status
status:
    @git status

# Show recent commits
log:
    @git log --oneline -10

# ============================================================================
# Deployment
# ============================================================================

# Deploy to staging
deploy-staging: package-all
    @echo "🚀 Deploying to staging..."
    @echo "⚠️  Not implemented yet"

# Deploy to production (requires tag)
deploy-production:
    @echo "🚀 Deploying to production..."
    @echo "⚠️  Only run on tagged releases!"
    @echo "⚠️  Not implemented yet"

# Upload packages to repositories
upload-packages: package-all
    @echo "📤 Uploading packages..."
    ./scripts/packaging/upload-to-repos.sh

# ============================================================================
# CI/CD
# ============================================================================

# Run CI pipeline locally (requires gitlab-runner)
ci-local:
    @echo "🔄 Running CI pipeline locally..."
    @command -v gitlab-runner >/dev/null 2>&1 || { echo "❌ Install gitlab-runner"; exit 1; }
    gitlab-runner exec docker build:debug
    gitlab-runner exec docker test:functional

# Validate CI configuration
ci-lint:
    @echo "🔍 Validating .gitlab-ci.yml..."
    @command -v gitlab-ci-lint >/dev/null 2>&1 || { echo "❌ gitlab-ci-lint not found"; exit 1; }
    gitlab-ci-lint .gitlab-ci.yml

# ============================================================================
# Docker
# ============================================================================

# Build Docker image
docker-build:
    @echo "🐳 Building Docker image..."
    docker build -t tree-navigator:latest .
    @echo "✅ Docker image built"

# Run in Docker container
docker-run: docker-build
    docker run --rm -v $(pwd):/workspace tree-navigator:latest \
        --export /workspace/docker-output.txt --max-depth 5

# Push Docker image to registry
docker-push: docker-build
    @echo "📤 Pushing to Docker registry..."
    docker tag tree-navigator:latest registry.gitlab.com/hyperpolymath/tree-navigator:latest
    docker push registry.gitlab.com/hyperpolymath/tree-navigator:latest

# ============================================================================
# Utilities
# ============================================================================

# Clean everything (build, packages, caches)
clean-all: clean
    @echo "🧹 Deep cleaning..."
    rm -rf dist/ build/ *.tar.gz *.deb *.rpm *.AppImage
    rm -rf test_output/ bench_data/
    @echo "✅ All clean"

# Show disk usage
disk-usage:
    @echo "💾 Disk usage:"
    @du -sh . obj/ bin/ 2>/dev/null || true

# Create directory structure for new feature
scaffold FEATURE:
    @echo "📁 Creating scaffold for {{FEATURE}}..."
    @mkdir -p src/{{FEATURE}}
    @touch src/{{FEATURE}}/{{FEATURE}}.ads
    @touch src/{{FEATURE}}/{{FEATURE}}.adb
    @echo "✅ Created src/{{FEATURE}}/"

# Show help for just commands
help:
    @just --list --unsorted

# Show system information
sysinfo:
    @echo "🖥️  System Information"
    @echo "===================="
    @echo "GNAT: $(gnat --version | head -1)"
    @echo "GPRbuild: $(gprbuild --version | head -1)"
    @echo "OS: $(uname -s)"
    @echo "Architecture: $(uname -m)"
    @echo "Cores: $(nproc)"

# ============================================================================
# Aliases
# ============================================================================

alias b := build
alias r := run
alias t := test
alias c := clean
alias i := install
alias u := uninstall
alias p := package-all
alias d := docs-preview
alias s := status
alias l := log

# ============================================================================
# CI/CD Recipes (used by GitLab CI)
# ============================================================================

# Recipe for CI: lint stage
ci-lint-stage: fmt-check check

# Recipe for CI: build stage
ci-build-stage: build

# Recipe for CI: test stage
ci-test-stage: test-full

# Recipe for CI: package stage
ci-package-stage: package-all

# Recipe for CI: deploy stage
ci-deploy-stage:
    @echo "🚀 CI Deploy stage"
    @echo "Not implemented yet"

# Install with short aliases 'tn' and 'treenav'
install-tn: release
    @echo "📦 Installing Tree Navigator with aliases..."
    sudo install -m 755 bin/main /usr/local/bin/tn
    sudo ln -sf /usr/local/bin/tn /usr/local/bin/treenav
    sudo ln -sf /usr/local/bin/tn /usr/local/bin/tree-navigator
    sudo mkdir -p /usr/local/share/man/man1
    sudo cp man/tree-navigator.1 /usr/local/share/man/man1/
    sudo ln -sf tree-navigator.1.gz /usr/local/share/man/man1/tn.1.gz 2>/dev/null || true
    sudo ln -sf tree-navigator.1.gz /usr/local/share/man/man1/treenav.1.gz 2>/dev/null || true
    sudo gzip -f /usr/local/share/man/man1/tree-navigator.1
    sudo mandb 2>/dev/null || true
    @echo "✅ Installed!"
    @echo ""
    @echo "Commands available:"
    @echo "  tn             # shortest"
    @echo "  treenav        # medium"
    @echo "  tree-navigator # full name"
    @echo ""
    @echo "Try: tn --help"

# Uninstall tn, treenav, and tree-navigator
uninstall-tn:
    @echo "🗑️  Uninstalling Tree Navigator..."
    sudo rm -f /usr/local/bin/tn
    sudo rm -f /usr/local/bin/treenav
    sudo rm -f /usr/local/bin/tree-navigator
    sudo rm -f /usr/local/share/man/man1/tree-navigator.1.gz
    sudo rm -f /usr/local/share/man/man1/tn.1.gz
    sudo rm -f /usr/local/share/man/man1/treenav.1.gz
    sudo mandb 2>/dev/null || true
    @echo "✅ Uninstalled"

# Run panic-attacker pre-commit scan
assail:
    @command -v panic-attack >/dev/null 2>&1 && panic-attack assail . || echo "panic-attack not found — install from https://github.com/hyperpolymath/panic-attacker"

# Self-diagnostic — checks dependencies, permissions, paths
doctor:
    @echo "Running diagnostics for tree-navigator..."
    @echo "Checking required tools..."
    @command -v just >/dev/null 2>&1 && echo "  [OK] just" || echo "  [FAIL] just not found"
    @command -v git >/dev/null 2>&1 && echo "  [OK] git" || echo "  [FAIL] git not found"
    @echo "Checking for hardcoded paths..."
    @grep -rn '$HOME\|$ECLIPSE_DIR' --include='*.rs' --include='*.ex' --include='*.res' --include='*.gleam' --include='*.sh' . 2>/dev/null | head -5 || echo "  [OK] No hardcoded paths"
    @echo "Diagnostics complete."

# Auto-repair common issues
heal:
    @echo "Attempting auto-repair for tree-navigator..."
    @echo "Fixing permissions..."
    @find . -name "*.sh" -exec chmod +x {} \; 2>/dev/null || true
    @echo "Cleaning stale caches..."
    @rm -rf .cache/stale 2>/dev/null || true
    @echo "Repair complete."

# Guided tour of key features
tour:
    @echo "=== tree-navigator Tour ==="
    @echo ""
    @echo "1. Project structure:"
    @ls -la
    @echo ""
    @echo "2. Available commands: just --list"
    @echo ""
    @echo "3. Read README.adoc for full overview"
    @echo "4. Read EXPLAINME.adoc for architecture decisions"
    @echo "5. Run 'just doctor' to check your setup"
    @echo ""
    @echo "Tour complete! Try 'just --list' to see all available commands."

# Open feedback channel with diagnostic context
help-me:
    @echo "=== tree-navigator Help ==="
    @echo "Platform: $(uname -s) $(uname -m)"
    @echo "Shell: $SHELL"
    @echo ""
    @echo "To report an issue:"
    @echo "  https://github.com/hyperpolymath/tree-navigator/issues/new"
    @echo ""
    @echo "Include the output of 'just doctor' in your report."
