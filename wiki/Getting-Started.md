# Getting Started with Tree Navigator

This guide will help you install and start using Tree Navigator in minutes.

## Prerequisites

### Required
- **GNAT Ada Compiler** - GCC 12.0+ with Ada 2022 support
- **GPRbuild** - GNAT Project Manager

### For Building from Source
- **Git** - Version control
- **Make** - Build automation (optional but recommended)

## Installation Methods

### Method 1: Package Installation (Recommended)

#### Debian/Ubuntu
```bash
# Download the DEB package
wget https://gitlab.com/YOUR_GROUP/tree-navigator/-/releases/v2.0.0/downloads/tree-navigator_2.0.0_amd64.deb

# Install
sudo dpkg -i tree-navigator_2.0.0_amd64.deb

# Verify
tree-navigator --version
```

#### Fedora/RHEL
```bash
# Download the RPM package
wget https://gitlab.com/YOUR_GROUP/tree-navigator/-/releases/v2.0.0/downloads/tree-navigator-2.0.0-1.x86_64.rpm

# Install
sudo dnf install tree-navigator-2.0.0-1.x86_64.rpm

# Verify
tree-navigator --version
```

### Method 2: Build from Source
```bash
# Clone repository
git clone https://gitlab.com/YOUR_GROUP/tree-navigator.git
cd tree-navigator

# Install dependencies (Fedora)
sudo dnf install gcc-gnat gprbuild

# Or (Debian/Ubuntu)
sudo apt-get install gnat-13 gprbuild

# Build
gprbuild -P tree_navigator.gpr

# Test
./bin/main --version

# Install system-wide
sudo ./scripts/install.sh
```

### Method 3: Docker
```bash
# Pull image
docker pull registry.gitlab.com/YOUR_GROUP/tree-navigator:latest

# Run
docker run --rm -v $(pwd):/workspace registry.gitlab.com/YOUR_GROUP/tree-navigator \
  --export /workspace/tree.txt --max-depth 5
```

## First Steps

### 1. Verify Installation
```bash
# Check version
tree-navigator --version

# View help
tree-navigator --help

# Read man page
man tree-navigator
```

### 2. Basic Usage

Export your current directory:
```bash
tree-navigator --export my-tree.txt
```

View the output:
```bash
cat my-tree.txt
```

### 3. Try Different Options

Limit depth:
```bash
tree-navigator --export tree.txt --max-depth 3
```

Exclude directories:
```bash
tree-navigator --export tree.txt --exclude-dirs "node_modules,target"
```

Show file sizes:
```bash
tree-navigator --export tree.txt --show-size
```

### 4. Interactive Mode

Start interactive navigation:
```bash
tree-navigator
```

Controls:
- Type a directory name to enter it
- `..` to go up one level
- `~` to go home
- `/` to go to root
- `q` to quit

## Common Use Cases

### Document Project Structure
```bash
# Create clean tree for README
tree-navigator --export project-structure.txt \
  --exclude-dirs "obj,bin,target,node_modules" \
  --max-depth 4
```

### Find All Executables
```bash
tree-navigator --export binaries.txt \
  --only-type executable \
  --show-size
```

### Audit Configuration Files
```bash
tree-navigator --export configs.txt \
  --only-type config \
  --show-hidden
```

## Next Steps

- **[User Guide](User-Guide)** - Learn all features
- **[CLI Reference](CLI-Reference)** - Complete command reference
- **[Examples](Examples)** - More usage examples
- **[Configuration](Configuration)** - Customize behavior

## Getting Help

If you encounter issues:
1. Check [Troubleshooting](Troubleshooting)
2. Review [FAQ](FAQ)
3. Search [existing issues](https://gitlab.com/YOUR_GROUP/tree-navigator/-/issues)
4. [Open a new issue](https://gitlab.com/YOUR_GROUP/tree-navigator/-/issues/new)

---

_Next: [User Guide](User-Guide) →_
