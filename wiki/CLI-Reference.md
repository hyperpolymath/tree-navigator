# Command-Line Reference

Complete reference for all Tree Navigator command-line options.

## Synopsis
```
tree-navigator [OPTIONS] [DEPTH]
tree-navigator --export FILE [FILTER_OPTIONS]
```

## General Options

### `-h, --help`
Display help message and exit.

**Example:**
```bash
tree-navigator --help
```

### `-v, --version`
Display version information and exit.

**Example:**
```bash
tree-navigator --version
```

**Output:**
```
tree-navigator 2.0.0 (Ada 2022)
Copyright (c) 2024 Tree Navigator Contributors
Licensed under MIT License
```

### `--no-color`
Disable colored output (useful for piping or logging).

**Example:**
```bash
tree-navigator --export tree.txt --no-color
```

### `--verbose`
Enable verbose output with debug information.

**Example:**
```bash
tree-navigator --export tree.txt --verbose
```

## Export Mode Options

### `--export FILE`
Export directory tree to FILE (enables export mode).

**Example:**
```bash
tree-navigator --export output.txt
```

### `--output FILE`
Alternative way to specify output file.

**Example:**
```bash
tree-navigator --output myproject-tree.txt
```

### `--dir PATH`
Specify root directory to export (default: current directory).

**Example:**
```bash
tree-navigator --export tree.txt --dir /path/to/project
```

## Filtering Options

### `--max-depth N`
Maximum depth to traverse (default: 10).

**Range:** 1-100

**Examples:**
```bash
tree-navigator --export tree.txt --max-depth 3
tree-navigator --export tree.txt --max-depth 20
```

### `--show-hidden`
Include hidden files (starting with `.`).

**Example:**
```bash
tree-navigator --export tree.txt --show-hidden
```

### `--no-files`
Show only directories, exclude files.

**Example:**
```bash
tree-navigator --export dirs-only.txt --no-files
```

### `--no-dirs`
Show only files, exclude subdirectories.

**Example:**
```bash
tree-navigator --export files-only.txt --no-dirs
```

### `--show-size`
Display file sizes in bytes.

**Example:**
```bash
tree-navigator --export tree.txt --show-size
```

### `--exclude-dirs LIST`
Comma-separated list of directories to exclude.

**Format:** `--exclude-dirs "dir1,dir2,dir3"`

**Examples:**
```bash
# Single directory
tree-navigator --export tree.txt --exclude-dirs node_modules

# Multiple directories
tree-navigator --export tree.txt --exclude-dirs "obj,bin,target"

# Many directories
tree-navigator --export tree.txt \
  --exclude-dirs "node_modules,target,.git,build,dist,obj,bin,coverage"
```

### `--exclude-files LIST`
Comma-separated list of files or patterns to exclude.

**Format:** `--exclude-files "pattern1,pattern2,pattern3"`

**Supports:** Exact names and wildcard patterns (`*`)

**Examples:**
```bash
# By extension
tree-navigator --export tree.txt --exclude-files "*.o,*.tmp"

# By name
tree-navigator --export tree.txt --exclude-files "Makefile,*.lock"

# Mixed
tree-navigator --export tree.txt \
  --exclude-files "*.o,*.ali,*.so,*.log,*.bak,*~"
```

### `--only-type TYPE`
Show only specific file type.

**Available Types:**
- `directory` - Directories only
- `executable` - Binary executables (`.exe`, `.bin`, or executable permission)
- `config` - Configuration files (`.conf`, `.cfg`)
- `script` - Shell scripts (`.sh`, `.bash`)
- `data` - Data files (`.json`, `.xml`, `.yaml`, `.yml`)
- `hidden` - Hidden files (starting with `.`)
- `regular` - Regular files

**Examples:**
```bash
# Find all executables
tree-navigator --export bins.txt --only-type executable

# Find all configs
tree-navigator --export configs.txt --only-type config

# Find all scripts
tree-navigator --export scripts.txt --only-type script

# Find all data files
tree-navigator --export data.txt --only-type data
```

## Interactive Mode Options

### `-d N, --depth N`
Set maximum navigation depth for interactive mode.

**Example:**
```bash
tree-navigator --depth 5
```

### Positional Depth Argument
Alternative way to specify depth.

**Example:**
```bash
tree-navigator 10  # Navigate to depth 10
```

## Complete Examples

### Basic Exports
```bash
# Current directory, default settings
tree-navigator --export tree.txt

# Specific directory
tree-navigator --export tree.txt --dir ~/projects/myapp

# Limited depth
tree-navigator --export tree.txt --max-depth 5
```

### Project Documentation
```bash
# Rust project structure
tree-navigator --export rust-structure.txt \
  --dir ~/rust-project \
  --exclude-dirs "target,.git" \
  --exclude-files "Cargo.lock" \
  --max-depth 4

# Node.js project structure
tree-navigator --export node-structure.txt \
  --dir ~/node-project \
  --exclude-dirs "node_modules,dist,.next,coverage" \
  --exclude-files "package-lock.json,*.map" \
  --max-depth 5

# Python project structure
tree-navigator --export python-structure.txt \
  --dir ~/python-project \
  --exclude-dirs "__pycache__,.pytest_cache,venv,.venv" \
  --exclude-files "*.pyc,*.pyo" \
  --max-depth 4
```

### Security & Auditing
```bash
# Find all executables with sizes
tree-navigator --export executables-audit.txt \
  --only-type executable \
  --show-size \
  --show-hidden

# Find all configuration files
tree-navigator --export config-audit.txt \
  --only-type config \
  --show-hidden

# Find all scripts
tree-navigator --export scripts-audit.txt \
  --only-type script \
  --show-hidden
```

### Specialized Use Cases
```bash
# Only show directory structure (no files)
tree-navigator --export dir-structure.txt \
  --no-files \
  --max-depth 10

# Show everything including hidden files
tree-navigator --export complete-tree.txt \
  --show-hidden \
  --show-size \
  --max-depth 20

# Quick shallow scan
tree-navigator --export quick-overview.txt \
  --max-depth 2 \
  --no-files
```

## Environment Variables

Currently, Tree Navigator does not use environment variables for configuration.

## Exit Status

- `0` - Success
- `1` - General error
- `2` - Invalid arguments

## Files

### Configuration
- `~/.config/tree-navigator/` - Configuration directory
- `~/.config/tree-navigator/config.toml` - Configuration file
- `~/.config/tree-navigator/bookmarks.txt` - Saved bookmarks

### Cache
- `~/.cache/tree-navigator/` - Cache directory

## See Also

- **man tree-navigator** - Man page
- **[User Guide](User-Guide)** - Complete usage guide
- **[Examples](Examples)** - Real-world examples
- **[Troubleshooting](Troubleshooting)** - Problem solving

---

_Back: [User Guide](User-Guide) | Next: [Examples](Examples) →_
