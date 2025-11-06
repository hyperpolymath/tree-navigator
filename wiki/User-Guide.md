# Tree Navigator User Guide

Complete guide to using Tree Navigator effectively.

## Table of Contents

1. [Export Mode](#export-mode)
2. [Interactive Mode](#interactive-mode)
3. [Filtering](#filtering)
4. [File Type Selection](#file-type-selection)
5. [Output Formats](#output-formats)
6. [Advanced Usage](#advanced-usage)

## Export Mode

Export mode is the primary feature - it creates filtered directory tree files.

### Basic Export
```bash
# Export current directory
tree-navigator --export output.txt

# Export specific directory
tree-navigator --export output.txt --dir /path/to/project

# Limit depth
tree-navigator --export output.txt --max-depth 5
```

### Output Information

Each export includes:
- Directory tree visualization
- Statistics (file/directory counts)
- Total size
- Exclusion counts
- Timestamp

Example output:
```
Directory tree: /home/user/project
Generated: 2024-11-06 10:30:45
Max depth: 5
================================================================================

📁 project
├── 📁 src
│   ├── 📄 main.adb
│   └── 📄 config.ads
└── 📄 README.md

================================================================================
Statistics:
  Directories:  2
  Files:        3
  Total size:   12345 bytes
```

## Interactive Mode

Navigate directories interactively with depth control.

### Starting Interactive Mode
```bash
# Default depth (10 levels)
tree-navigator

# Custom depth
tree-navigator 5
tree-navigator --depth 10
```

### Navigation Commands

| Command | Action |
|---------|--------|
| `dirname` | Enter directory |
| `..` | Go up one level |
| `/` | Jump to root |
| `~` | Go to home directory |
| `b` | Show bookmarks |
| `+` | Add bookmark |
| `q` | Quit |

### Bookmarks

Save frequently accessed locations:
```bash
# In interactive mode:
# 1. Navigate to directory
# 2. Press '+' 
# 3. Enter bookmark name
# 4. Press 'b' to view bookmarks
# 5. Enter bookmark name to jump there
```

## Filtering

Powerful filtering options for precise control.

### Exclude Directories
```bash
# Single directory
tree-navigator --export tree.txt --exclude-dirs node_modules

# Multiple directories (comma-separated)
tree-navigator --export tree.txt --exclude-dirs "obj,bin,target"

# Common exclusions
tree-navigator --export tree.txt \
  --exclude-dirs "node_modules,target,.git,build,dist,obj,bin"
```

### Exclude Files
```bash
# By extension
tree-navigator --export tree.txt --exclude-files "*.o,*.tmp"

# By name
tree-navigator --export tree.txt --exclude-files "*.log,*.bak,*~"

# Mixed
tree-navigator --export tree.txt \
  --exclude-files "*.o,*.ali,*.so,*.tmp,*.log"
```

### Depth Control
```bash
# Shallow (quick overview)
tree-navigator --export tree.txt --max-depth 2

# Medium (default)
tree-navigator --export tree.txt --max-depth 5

# Deep (detailed view)
tree-navigator --export tree.txt --max-depth 10
```

### Show/Hide Options
```bash
# Show hidden files (starting with .)
tree-navigator --export tree.txt --show-hidden

# Only directories (no files)
tree-navigator --export tree.txt --no-files

# Only files (no subdirectories)
tree-navigator --export tree.txt --no-dirs

# Show file sizes
tree-navigator --export tree.txt --show-size
```

## File Type Selection

Filter by specific file categories.

### Available Types

- `directory` - Directories only
- `executable` - Binary executables
- `config` - Configuration files (.conf, .cfg)
- `script` - Shell scripts (.sh, .bash)
- `data` - Data files (.json, .xml, .yaml)
- `hidden` - Hidden files (starting with .)
- `regular` - Regular files

### Type Filtering Examples
```bash
# Only executables
tree-navigator --export bins.txt --only-type executable

# Only configs
tree-navigator --export configs.txt --only-type config

# Only scripts
tree-navigator --export scripts.txt --only-type script

# Only data files
tree-navigator --export data.txt --only-type data
```

## Output Formats

### Text Output (Default)

Standard ASCII tree with Unicode icons:
```
📁 project
├── 📄 file.txt
└── ⚙️ script.sh
```

### With Statistics
```bash
tree-navigator --export tree.txt --show-size
```

Output includes file sizes:
```
📁 project
├── 📄 file.txt [1234 bytes]
└── ⚙️ script.sh [567 bytes]
```

## Advanced Usage

### Project Documentation

Create clean trees for README files:
```bash
# Rust project
tree-navigator --export structure.txt \
  --exclude-dirs "target,.git" \
  --exclude-files "Cargo.lock" \
  --max-depth 4

# Node.js project
tree-navigator --export structure.txt \
  --exclude-dirs "node_modules,dist,.next" \
  --exclude-files "package-lock.json,*.map" \
  --max-depth 5

# Ada/GNAT project
tree-navigator --export structure.txt \
  --exclude-dirs "obj,bin" \
  --exclude-files "*.ali,*.o" \
  --max-depth 3
```

### Security Audits

Find executables and scripts:
```bash
# All executables with sizes
tree-navigator --export executables.txt \
  --only-type executable \
  --show-size

# All scripts
tree-navigator --export scripts.txt \
  --only-type script \
  --show-hidden
```

### Configuration Management

List all configuration files:
```bash
tree-navigator --export all-configs.txt \
  --only-type config \
  --show-hidden
```

### Comparison Workflows
```bash
# Before cleanup
tree-navigator --export before.txt --max-depth 5

# After cleanup
tree-navigator --export after.txt --max-depth 5

# Compare
diff before.txt after.txt
```

## Performance Tips

### Large Directories
```bash
# Limit depth for speed
tree-navigator --export tree.txt --max-depth 3

# Exclude large directories
tree-navigator --export tree.txt \
  --exclude-dirs "node_modules,target,.git"
```

### Quick Scans
```bash
# Directories only (fastest)
tree-navigator --export dirs.txt --no-files --max-depth 5
```

## Tips & Tricks

### Pipe to Less
```bash
tree-navigator --export /dev/stdout | less
```

### Combine with Grep
```bash
tree-navigator --export tree.txt
grep "\.adb$" tree.txt  # Find all .adb files
```

### Automated Documentation
```bash
#!/bin/bash
# Generate fresh project structure daily
tree-navigator --export docs/structure.txt \
  --exclude-dirs "obj,bin,target" \
  --max-depth 4

git add docs/structure.txt
git commit -m "Update project structure"
```

## See Also

- **[CLI Reference](CLI-Reference)** - Complete option list
- **[Examples](Examples)** - Real-world examples
- **[Troubleshooting](Troubleshooting)** - Problem solving

---

_Next: [CLI Reference](CLI-Reference) →_
