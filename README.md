cat > README.md << 'EOF'
# 🌲 Tree Navigator

> A powerful, type-safe directory tree export and navigation tool written in Ada Owith its safety and compile time guarantees)

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Ada 2022](https://img.shields.io/badge/Ada-2022-blue.svg)](https://www.adacore.com/)
[![Build](https://img.shields.io/badge/build-passing-brightgreen.svg)]()

**Tree Navigator** is a professional-grade directory tree visualization and export tool that solves the common problem of overwhelming `tree` output in large projects (especially Rust, Node.js, and other projects with massive dependency directories). Built with Ada 2022, it provides compile-time safety, extensive filtering options, and clean, readable output.

## 🎯 Why Tree Navigator?

**The Problem:** Standard `tree` commands output thousands of lines for modern projects:
- Rust projects: `target/` directories with 50,000+ files
- Node.js projects: `node_modules/` with endless dependencies  
- Build artifacts cluttering your tree view
- Difficulty finding specific file types
- No easy way to filter by permissions or file attributes

**The Solution:** Tree Navigator gives you surgical control:
- 🎯 **Smart Filtering**: Exclude directories/files by name or pattern
- 🔍 **Type Selection**: Show only executables, configs, scripts, or data files
- 📊 **Statistics**: Get file counts, sizes, and exclusion metrics
- 💾 **Clean Export**: Save filtered trees to files for documentation
- 🛡️ **Robust**: Handle permission errors gracefully
- ⚡ **Fast**: Compile-time optimizations via Ada

## ✨ Features

### Core Capabilities
- 🎯 **Advanced Filtering** - Exclude specific directories and file patterns
- 📂 **Type-Based Selection** - Filter by file type (executable, config, script, data, etc.)
- 📊 **Statistics** - Automatic counting of files, directories, and total size
- 💾 **File Export** - Save tree output to files for documentation or analysis
- 🔍 **Hidden File Control** - Show or hide dotfiles
- 📏 **Depth Limiting** - Control how deep to traverse
- 📦 **Size Display** - Optional file size information
- 🎨 **Color-Coded** - Visual distinction with emoji icons
- 🛡️ **Permission Handling** - Graceful handling of access denied errors
- 🔖 **Interactive Mode** - Navigate and bookmark favorite locations

### File Type Detection
Tree Navigator automatically categorizes files:
- 📁 **Directories**
- ⚙️ **Executables** (`.exe`, `.bin`, executable permission)
- ⚙️ **System Configs** (`.conf`, `.cfg`)
- 📜 **Shell Scripts** (`.sh`, `.bash`)
- 📄 **Data Files** (`.json`, `.xml`, `.yaml`, `.yml`)
- 👻 **Hidden Files** (starting with `.`)
- 📄 **Regular Files** (everything else)

## 🚀 Quick Start

### Prerequisites

- **GNAT Ada Compiler**: GCC 12.0+ with Ada 2022 support
- **GPRbuild**: GNAT Project Manager

### Installation
```bash
# Clone the repository
git clone https://github.com/yourusername/tree-navigator.git
cd tree-navigator

# Build
gprbuild -P tree_navigator.gpr

# Optionally install system-wide
sudo cp bin/main /usr/local/bin/tree-navigator
```

## 📖 Usage

### Export Mode (Primary Use Case)

Export directory trees to files with powerful filtering:
```bash
# Basic export
tree-navigator --export output.txt

# Export with depth limit
tree-navigator --export output.txt --max-depth 5

# Export specific directory
tree-navigator --export output.txt --dir /path/to/project

# Show file sizes
tree-navigator --export output.txt --show-size
```

### Filtering Options

#### Exclude Directories
Perfect for ignoring build artifacts and dependencies:
```bash
# Rust project - exclude build directory
tree-navigator --export rust-tree.txt --exclude-dirs target

# Node.js - exclude dependencies
tree-navigator --export node-tree.txt --exclude-dirs node_modules

# Multiple exclusions
tree-navigator --export clean-tree.txt --exclude-dirs "target,node_modules,.git,build,dist"

# Ada/GNAT project
tree-navigator --export ada-tree.txt --exclude-dirs obj,bin
```

#### Exclude Files
Filter out specific files or patterns:
```bash
# Exclude compiled objects
tree-navigator --export source-tree.txt --exclude-files "*.o,*.ali,*.so"

# Exclude temporary files
tree-navigator --export clean-tree.txt --exclude-files "*.tmp,*.log,*.bak,*~"

# Exclude multiple patterns
tree-navigator --export code-tree.txt --exclude-files "*.o,*.exe,*.dll,*.pyc"
```

#### Filter by File Type
Show only specific categories of files:
```bash
# Only executables
tree-navigator --export bins.txt --only-type executable

# Only configuration files
tree-navigator --export configs.txt --only-type config

# Only shell scripts
tree-navigator --export scripts.txt --only-type script

# Only data files (JSON, XML, YAML)
tree-navigator --export data.txt --only-type data

# Only directories (no files)
tree-navigator --export dirs.txt --no-files

# Only files (no subdirectories)
tree-navigator --export files.txt --no-dirs
```

#### Hidden Files
```bash
# Include hidden files
tree-navigator --export full-tree.txt --show-hidden

# Show only hidden files
tree-navigator --export hidden-tree.txt --only-type hidden
```

### Real-World Examples

#### Rust Project Documentation
```bash
# Clean tree for README, excluding build artifacts
tree-navigator --export project-structure.txt \
  --exclude-dirs "target,.git" \
  --exclude-files "*.lock,*.toml" \
  --max-depth 4
```

#### Node.js Project
```bash
# Show project structure without dependencies
tree-navigator --export app-structure.txt \
  --exclude-dirs "node_modules,dist,.next,coverage" \
  --exclude-files "*.map,*.log" \
  --max-depth 5
```

#### Find All Executables
```bash
# Locate all binary files in a project
tree-navigator --export executables.txt \
  --only-type executable \
  --show-size
```

#### Configuration Audit
```bash
# List all config files for review
tree-navigator --export all-configs.txt \
  --only-type config \
  --show-hidden
```

#### Shell Script Inventory
```bash
# Find all shell scripts in the system
tree-navigator --export scripts.txt \
  --dir /usr/local/bin \
  --only-type script
```

### Interactive Navigation Mode

Navigate directories interactively with depth control:
```bash
# Start interactive mode
tree-navigator

# Start with depth limit
tree-navigator 5

# Or
tree-navigator --depth 10
```

**Interactive Commands:**
- `[dirname]` - Enter a directory
- `[..]` - Go up one level
- `[/]` - Jump to root
- `[~]` - Go to home
- `[b]` - Show bookmarks
- `[+]` - Add bookmark
- `[q]` - Quit

## 📋 Command-Line Reference

### General Options
```
-h, --help              Show help message
-v, --version           Show version information
--no-color              Disable colored output
--verbose               Enable verbose output
```

### Export Mode Options
```
--export FILE           Export tree to file (enables export mode)
--output FILE           Alternative way to specify output file
--dir PATH              Root directory to export (default: current)
```

### Filtering Options
```
--max-depth N           Maximum depth to traverse (default: 10)
--show-hidden           Include hidden files (starting with .)
--no-files              Only show directories
--no-dirs               Only show files  
--show-size             Display file sizes
--exclude-dirs LIST     Comma-separated list of directories to exclude
--exclude-files LIST    Comma-separated list of files/patterns to exclude
--only-type TYPE        Only show specific file type
                        Types: directory, executable, config, script,
                               data, hidden, regular
```

### Interactive Mode Options
```
-d, --depth N           Set maximum navigation depth
```

## 🎨 Output Format

Example output:
```
Directory tree: /home/user/project
Generated: 2024-11-06 10:30:45
Max depth: 5
================================================================================

📁 project
├── 📁 src
│   ├── 📄 main.adb
│   ├── 📄 config.ads
│   └── 📄 navigator.adb
├── 📁 bin
│   └── ⚙️ tree-navigator
├── ⚙️ Makefile
└── 📄 README.md

================================================================================
Statistics:
  Directories:  2
  Files:        5
  Total size:   45231 bytes
  Excluded dirs: 1
```

## 🏗️ Project Structure
```
tree-navigator/
├── src/
│   ├── main.adb              # CLI argument parsing & main logic
│   ├── navigator.ads/adb     # Interactive navigation
│   ├── tree_printer.ads/adb  # Tree export engine
│   ├── bookmarks.ads/adb     # Bookmark management
│   ├── config.ads/adb        # Configuration
│   ├── terminal.ads/adb      # Terminal I/O
│   └── file_types.ads/adb    # File categorization
├── bin/                      # Compiled executable
├── obj/                      # Build artifacts
├── tree_navigator.gpr        # GNAT project file
├── Makefile                  # Build automation
├── README.md                 # This file
├── LICENSE                   # MIT License
├── CONTRIBUTING.md           # Contribution guidelines
└── CHANGELOG.md              # Version history
```

## 🔧 Development

### Building from Source
```bash
# Clean build
gprclean -P tree_navigator.gpr
gprbuild -P tree_navigator.gpr

# Using Make
make clean
make build

# Run
./bin/main --help
```

### Running Tests
```bash
# Test basic export
./bin/main --export test.txt --max-depth 3

# Test with filters
./bin/main --export test.txt --exclude-dirs "obj,bin"

# Test file type filtering
./bin/main --export test.txt --only-type executable
```

## ⚙️ Configuration

Tree Navigator stores data in:
- **Config**: `~/.config/tree-navigator/`
- **Bookmarks**: `~/.config/tree-navigator/bookmarks.txt`
- **Cache**: `~/.cache/tree-navigator/`

## 🤝 Contributing

Contributions welcome! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Quick Start
1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Ensure it compiles cleanly
5. Submit a pull request

## 📝 License

MIT & Palimpsest Dual License - see [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **Ada Community** - Excellent language and tools
- **GNAT** - World-class Ada compiler
- **Contributors** - See [CONTRIBUTORS.md](CONTRIBUTORS.md)

## 📚 Additional Resources

- [Ada 2022 Reference](http://www.ada-auth.org/standards/22rm/html/RM-TOC.html)
- [GNAT User's Guide](https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn.html)
- [Ada Programming](https://en.wikibooks.org/wiki/Ada_Programming)

## 🐛 Bug Reports & Features

Found a bug or have a feature request? [Open an issue](https://github.com/yourusername/tree-navigator/issues)!

---