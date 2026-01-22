# Contributing to Tree Navigator

Thank you for your interest in contributing to Tree Navigator! This document provides guidelines and instructions for contributing.

## Development Setup

### Prerequisites

- GNAT Ada compiler (GCC 12.0 or later with Ada 2022 support)
- GPRbuild
- Git

### Building from Source
```bash
git clone https://github.com/yourusername/tree-navigator.git
cd tree-navigator
gprbuild -P tree_navigator.gpr
./bin/main
```

## Code Style

This project follows Ada 2022 standards with these conventions:

- **Indentation**: 3 spaces (Ada standard)
- **Line length**: Keep under 100 characters where practical
- **Naming**: Use `Snake_Case` for identifiers
- **Comments**: Use `--` for single-line comments
- **Pragmas**: Place `pragma Ada_2022;` at the top of each file

## Project Structure
```
tree-navigator/
├── src/           # Ada source files (.ads/.adb)
├── obj/           # Compiled objects (gitignored)
├── bin/           # Compiled executable (gitignored)
├── tree_navigator.gpr  # GNAT project file
└── README.md
```

## Testing

Before submitting a pull request:

1. Ensure your code compiles without errors:
```bash
   gprbuild -P tree_navigator.gpr
```

2. Test basic functionality:
```bash
   ./bin/main --help
   ./bin/main
```

3. Verify no new warnings are introduced

## Submitting Changes

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Pull Request Guidelines

- Provide a clear description of the changes
- Reference any related issues
- Ensure code compiles cleanly
- Update documentation if needed
- Add yourself to CONTRIBUTORS.md

## Code of Conduct

Be respectful, constructive, and professional in all interactions.

## Questions?

Open an issue for questions, bug reports, or feature requests.
