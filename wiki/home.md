# 🌲 Tree Navigator Wiki

Welcome to the Tree Navigator documentation! This wiki provides comprehensive guides for using, developing, and deploying Tree Navigator.

## Quick Links

### For Users
- **[Getting Started](Getting-Started)** - Installation and first steps
- **[User Guide](User-Guide)** - Complete usage documentation
- **[CLI Reference](CLI-Reference)** - Command-line options and examples
- **[Troubleshooting](Troubleshooting)** - Common issues and solutions
- **[FAQ](FAQ)** - Frequently asked questions

### For Developers
- **[Development Setup](Development-Setup)** - Set up your development environment
- **[Architecture](Architecture)** - System design and structure
- **[Contributing Guide](Contributing)** - How to contribute
- **[Coding Standards](Coding-Standards)** - Ada conventions and style guide
- **[Testing Guide](Testing)** - Writing and running tests
- **[Release Process](Release-Process)** - How releases are created

### For DevOps
- **[CI/CD Pipeline](CI-CD-Pipeline)** - GitLab CI/CD configuration
- **[Deployment Guide](Deployment)** - Deploy to various platforms
- **[Monitoring](Monitoring)** - Observability and monitoring setup
- **[Security](Security)** - Security best practices

### Reference
- **[API Documentation](API-Documentation)** - Ada package documentation
- **[Configuration](Configuration)** - Configuration options
- **[File Formats](File-Formats)** - Input/output file formats
- **[Changelog](Changelog)** - Version history

## About Tree Navigator

Tree Navigator is a powerful, type-safe directory tree visualization and export tool built with Ada 2022. It solves the common problem of overwhelming `tree` output in large projects by providing:

- 🎯 Smart filtering (exclude directories/files by name or pattern)
- 🔍 Type-based selection (show only executables, configs, scripts, etc.)
- 📊 Statistics (file counts, sizes, exclusion metrics)
- 💾 Clean export (save filtered trees to files)
- 🛡️ Robust error handling
- ⚡ Compile-time safety via Ada

## Project Links

- **Repository:** [gitlab.com/YOUR_GROUP/tree-navigator](https://gitlab.com/YOUR_GROUP/tree-navigator)
- **Issues:** [Issue Tracker](https://gitlab.com/YOUR_GROUP/tree-navigator/-/issues)
- **CI/CD:** [Pipelines](https://gitlab.com/YOUR_GROUP/tree-navigator/-/pipelines)
- **License:** [MIT License](https://gitlab.com/YOUR_GROUP/tree-navigator/-/blob/main/LICENSE)

## Quick Start
```bash
# Install from package
sudo dpkg -i tree-navigator_2.0.0_amd64.deb

# Or build from source
git clone https://gitlab.com/YOUR_GROUP/tree-navigator.git
cd tree-navigator
gprbuild -P tree_navigator.gpr
sudo ./scripts/install.sh

# Export a directory tree
tree-navigator --export output.txt --max-depth 5

# Interactive navigation
tree-navigator
```

## Community

- **Report bugs:** [Issue Tracker](https://gitlab.com/YOUR_GROUP/tree-navigator/-/issues)
- **Suggest features:** [Feature Requests](https://gitlab.com/YOUR_GROUP/tree-navigator/-/issues?label_name%5B%5D=feature)
- **Contribute:** See [Contributing Guide](Contributing)

## Support

Need help? Check these resources:
1. [FAQ](FAQ) - Common questions
2. [Troubleshooting](Troubleshooting) - Problem solving
3. [Issues](https://gitlab.com/YOUR_GROUP/tree-navigator/-/issues) - Report bugs
4. [Discussions](https://gitlab.com/YOUR_GROUP/tree-navigator/-/issues?label_name%5B%5D=discussion) - Ask questions

---

_Last updated: 2024-11-06 | Version: 2.0.0_
