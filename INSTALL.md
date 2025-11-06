# Tree Navigator Installation Guide

Complete installation instructions for all platforms and methods.

## Quick Install

### Debian/Ubuntu
```bash
# From PPA (recommended)
sudo add-apt-repository ppa:hyperpolymath/tree-navigator
sudo apt update
sudo apt install tree-navigator

# From DEB package
wget https://gitlab.com/hyperpolymath/tree-navigator/-/releases/v2.0.0/downloads/tree-navigator_2.0.0_amd64.deb
sudo dpkg -i tree-navigator_2.0.0_amd64.deb
```

### Fedora/RHEL/CentOS
```bash
# From Copr (recommended)
sudo dnf copr enable hyperpolymath/tree-navigator
sudo dnf install tree-navigator

# From RPM package
sudo dnf install https://gitlab.com/hyperpolymath/tree-navigator/-/releases/v2.0.0/downloads/tree-navigator-2.0.0-1.x86_64.rpm
```

### Arch Linux
```bash
# From AUR
yay -S tree-navigator

# Or manually
git clone https://aur.archlinux.org/tree-navigator.git
cd tree-navigator
makepkg -si
```

### macOS
```bash
# Homebrew
brew install tree-navigator

# MacPorts
sudo port install tree-navigator
```

### Flatpak (Universal)
```bash
flatpak install flathub com.gitlab.hyperpolymath.TreeNavigator
flatpak run com.gitlab.hyperpolymath.TreeNavigator
```

### Snap (Universal)
```bash
sudo snap install tree-navigator
```

### From Source
```bash
# Install dependencies
sudo dnf install gcc-gnat gprbuild  # Fedora
sudo apt install gnat-13 gprbuild   # Debian/Ubuntu

# Clone and build
git clone https://gitlab.com/hyperpolymath/tree-navigator.git
cd tree-navigator
gprbuild -P tree_navigator.gpr -XBuild_Mode=release

# Install
sudo cp bin/main /usr/local/bin/tn
sudo cp man/tree-navigator.1 /usr/local/share/man/man1/
sudo gzip /usr/local/share/man/man1/tree-navigator.1
sudo mandb
```

## Shell Alias Setup

Tree Navigator installs as `tree-navigator`, but we recommend the short alias **`tn`** (Tree Navigator).

### Why `tn`?

- ✅ Short and memorable
- ✅ Doesn't conflict with existing commands
- ✅ Meaningful: **T**ree **N**avigator
- ✅ Fast to type
- ✅ Consistent across all shells

### Automatic Alias Installation
```bash
# This adds the alias to your shell config automatically
tree-navigator --install-alias
```

This will detect your shell and add `alias tn='tree-navigator'` to the appropriate config file.

### Manual Alias Setup

#### Bash
```bash
echo "alias tn='tree-navigator'" >> ~/.bashrc
source ~/.bashrc
```

#### Zsh
```bash
echo "alias tn='tree-navigator'" >> ~/.zshrc
source ~/.zshrc
```

#### Fish
```bash
echo "alias tn='tree-navigator'" >> ~/.config/fish/config.fish
source ~/.config/fish/config.fish
```

#### Nushell
```bash
echo "alias tn = tree-navigator" >> ~/.config/nushell/config.nu
```

#### Tcsh
```bash
echo "alias tn tree-navigator" >> ~/.tcshrc
source ~/.tcshrc
```

#### Dash (add to profile)
```bash
echo "alias tn='tree-navigator'" >> ~/.profile
. ~/.profile
```

### Verify Installation
```bash
# Full command
tree-navigator --version

# With alias
tn --version

# Both should output:
# tree-navigator 2.0.0 (Ada 2022)
```

## Usage Examples
```bash
# Export directory tree
tn --export output.txt --max-depth 5

# Exclude common directories
tn --export clean.txt --exclude-dirs "node_modules,target,obj,bin"

# Show only executables
tn --export bins.txt --only-type executable

# Interactive mode
tn

# Show help
tn --help

# View man page
man tree-navigator
```

## System-Wide Installation (From Source)

For manual system-wide installation:
```bash
# After building
sudo install -m 755 bin/main /usr/local/bin/tn
sudo install -m 644 man/tree-navigator.1 /usr/local/share/man/man1/
sudo gzip -f /usr/local/share/man/man1/tree-navigator.1
sudo mandb

# Create symlink for full name
sudo ln -s /usr/local/bin/tn /usr/local/bin/tree-navigator
```

Now both `tn` and `tree-navigator` will work!

## User-Only Installation (No Root)
```bash
# Build
gprbuild -P tree_navigator.gpr -XBuild_Mode=release

# Install to user directory
mkdir -p ~/.local/bin
cp bin/main ~/.local/bin/tn

# Add to PATH (if not already)
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc

# Man page
mkdir -p ~/.local/share/man/man1
cp man/tree-navigator.1 ~/.local/share/man/man1/
gzip -f ~/.local/share/man/man1/tree-navigator.1

# Update MANPATH
echo 'export MANPATH="$HOME/.local/share/man:$MANPATH"' >> ~/.bashrc
source ~/.bashrc
```

## Uninstallation

### Package Manager
```bash
# Debian/Ubuntu
sudo apt remove tree-navigator

# Fedora/RHEL
sudo dnf remove tree-navigator

# Arch
sudo pacman -R tree-navigator

# Flatpak
flatpak uninstall com.gitlab.hyperpolymath.TreeNavigator

# Snap
sudo snap remove tree-navigator
```

### Manual
```bash
sudo rm -f /usr/local/bin/tn
sudo rm -f /usr/local/bin/tree-navigator
sudo rm -f /usr/local/share/man/man1/tree-navigator.1.gz
sudo mandb

# Remove alias from shell config
# Edit ~/.bashrc, ~/.zshrc, etc. and remove the line:
# alias tn='tree-navigator'
```

## Troubleshooting

### Command not found
```bash
# Check if installed
which tn
which tree-navigator

# Check PATH
echo $PATH

# Verify binary location
ls -l /usr/local/bin/tn
```

### Permission denied
```bash
# Make executable
chmod +x /usr/local/bin/tn
```

### Man page not found
```bash
# Update man database
sudo mandb

# Check MANPATH
echo $MANPATH

# Verify man page exists
ls -l /usr/local/share/man/man1/tree-navigator.1.gz
```

## Dependencies

### Runtime (for binary packages)
- libgnat-13 or libgnat-15

### Build (for source installation)
- GNAT Ada Compiler (GCC 13+)
- GPRbuild
- Make (optional)

## Updating

### Package Manager
```bash
# Debian/Ubuntu
sudo apt update && sudo apt upgrade tree-navigator

# Fedora/RHEL
sudo dnf upgrade tree-navigator

# Arch
yay -Syu tree-navigator
```

### From Source
```bash
cd tree-navigator
git pull origin main
gprbuild -P tree_navigator.gpr -XBuild_Mode=release
sudo cp bin/main /usr/local/bin/tn
```

## Platform-Specific Notes

### Fedora Silverblue / CoreOS
```bash
# Use Flatpak or toolbox
flatpak install com.gitlab.hyperpolymath.TreeNavigator

# Or in toolbox
toolbox create
toolbox enter
sudo dnf install tree-navigator
```

### NixOS
```nix
# In configuration.nix
environment.systemPackages = with pkgs; [
  tree-navigator
];
```

### Windows (WSL)
```bash
# Use Ubuntu/Debian instructions in WSL
sudo apt install tree-navigator
```

## Getting Help

- **Documentation:** https://gitlab.com/hyperpolymath/tree-navigator/-/wikis/home
- **Issues:** https://gitlab.com/hyperpolymath/tree-navigator/-/issues
- **Man Page:** `man tree-navigator`
- **Built-in Help:** `tn --help`

## Next Steps

After installation:
1. Read the [User Guide](https://gitlab.com/hyperpolymath/tree-navigator/-/wikis/User-Guide)
2. Try the examples: `tn --help`
3. Set up shell completion (if available)
4. Explore interactive mode: `tn`

---

**Installed successfully?** Share your experience or report issues at:
https://gitlab.com/hyperpolymath/tree-navigator/-/issues
