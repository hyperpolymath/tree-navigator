# Tree Navigator Naming Conventions

## Project Naming

### Repository Name
**Use hyphens:** `tree-navigator`
- URLs: `gitlab.com/group/tree-navigator`
- Clone: `git clone git@gitlab.com:group/tree-navigator.git`
- Directory: `cd tree-navigator`

### GPR Project File
**Use hyphens:** `tree-navigator.gpr`
```bash
gprbuild -P tree-navigator.gpr
```

**Note:** Inside the GPR file, the project name can use underscores:
```ada
project Tree_Navigator is
   -- This is fine, it's Ada convention
end Tree_Navigator;
```

### Executable Name
**Use hyphens:** `tree-navigator`
```bash
./bin/main  # During development
/usr/local/bin/tree-navigator  # When installed
```

### Package Names
**Use hyphens:**
- DEB: `tree-navigator_2.0.0_amd64.deb`
- RPM: `tree-navigator-2.0.0-1.x86_64.rpm`
- Tarball: `tree-navigator-2.0.0-linux-x86_64.tar.gz`

## Ada Source Files

### File Names
**Use underscores:** (Ada standard convention)
```
src/
├── main.adb
├── tree_printer.ads
├── tree_printer.adb
├── file_types.ads
├── file_types.adb
└── ...
```

### Package Names in Code
**Use underscores:** (Ada requirement)
```ada
package Tree_Printer is
package File_Types is
```

### Directory Structure
```
tree-navigator/           ← hyphen (repository)
├── tree-navigator.gpr    ← hyphen (project file)
├── src/
│   ├── tree_printer.ads  ← underscore (Ada source)
│   └── tree_printer.adb  ← underscore (Ada source)
├── obj/                  ← no separator needed
├── bin/
│   └── main              ← no separator needed
└── man/
    └── tree-navigator.1  ← hyphen (command name)
```

## Docker & Containers

**Use hyphens:**
```bash
docker build -t tree-navigator:latest .
docker run tree-navigator --help
```

**Registry:**
```
registry.gitlab.com/group/tree-navigator:latest
```

## Environment Variables

**Use underscores (POSIX convention):**
```bash
TREE_NAVIGATOR_CONFIG=/etc/tree-navigator/config
TREE_NAVIGATOR_CACHE_DIR=/var/cache/tree-navigator
```

## Configuration Files

**Use hyphens:**
```
~/.config/tree-navigator/
├── config.toml
├── bookmarks.txt
└── history.txt
```

## URLs & Endpoints

**Use hyphens:**
```
https://tree-navigator.io
https://docs.tree-navigator.io
https://api.tree-navigator.io/v1/export
```

## Git References

**Use hyphens:**
```bash
# Branches
git checkout feature/tree-export
git checkout bugfix/memory-leak

# Tags
git tag v2.0.0-rc1
git tag v2.0.0-beta
```

## Summary

| Context | Convention | Example |
|---------|-----------|---------|
| Repository | hyphen | `tree-navigator` |
| GPR file | hyphen | `tree-navigator.gpr` |
| Ada sources | underscore | `tree_printer.ads` |
| Ada packages | underscore | `Tree_Printer` |
| Executable | hyphen | `tree-navigator` |
| Packages (DEB/RPM) | hyphen | `tree-navigator_2.0.0_amd64.deb` |
| Docker images | hyphen | `tree-navigator:latest` |
| Config dirs | hyphen | `~/.config/tree-navigator/` |
| Env vars | underscore | `TREE_NAVIGATOR_CONFIG` |
| URLs | hyphen | `tree-navigator.io` |
| Man pages | hyphen | `tree-navigator.1` |

## Why Different Conventions?

1. **Hyphens for user-facing names**: Easier to type, URL-friendly, modern convention
2. **Underscores for Ada**: Required by Ada language specification for file names
3. **Underscores for environment variables**: POSIX shell convention (hyphens not allowed)
4. **Consistency within each domain**: URLs all use hyphens, Ada all uses underscores

## Migration Checklist

When renaming from `tree_navigator` to `tree-navigator`:

- [ ] Rename repository on GitLab/GitHub
- [ ] Update local git remote URL
- [ ] Rename `.gpr` file
- [ ] Update all build scripts
- [ ] Update CI/CD configurations
- [ ] Update README and documentation
- [ ] Update Docker files
- [ ] Update deployment scripts
- [ ] Keep Ada source files unchanged (they're correct as-is)
- [ ] Clean and rebuild: `gprbuild -P tree-navigator.gpr`
- [ ] Test installation and man page
