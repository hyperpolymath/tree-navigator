# Tree Navigator Wiki

This directory contains the GitLab Wiki pages for Tree Navigator.

## Structure

- `home.md` - Wiki home page
- `Getting-Started.md` - Installation and quick start
- `User-Guide.md` - Complete usage guide
- `CLI-Reference.md` - Command-line reference
- (More pages to be added)

## Viewing Locally
```bash
# Install gollum (Ruby gem for viewing wikis)
gem install gollum

# Start wiki server
cd wiki
gollum

# Open browser to http://localhost:4567
```

## Updating Wiki on GitLab

The wiki is automatically synced with the `wiki` directory in the repository.

### Manual Update
```bash
# Clone wiki separately
git clone https://gitlab.com/YOUR_GROUP/tree-navigator.wiki.git

# Or push from this directory
cd wiki
git init
git remote add origin https://gitlab.com/YOUR_GROUP/tree-navigator.wiki.git
git add .
git commit -m "Update wiki"
git push -u origin main
```

## Contributing

See [Contributing Guide](Contributing.md) for guidelines on updating documentation.
