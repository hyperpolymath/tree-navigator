# Tree Navigator

[![MIT License](https://img.shields.io/badge/License-MIT-green.svg)](https://choosealicense.com/licenses/mit/)
[![GitHub Stars](https://img.shields.io/github/stars/hyperpolymath/tree-navigator.svg)](https://github.com/hyperpolymath/tree-navigator/stargazers)
[![GitHub Issues](https://img.shields.io/github/issues/hyperpolymath/tree-navigator.svg)](https://github.com/hyperpolymath/tree-navigator/issues)
[![GitHub Forks](https://img.shields.io/github/forks/hyperpolymath/tree-navigator.svg)](https://github.com/hyperpolymath/tree-navigator/network)
[![npm version](https://img.shields.io/npm/v/tree-navigator.svg)](https://www.npmjs.com/package/tree-navigator)

## Overview

**Tree Navigator** is a powerful library for navigating, visualizing, and manipulating hierarchical tree structures. Whether you're working with DOM trees, file systems, organizational charts, decision trees, or any hierarchical data, Tree Navigator provides intuitive APIs and visual tools to traverse, search, and transform tree structures efficiently.

## Features

- **Flexible Tree Traversal**: Pre-order, in-order, post-order, level-order (BFS), and DFS traversal
- **Visual Rendering**: Beautiful ASCII and graphical tree visualizations
- **Search & Filter**: Find nodes by value, depth, path, or custom predicates
- **Tree Manipulation**: Insert, delete, move, and restructure nodes
- **Path Navigation**: Navigate using path expressions (e.g., `/root/child/grandchild`)
- **Tree Transformation**: Map, filter, and reduce operations on tree structures
- **Multiple Formats**: Import/export JSON, XML, YAML, and custom formats
- **Performance Optimized**: Efficient algorithms for large tree structures
- **TypeScript Support**: Full type definitions for enhanced development experience
- **Plugin System**: Extend functionality with custom plugins

## Installation

### npm

```bash
npm install tree-navigator
```

### yarn

```bash
yarn add tree-navigator
```

### CDN

```html
<script src="https://cdn.jsdelivr.net/npm/tree-navigator@latest/dist/tree-navigator.min.js"></script>
```

## Quick Start

### Basic Usage

```javascript
const { TreeNavigator } = require('tree-navigator');

// Create a tree
const tree = new TreeNavigator({
  value: 'root',
  children: [
    {
      value: 'child1',
      children: [
        { value: 'grandchild1' },
        { value: 'grandchild2' }
      ]
    },
    {
      value: 'child2',
      children: [
        { value: 'grandchild3' }
      ]
    }
  ]
});

// Navigate the tree
const node = tree.find('grandchild2');
console.log(node.parent.value); // 'child1'

// Visualize the tree
tree.visualize();
// Output:
// root
// ├── child1
// │   ├── grandchild1
// │   └── grandchild2
// └── child2
//     └── grandchild3
```

### TypeScript Example

```typescript
import { TreeNavigator, TreeNode } from 'tree-navigator';

interface CustomData {
  id: number;
  name: string;
  metadata?: Record<string, any>;
}

const tree = new TreeNavigator<CustomData>({
  value: { id: 1, name: 'root' },
  children: [
    {
      value: { id: 2, name: 'child1' },
      children: []
    }
  ]
});

// Type-safe operations
const node = tree.findById(2);
console.log(node.value.name); // 'child1'
```

## Core Features

### 1. Tree Traversal

```javascript
// Depth-First Search (Pre-order)
tree.traverse('dfs-preorder', (node) => {
  console.log(node.value);
});

// Breadth-First Search (Level-order)
tree.traverse('bfs', (node) => {
  console.log(node.value);
});

// Post-order traversal
tree.traverse('dfs-postorder', (node) => {
  console.log(node.value);
});

// Custom traversal with conditions
tree.traverse('dfs', (node) => {
  if (node.depth < 3) {
    console.log(node.value);
    return true; // Continue
  }
  return false; // Stop traversing this branch
});
```

### 2. Search & Filter

```javascript
// Find by value
const node = tree.find('grandchild2');

// Find by predicate
const nodes = tree.findAll((node) => node.value.startsWith('child'));

// Find by path
const node = tree.navigate('/root/child1/grandchild1');

// Find by depth
const nodesAtDepth2 = tree.findByDepth(2);

// Get all leaf nodes
const leaves = tree.getLeaves();

// Get all ancestors
const ancestors = node.getAncestors();

// Get all descendants
const descendants = node.getDescendants();
```

### 3. Tree Manipulation

```javascript
// Add child node
const parent = tree.find('child1');
parent.addChild({ value: 'new-grandchild' });

// Insert before
const sibling = tree.find('grandchild2');
sibling.insertBefore({ value: 'inserted-node' });

// Move node
const nodeToMove = tree.find('grandchild3');
nodeToMove.moveTo(parent);

// Remove node
const nodeToRemove = tree.find('grandchild1');
nodeToRemove.remove();

// Clone subtree
const cloned = node.clone();
```

### 4. Tree Transformation

```javascript
// Map values
const transformed = tree.map((node) => ({
  ...node.value,
  processed: true
}));

// Filter nodes
const filtered = tree.filter((node) => node.depth <= 2);

// Reduce tree to single value
const sum = tree.reduce((acc, node) => acc + node.value.count, 0);

// Flatten tree to array
const flattened = tree.toArray();
```

### 5. Visualization

```javascript
// ASCII visualization
tree.visualize();

// Custom visualization
tree.visualize({
  format: 'unicode',
  showDepth: true,
  showIndex: true
});

// HTML visualization
const html = tree.toHTML({
  className: 'tree-view',
  collapsible: true
});

// SVG visualization
const svg = tree.toSVG({
  width: 800,
  height: 600,
  orientation: 'horizontal'
});

// Mermaid diagram
const mermaid = tree.toMermaid();
```

## Advanced Features

### Path-Based Navigation

```javascript
// XPath-like syntax
const node = tree.select('/root/child1[0]/grandchild1');

// Wildcard support
const nodes = tree.selectAll('/root/*/grandchild*');

// Attribute matching
const nodes = tree.selectAll('//*[@type="important"]');
```

### Tree Statistics

```javascript
const stats = tree.getStatistics();
console.log(stats);
// {
//   totalNodes: 6,
//   depth: 2,
//   leaves: 3,
//   branches: 3,
//   averageBranchingFactor: 2,
//   balanced: false
// }
```

### Comparison & Diff

```javascript
const tree1 = new TreeNavigator({ value: 'root', children: [...] });
const tree2 = new TreeNavigator({ value: 'root', children: [...] });

// Compare trees
const isEqual = tree1.equals(tree2);

// Get diff
const diff = tree1.diff(tree2);
console.log(diff.added);    // Nodes added in tree2
console.log(diff.removed);  // Nodes removed from tree1
console.log(diff.modified); // Nodes with changed values
```

### Serialization

```javascript
// To JSON
const json = tree.toJSON();

// From JSON
const tree = TreeNavigator.fromJSON(json);

// To XML
const xml = tree.toXML();

// To YAML
const yaml = tree.toYAML();

// To DOT (Graphviz)
const dot = tree.toDOT();
```

## API Reference

### TreeNavigator

| Method | Description |
|--------|-------------|
| `find(value)` | Find first node with matching value |
| `findAll(predicate)` | Find all nodes matching predicate |
| `navigate(path)` | Navigate to node by path |
| `traverse(strategy, callback)` | Traverse tree with given strategy |
| `map(fn)` | Create new tree with mapped values |
| `filter(predicate)` | Create new tree with filtered nodes |
| `reduce(fn, initial)` | Reduce tree to single value |
| `visualize(options)` | Display tree visualization |
| `toArray()` | Convert tree to flat array |
| `toJSON()` | Serialize to JSON |

### TreeNode

| Property/Method | Description |
|-----------------|-------------|
| `value` | Node value |
| `children` | Array of child nodes |
| `parent` | Parent node reference |
| `depth` | Node depth in tree |
| `isLeaf()` | Check if node is leaf |
| `isRoot()` | Check if node is root |
| `addChild(node)` | Add child node |
| `remove()` | Remove node from tree |
| `moveTo(newParent)` | Move node to new parent |

## Use Cases

### File System Navigation

```javascript
const fileTree = new TreeNavigator({
  value: '/',
  type: 'directory',
  children: [
    {
      value: 'home',
      type: 'directory',
      children: [
        { value: 'user', type: 'directory', children: [] }
      ]
    },
    { value: 'etc', type: 'directory', children: [] }
  ]
});

// Find all directories
const directories = fileTree.findAll(node => node.type === 'directory');
```

### Organizational Chart

```javascript
const orgChart = new TreeNavigator({
  value: { name: 'CEO', department: 'Executive' },
  children: [
    {
      value: { name: 'CTO', department: 'Technology' },
      children: [
        { value: { name: 'Dev Lead', department: 'Engineering' } }
      ]
    }
  ]
});

orgChart.visualize();
```

### Decision Tree

```javascript
const decisionTree = new TreeNavigator({
  value: { question: 'Is it raining?', type: 'decision' },
  children: [
    {
      value: { answer: 'Yes', action: 'Take umbrella' },
      children: []
    },
    {
      value: { answer: 'No', action: 'Enjoy the sun' },
      children: []
    }
  ]
});
```

## Configuration

```javascript
const tree = new TreeNavigator(data, {
  // Custom ID field
  idField: 'id',

  // Custom children field
  childrenField: 'children',

  // Enable caching for better performance
  cache: true,

  // Maximum depth limit
  maxDepth: 10,

  // Custom comparator
  comparator: (a, b) => a.value === b.value
});
```

## CLI Tool

```bash
# Install globally
npm install -g tree-navigator-cli

# Visualize JSON tree
tree-nav visualize tree.json

# Convert formats
tree-nav convert tree.json --to xml

# Get statistics
tree-nav stats tree.json

# Search tree
tree-nav search tree.json --query "value=test"
```

## Documentation

- [Getting Started Guide](./docs/getting-started.md)
- [API Documentation](./docs/api.md)
- [Examples & Recipes](./docs/examples.md)
- [Visualization Guide](./docs/visualization.md)
- [Performance Optimization](./docs/performance.md)
- [Plugin Development](./docs/plugins.md)

## Contributing

Contributions are welcome! Please read our [Contributing Guidelines](CONTRIBUTING.md):

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/NewFeature`)
3. Write tests for your changes
4. Commit your changes (`git commit -m 'Add new feature'`)
5. Push to the branch (`git push origin feature/NewFeature`)
6. Open a Pull Request

## Testing

```bash
# Run all tests
npm test

# Run with coverage
npm run test:coverage

# Run specific test suite
npm test -- --grep "traversal"

# Run benchmarks
npm run benchmark
```

## Roadmap

- [ ] React/Vue component library for tree visualization
- [ ] Advanced animation support
- [ ] Undo/redo functionality
- [ ] Persistent tree storage adapters
- [ ] WebAssembly version for performance
- [ ] Graph algorithms (shortest path, etc.)
- [ ] Visual tree editor interface

## Performance

Tree Navigator is optimized for performance:
- Handles trees with 100,000+ nodes efficiently
- O(1) parent access
- O(log n) search with indexing
- Lazy loading support for large trees
- Memory-efficient serialization

## Browser Support

- Chrome (latest)
- Firefox (latest)
- Safari (latest)
- Edge (latest)
- Node.js 14+

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Author

**hyperpolymath**
- GitHub: [@hyperpolymath](https://github.com/hyperpolymath)

## Keywords

tree structure, tree navigation, tree traversal, hierarchical data, data structures, tree visualization, DOM tree, file system tree, tree manipulation, graph traversal, BFS, DFS, tree algorithms, node navigation, tree rendering, ASCII tree, organizational chart, decision tree

## Resources

- [Tree Data Structure (Wikipedia)](https://en.wikipedia.org/wiki/Tree_(data_structure))
- [Tree Traversal Algorithms](https://en.wikipedia.org/wiki/Tree_traversal)
- [Visualization Examples](./examples)

## Acknowledgments

- Computer science community
- Open source contributors
- Algorithm researchers
- All developers working with tree structures

---

**Navigate your data hierarchies with ease!** Star this repository to support tree structure innovation!
