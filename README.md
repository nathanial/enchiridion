# Enchiridion

A terminal-based novel writing assistant with AI assistance for Lean 4.

Enchiridion provides a TUI (Terminal User Interface) for writing novels, leveraging AI to assist with the creative process.

## Dependencies

- [terminus](https://github.com/nathanial/terminus) - Terminal UI library
- [wisp](https://github.com/nathanial/wisp) - HTTP client for AI API communication

## Installation

This project depends on local workspace siblings. Clone the entire workspace:

```bash
git clone <workspace-url>
cd lean-workspace/enchiridion
lake build
```

## Building

```bash
lake build
```

## Running

```bash
lake exe enchiridion
```

## Running Tests

```bash
lake test
```

## Architecture

- `Enchiridion/Core.lean` - Core types and utilities
- `Enchiridion/Model.lean` - Data models for novels, chapters, scenes
- `Enchiridion/State.lean` - Application state management
- `Enchiridion/Storage.lean` - Persistence layer
- `Enchiridion/AI.lean` - AI integration for writing assistance
- `Enchiridion/UI.lean` - Terminal UI components

## License

MIT License - see [LICENSE](LICENSE) for details.
