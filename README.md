# Emacs Configuration

A modular, performance-optimized Emacs configuration for software development with 33+ specialized modules.

## Features

- **Modern Development Environment**: Eglot LSP, Tree-sitter, multiple language support
- **Smart Configuration Reloading**: Non-blocking reload with byte-compilation support
- **CUA Mode Integration**: Standard copy/paste keybindings with intelligent mode-specific handling
- **Emergency Recovery**: Built-in diagnostics and fixes for common issues
- **Completion System**: Company mode with multi-backend support
- **Applications**: Email (mu4e), RSS reader (Elfeed), Portfolio tracker

## Quick Start

### Installation

1. Clone this repository to `~/.config/emacs/`:
```bash
git clone <repository-url> ~/.config/emacs
```

2. Start Emacs. The configuration will automatically install required packages on first run.

### Key Bindings

#### Configuration Management
- `C-c C-r` - Reload configuration (non-blocking)
- `C-u C-c C-r` - Reload configuration (blocking)
- `C-c r` - Reload current file only
- `C-c R` - Fast reload using byte-compiled files

#### Emergency Fixes
- `C-c C-!` - Emergency restore editing capability
- `C-c C-?` - Diagnose editing issues

#### Development
- `M-x enable-dev-mode-modern` - Enable Eglot-based development
- `M-x enable-dev-mode` - Enable LSP-mode (legacy)

#### Applications
- `C-x w` - Open Elfeed RSS reader
- `C-c $` - Open portfolio tracker

## Architecture

### Directory Structure
```
~/.config/emacs/
├── init.el                 # Main initialization file
├── early-init.el          # Early startup optimization
├── lisp/                  # Configuration modules
│   ├── init-core.el      # Package management
│   ├── init-ui.el        # UI configuration
│   ├── init-editor.el    # Editor settings
│   ├── init-eglot.el     # LSP client
│   └── ...               # 30+ more modules
├── CLAUDE.md             # AI assistant guidance
└── README.md            # This file
```

### Module Categories

**Core System**
- Package management with use-package
- Company completion system
- CUA mode configuration
- UI themes and display settings

**Development Tools**
- Eglot (built-in LSP)
- Tree-sitter support
- Git integration (Magit)
- Project management

**Language Support**
- Python, JavaScript, TypeScript
- Go, Rust, C/C++
- Ruby, Java, Kotlin
- Markdown, YAML, JSON

**Applications**
- mu4e email client
- Elfeed RSS reader
- Portfolio tracker (somehwat work in progress)
- Org mode enhancements

## Commands Reference

### Configuration Management
```elisp
M-x reload-emacs-config        # Non-blocking reload
M-x reload-emacs-config-fast   # Fast reload (byte-compiled)
M-x reload-emacs-config-smart  # Smart reload (only changed)
M-x byte-compile-config        # Compile all config files
M-x clean-byte-compiled-files  # Remove .elc files
```

### Package Management
```elisp
M-x package-refresh-without-proxy  # Refresh bypassing proxy
M-x package-install-without-proxy  # Install bypassing proxy
M-x install-dev-packages          # Install development packages
M-x toggle-proxy                  # Toggle proxy settings
```

### Diagnostics
```elisp
M-x fix-editing-now             # Emergency restore editing
M-x diagnose-editing-issue      # Diagnose editing problems
M-x diagnose-cua-selection      # Debug CUA mode issues
M-x diagnose-key-conflicts      # Debug keybinding conflicts
```

## Customization

### Adding New Modules

1. Create a new file in `lisp/` directory (e.g., `init-myfeature.el`)
2. Add configuration using `use-package` format
3. Load it in `init.el`:
```elisp
(require 'init-myfeature)
```

### Disabling Modules

Comment out the corresponding `require` statement in `init.el`:
```elisp
;; (require 'init-module-name)
```

### Personal Configuration

Create `personal-config.el` in the root directory for personal settings that won't be committed to version control.

## Performance Optimization

### Byte Compilation
Speed up startup by byte-compiling configuration:
```bash
emacs --batch --eval "(byte-compile-config)"
```

### Startup Profiling
```elisp
M-x emacs-init-time  # Show initialization time
M-x profiler-start   # Start profiling
M-x profiler-report  # View profiling results
```

## Troubleshooting

### Editing Disabled
If you can't type or edit:
1. Press `C-c C-!` for emergency fix
2. Run `M-x fix-editing-now`
3. Check with `M-x diagnose-editing-issue`

### Package Installation Issues
If packages fail to install:
1. Try `M-x package-refresh-without-proxy`
2. Toggle proxy with `M-x toggle-proxy`
3. Manually refresh: `M-x package-refresh-contents`

### Slow Startup
1. Byte-compile configuration: `M-x byte-compile-config`
2. Use fast reload: `C-c R`
3. Check startup time: `M-x emacs-init-time`

### Keybinding Conflicts
1. Run `M-x diagnose-key-conflicts`
2. Check CUA mode: `M-x diagnose-cua-selection`
3. Review mode-specific keybindings

## CUA Mode Special Handling

The configuration intelligently manages CUA mode (standard C-c/C-v/C-x keybindings) by automatically disabling it in modes that need single-key commands:
- Magit (git interface)
- Dired (file manager)
- Elfeed (RSS reader)
- Special read-only buffers

## Requirements

- Emacs 29.1 or later (for Tree-sitter support)
- Git (for Magit)
- ripgrep (optional, for fast searching)
- Language servers for development (installed automatically per language)
