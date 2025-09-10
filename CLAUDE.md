# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a modular Emacs configuration with 33+ configuration modules in the `lisp/` directory. The configuration is designed for software development with support for multiple languages, LSP, version control, and various productivity tools.

## Core Commands

### Configuration Management
```elisp
M-x reload-emacs-config        ; Non-blocking reload (won't freeze UI)
M-x reload-emacs-config-blocking ; Original blocking reload
M-x reload-emacs-config-fast  ; Fast reload using byte-compiled files
M-x reload-emacs-config-smart ; Smart reload (only changed files)
M-x reload-current-file       ; Reload only current .el file
M-x byte-compile-config       ; Byte-compile all config files for faster loading
M-x clean-byte-compiled-files ; Remove all .elc files
M-x byte-compile-init-files   ; Compile only lisp/ directory files

;; Keybindings:
C-c C-r   ; Non-blocking reload (default)
C-u C-c C-r ; Blocking reload (old behavior)
C-c r     ; Reload current file only
C-c R     ; Fast reload (byte-compiled)
```

### Emergency Fixes
```elisp
M-x fix-editing-now            ; Emergency restore editing capability
M-x diagnose-editing-issue     ; Diagnose why editing is disabled
M-x diagnose-cua-selection     ; Debug CUA mode and selection issues
M-x diagnose-key-conflicts     ; Debug keybinding conflicts
C-c C-!                        ; Quick emergency fix keybinding
C-c C-?                        ; Quick diagnosis keybinding
```

### Package Management
```elisp
M-x package-refresh-without-proxy  ; Refresh packages bypassing proxy
M-x package-install-without-proxy  ; Install package bypassing proxy
M-x install-dev-packages           ; Install common development packages
M-x toggle-proxy                   ; Toggle proxy settings on/off
```

### Development Modes
```elisp
M-x enable-dev-mode-modern    ; Enable Eglot-based development mode
M-x enable-dev-mode           ; Enable LSP-mode based development (legacy)
M-x disable-eslint-in-buffer  ; Disable ESLint in current buffer
```

## Architecture

### Module Loading Order (init.el)
1. **Emergency fixes** (`init-emergency-fix`) - Loaded first to ensure editing works
2. **Core modules** - Package management, completion, UI, editor settings
3. **Development tools** - Project management, VCS, LSP, treesitter
4. **Optional configs** - Loaded conditionally if files exist

### Key Configuration Modules

**Core System:**
- `init-core.el` - Package management with use-package
- `init-completion.el` - Company mode for auto-completion (replaced Corfu)
- `init-ui.el` - CUA mode configuration, themes, display settings
- `init-editor.el` - Selection keybindings, shift-selection fixes

**Development:**
- `init-eglot.el` - Built-in LSP client configuration
- `init-treesitter.el` - Tree-sitter support for Emacs 29+
- `emacs-dev-config-modern.el` - Modern development setup with Eglot
- `emacs-dev-config.el` - Legacy LSP-mode configuration

**Fix Modules:**
- `init-emergency-fix.el` - Emergency editing restoration
- `init-eslint-fix.el` - ESLint configuration handling
- `init-seq-fix.el` - Seq library compatibility fixes

**Applications:**
- `elfeed-config.el` - RSS reader with CUA mode fixes
- `portfolio-tracker-v2.el` - Investment portfolio tracking
- `mu4e-config.el` - Email client configuration

### CUA Mode and Keybinding Architecture

The configuration uses CUA mode for standard copy/paste (C-c, C-v, C-x) but disables it in special modes where single-key commands are needed:
- Elfeed buffers
- Magit
- Dired
- Portfolio tracker

C-Shift-Arrow word selection is specially configured to work with CUA mode through custom functions in `init-editor.el`.

### Completion System

Uses Company mode with:
- Auto-completion after 2 characters
- 0.2 second delay
- Tab/Shift-Tab navigation
- Multiple backends configured

### Custom Keybindings
- `C-c C-r` - Reload configuration
- `C-c C-d c` - Diagnose CUA/selection
- `C-c C-d k` - Diagnose key conflicts
- `C-x w` - Open Elfeed RSS reader
- `C-c $` - Open portfolio tracker

## Important Notes

- The configuration automatically disables CUA mode in special modes to preserve single-key commands
- ESLint is automatically disabled if no configuration file is found in the project
- Proxy settings can be toggled for package installation (default: eudewerepo001:3128)
- Byte-compilation is recommended for faster startup (`M-x byte-compile-config`)
- Emergency fix functions are available if editing becomes disabled