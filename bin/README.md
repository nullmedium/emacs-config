# Emacs Console Helper Scripts

Collection of helper scripts to launch Emacs in terminal mode (`-nw`) with specific configurations.

## Installation

Add this directory to your PATH:

```bash
export PATH="$HOME/.emacs.d/bin:$PATH"
```

Add to your shell configuration file (`~/.bashrc`, `~/.zshrc`, etc.) to make it permanent.

## Main Launcher

### `em` - Universal Emacs launcher

The main entry point for all modes. Usage:

```bash
em [mode] [args...]
```

Without arguments, launches standard Emacs in terminal mode.

## Available Modes

### Email
```bash
em mail                 # Launch mu4e email client
```

### RSS Feeds
```bash
em feeds                # Launch Elfeed RSS reader
```

### Development
```bash
em dev                  # Launch with development environment
em dev project/         # Open specific project in dev mode
```

### Git
```bash
em magit                # Launch Magit in current directory
em magit /path/to/repo  # Launch Magit in specific repository
```

### File Comparison
```bash
em compare file1 file2  # Compare two files using ediff
```

### File Manager
```bash
em dired                # Launch Dired in current directory
em dired /path/to/dir   # Launch Dired in specific directory
```

### Portfolio Tracker
```bash
em portfolio            # Launch portfolio tracker
```

### Org Mode
```bash
em org                  # Launch Org agenda
em org notes.org        # Open/create specific org file
```

### Terminal
```bash
em terminal             # Launch terminal emulator in Emacs
```

### Quick Mode
```bash
em quick                # Launch without configuration (emacs -Q)
em quick file.txt       # Quick edit without loading config
```

## Individual Scripts

You can also use the scripts directly:

- `emacs-mail` - Email client
- `emacs-feeds` - RSS reader
- `emacs-dev` - Development environment
- `emacs-magit` - Git interface
- `emacs-compare` - File comparison
- `emacs-dired` - File manager
- `emacs-portfolio` - Portfolio tracker
- `emacs-org` - Org mode
- `emacs-terminal` - Terminal emulator
- `emacs-quick` - Quick mode without config

## Examples

```bash
# Quick email check
em mail

# Work on a project
em dev ~/projects/myapp

# Compare configuration files
em compare config.old config.new

# Quick file edit without loading full config
em quick /etc/hosts

# Manage git repository
em magit ~/projects/myapp

# Browse files
em dired ~/Documents
```

## Tips

- Use `em quick` for system file edits or when you need fast startup
- The dev mode automatically enables modern development features and Treemacs
- All modes support standard Emacs command-line arguments
- Scripts preserve terminal mode (`-nw`) for console usage