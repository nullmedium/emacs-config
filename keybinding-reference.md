# Emacs Keybinding Reference

## Navigation & Interface
- `M-x` - Helm command execution
- `C-x C-f` - Helm find files
- `C-x b` - Helm switch buffer
- `C-x C-b` - Helm buffer list
- `M-y` - Helm kill ring
- `C-c h o` - Helm occur
- `C-h SPC` - Helm mark rings

## Version Control (Magit)
- `C-x g` - Magit status
- `C-x M-g` - Magit dispatch
- `C-c g` - Magit file dispatch
- `C-c C-p` - Save commit as patch (in magit modes)

## Diff Highlighting
- `M-n` - Next diff hunk
- `M-p` - Previous diff hunk
- `C-c v r` - Revert hunk
- `C-c v s` - Show diff at hunk
- `C-c v u` - Update diff indicators

## Treemacs
- `M-0` - Select treemacs window
- `C-x t t` - Toggle treemacs
- `<F8>` - Toggle treemacs (alternative)
- `C-x t d` - Select directory
- `C-x t B` - Treemacs bookmark
- `C-c t f` - Toggle and focus treemacs

## Search
- `C-c r` - Deadgrep (ripgrep search)
- `C-c p` - Projectile prefix

## Development
- `C-c l` - LSP prefix
- `C-c c` - Compile
- `C-c q` - Quick compile and run
- `C-c t c` - Generate C++ tags
- `C-c t p` - Generate Python tags
- `C-c t a` - Generate all tags

## Multiple Cursors
- `C-S-l` - Edit lines
- `C-S-d` - Mark all like this
- `C->` - Mark next like this
- `C-<` - Mark previous like this
- `C-c m n` - Skip to next
- `C-c m p` - Skip to previous
- `C-S-<mouse-1>` - Add cursor on click

## Editor
- `C-=` - Expand region
- `C-<return>` - CUA rectangle mark

## Markdown/Notes
- `C-c o j` - Obsidian jump to note
- `C-c o n` - New Obsidian note
- `C-c o l` - Insert Obsidian link
- `C-c o s` - Search Obsidian vault
- `C-c C-t` - Generate markdown TOC
- `C-c z` - Olivetti mode (distraction-free)

## File Management
- `C-c n t` - Toggle neotree
- `C-c h` - Show dev config help

## System
- `C-x k` - Kill buffer (no confirm)
- `C-c C-r` - Reload Emacs config

## Conflicts Resolved
- Removed `C-d` override (now uses default delete-char)
- Helm replaces all Ivy/Counsel/Swiper bindings
- Single interface for completion (Helm only)