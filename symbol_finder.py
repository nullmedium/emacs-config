#!/usr/bin/env python3
"""
Symbol finder tool for C++ and QML source files with caching support.
"""

import os
import re
import json
import time
import hashlib
import argparse
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Set
from dataclasses import dataclass, asdict
from collections import defaultdict

@dataclass
class Symbol:
    """Represents a symbol found in source code."""
    name: str
    file_path: str
    line_number: int
    symbol_type: str  # 'class', 'function', 'variable', 'property', 'signal', 'method', 'enum', 'namespace'
    context: str  # Line content for context
    
    def to_dict(self):
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data):
        return cls(**data)

class SymbolCache:
    """Manages caching of symbol information."""
    
    def __init__(self, cache_dir: str = ".symbol_cache"):
        self.cache_dir = Path(cache_dir)
        self.cache_dir.mkdir(exist_ok=True)
        self.index_file = self.cache_dir / "index.json"
        self.symbols_file = self.cache_dir / "symbols.json"
        self.index = self._load_index()
        self.symbols = self._load_symbols()
    
    def _load_index(self) -> Dict[str, Dict]:
        """Load file index from cache."""
        if self.index_file.exists():
            with open(self.index_file, 'r') as f:
                return json.load(f)
        return {}
    
    def _load_symbols(self) -> Dict[str, List[Dict]]:
        """Load symbols from cache."""
        if self.symbols_file.exists():
            with open(self.symbols_file, 'r') as f:
                return json.load(f)
        return {}
    
    def save(self):
        """Save cache to disk."""
        with open(self.index_file, 'w') as f:
            json.dump(self.index, f, indent=2)
        with open(self.symbols_file, 'w') as f:
            json.dump(self.symbols, f, indent=2)
    
    def is_file_cached(self, file_path: str) -> bool:
        """Check if file is cached and up to date."""
        if file_path not in self.index:
            return False
        
        cached_mtime = self.index[file_path].get('mtime', 0)
        current_mtime = os.path.getmtime(file_path)
        return cached_mtime >= current_mtime
    
    def update_file(self, file_path: str, symbols: List[Symbol]):
        """Update cache for a specific file."""
        self.index[file_path] = {
            'mtime': os.path.getmtime(file_path),
            'symbol_count': len(symbols)
        }
        self.symbols[file_path] = [s.to_dict() for s in symbols]
    
    def get_symbols(self, file_path: str) -> List[Symbol]:
        """Get cached symbols for a file."""
        if file_path in self.symbols:
            return [Symbol.from_dict(s) for s in self.symbols[file_path]]
        return []
    
    def get_all_symbols(self) -> List[Symbol]:
        """Get all cached symbols."""
        all_symbols = []
        for file_path, symbols in self.symbols.items():
            all_symbols.extend([Symbol.from_dict(s) for s in symbols])
        return all_symbols

class CppParser:
    """Parser for C++ source files."""
    
    # Patterns for C++ symbols
    CLASS_PATTERN = re.compile(r'^\s*(class|struct|union)\s+([A-Za-z_]\w*)', re.MULTILINE)
    FUNCTION_PATTERN = re.compile(r'^\s*(?:(?:static|inline|virtual|const|explicit|friend|extern)\s+)*(?:[\w:]+\s+)?([A-Za-z_]\w*)\s*\([^)]*\)\s*(?:const)?\s*(?:override)?\s*[{;]', re.MULTILINE)
    NAMESPACE_PATTERN = re.compile(r'^\s*namespace\s+([A-Za-z_]\w*)', re.MULTILINE)
    ENUM_PATTERN = re.compile(r'^\s*enum\s+(?:class\s+)?([A-Za-z_]\w*)', re.MULTILINE)
    TYPEDEF_PATTERN = re.compile(r'^\s*(?:typedef|using)\s+.*\s+([A-Za-z_]\w*)\s*[;=]', re.MULTILINE)
    MEMBER_VAR_PATTERN = re.compile(r'^\s*(?:(?:static|const|mutable)\s+)*(?:[\w:]+\s+)+([A-Za-z_]\w*)\s*[;=]', re.MULTILINE)
    
    def parse_file(self, file_path: str) -> List[Symbol]:
        """Parse a C++ file and extract symbols."""
        symbols = []
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')
        except Exception as e:
            print(f"Error reading {file_path}: {e}", file=sys.stderr)
            return symbols
        
        # Extract classes/structs
        for match in self.CLASS_PATTERN.finditer(content):
            line_num = content[:match.start()].count('\n') + 1
            symbols.append(Symbol(
                name=match.group(2),
                file_path=file_path,
                line_number=line_num,
                symbol_type='class',
                context=lines[line_num - 1].strip() if line_num <= len(lines) else ""
            ))
        
        # Extract namespaces
        for match in self.NAMESPACE_PATTERN.finditer(content):
            line_num = content[:match.start()].count('\n') + 1
            symbols.append(Symbol(
                name=match.group(1),
                file_path=file_path,
                line_number=line_num,
                symbol_type='namespace',
                context=lines[line_num - 1].strip() if line_num <= len(lines) else ""
            ))
        
        # Extract enums
        for match in self.ENUM_PATTERN.finditer(content):
            line_num = content[:match.start()].count('\n') + 1
            symbols.append(Symbol(
                name=match.group(1),
                file_path=file_path,
                line_number=line_num,
                symbol_type='enum',
                context=lines[line_num - 1].strip() if line_num <= len(lines) else ""
            ))
        
        # Extract functions (basic pattern, may need refinement)
        for match in self.FUNCTION_PATTERN.finditer(content):
            name = match.group(1)
            # Filter out some common false positives
            if name not in ['if', 'while', 'for', 'switch', 'return', 'delete', 'new']:
                line_num = content[:match.start()].count('\n') + 1
                symbols.append(Symbol(
                    name=name,
                    file_path=file_path,
                    line_number=line_num,
                    symbol_type='function',
                    context=lines[line_num - 1].strip() if line_num <= len(lines) else ""
                ))
        
        return symbols

class QmlParser:
    """Parser for QML files."""
    
    # Patterns for QML symbols - more comprehensive
    QML_TYPE_PATTERN = re.compile(r'^\s*([A-Z]\w*)\s*\{', re.MULTILINE)
    # Match both regular and readonly properties with better type capture
    PROPERTY_PATTERN = re.compile(r'^\s*(?:readonly\s+)?property\s+(?:[\w.<>]+\s+)?([a-zA-Z_]\w*)', re.MULTILINE)
    SIGNAL_PATTERN = re.compile(r'^\s*signal\s+([a-zA-Z_]\w*)', re.MULTILINE)
    FUNCTION_PATTERN = re.compile(r'^\s*function\s+([a-zA-Z_]\w*)\s*\(', re.MULTILINE)
    ID_PATTERN = re.compile(r'^\s*id:\s*([a-zA-Z_]\w*)', re.MULTILINE)
    # Match simple property assignments - but be more selective
    PROPERTY_BINDING_PATTERN = re.compile(r'^\s*([a-zA-Z_]\w*):\s*(?:["\'{#]|[0-9]|true|false)', re.MULTILINE)
    
    def parse_file(self, file_path: str, debug: bool = False) -> List[Symbol]:
        """Parse a QML file and extract symbols."""
        symbols = []
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')
        except Exception as e:
            print(f"Error reading {file_path}: {e}", file=sys.stderr)
            return symbols
        
        if debug:
            print(f"Debug: Parsing {file_path}, {len(lines)} lines", file=sys.stderr)
        
        # Extract QML types
        for match in self.QML_TYPE_PATTERN.finditer(content):
            line_num = content[:match.start()].count('\n') + 1
            symbols.append(Symbol(
                name=match.group(1),
                file_path=file_path,
                line_number=line_num,
                symbol_type='class',
                context=lines[line_num - 1].strip() if line_num <= len(lines) else ""
            ))
        
        # Extract properties (including readonly)
        property_matches = list(self.PROPERTY_PATTERN.finditer(content))
        if debug:
            print(f"Debug: Found {len(property_matches)} property declarations", file=sys.stderr)
        
        for match in property_matches:
            line_num = content[:match.start()].count('\n') + 1
            name = match.group(1)
            # Avoid duplicates by checking if we already have this symbol at same line
            if not any(s.name == name and s.line_number == line_num for s in symbols):
                symbols.append(Symbol(
                    name=name,
                    file_path=file_path,
                    line_number=line_num,
                    symbol_type='property',
                    context=lines[line_num - 1].strip() if line_num <= len(lines) else ""
                ))
                if debug and 'color' in name.lower():
                    print(f"Debug: Found color property: {name} at line {line_num}", file=sys.stderr)
        
        # Extract property bindings (like colorMedTechNavy1: "#value")
        for match in self.PROPERTY_BINDING_PATTERN.finditer(content):
            line_num = content[:match.start()].count('\n') + 1
            name = match.group(1)
            # Skip common keywords that aren't properties
            if name not in {'import', 'if', 'else', 'for', 'while', 'return', 'var', 'let', 'const'}:
                # Check if this isn't already captured
                if not any(s.name == name and abs(s.line_number - line_num) < 2 for s in symbols):
                    symbols.append(Symbol(
                        name=name,
                        file_path=file_path,
                        line_number=line_num,
                        symbol_type='property',
                        context=lines[line_num - 1].strip() if line_num <= len(lines) else ""
                    ))
        
        # Extract signals
        for match in self.SIGNAL_PATTERN.finditer(content):
            line_num = content[:match.start()].count('\n') + 1
            symbols.append(Symbol(
                name=match.group(1),
                file_path=file_path,
                line_number=line_num,
                symbol_type='signal',
                context=lines[line_num - 1].strip() if line_num <= len(lines) else ""
            ))
        
        # Extract functions
        for match in self.FUNCTION_PATTERN.finditer(content):
            line_num = content[:match.start()].count('\n') + 1
            symbols.append(Symbol(
                name=match.group(1),
                file_path=file_path,
                line_number=line_num,
                symbol_type='function',
                context=lines[line_num - 1].strip() if line_num <= len(lines) else ""
            ))
        
        # Extract IDs
        for match in self.ID_PATTERN.finditer(content):
            line_num = content[:match.start()].count('\n') + 1
            symbols.append(Symbol(
                name=match.group(1),
                file_path=file_path,
                line_number=line_num,
                symbol_type='variable',
                context=lines[line_num - 1].strip() if line_num <= len(lines) else ""
            ))
        
        return symbols

class SymbolFinder:
    """Main symbol finder class."""
    
    def __init__(self, root_dir: str = ".", cache_dir: str = ".symbol_cache"):
        self.root_dir = Path(root_dir).resolve()
        # Always use absolute path for cache_dir
        if not Path(cache_dir).is_absolute():
            cache_dir = self.root_dir / cache_dir
        self.cache = SymbolCache(str(cache_dir))
        self.cpp_parser = CppParser()
        self.qml_parser = QmlParser()
        self.file_extensions = {
            '.cpp', '.cc', '.cxx', '.c++', '.hpp', '.h', '.hh', '.hxx', '.h++',
            '.qml', '.js'
        }
    
    def should_index_file(self, file_path: Path) -> bool:
        """Check if a file should be indexed."""
        return file_path.suffix.lower() in self.file_extensions
    
    def index_file(self, file_path: str, debug: bool = False, force: bool = False) -> List[Symbol]:
        """Index a single file."""
        file_path_obj = Path(file_path)
        
        if not file_path_obj.exists():
            if debug:
                print(f"Debug: File does not exist: {file_path}", file=sys.stderr)
            return []
        
        # Check cache first (unless force is True)
        if not force and self.cache.is_file_cached(file_path):
            if debug:
                print(f"Debug: Using cached symbols for {file_path}", file=sys.stderr)
            return self.cache.get_symbols(file_path)
        
        # Parse file based on extension
        symbols = []
        if file_path_obj.suffix.lower() in {'.qml', '.js'}:
            symbols = self.qml_parser.parse_file(file_path, debug=debug)
        else:
            symbols = self.cpp_parser.parse_file(file_path)
        
        if debug:
            print(f"Debug: Found {len(symbols)} symbols in {file_path}", file=sys.stderr)
        
        # Update cache
        self.cache.update_file(file_path, symbols)
        return symbols
    
    def index_directory(self, directory: str = None, force_reindex: bool = False, debug: bool = False):
        """Index all files in a directory tree."""
        if directory is None:
            directory = self.root_dir
        
        directory = Path(directory).resolve()
        
        if debug:
            print(f"Debug: Indexing directory {directory}", file=sys.stderr)
        
        indexed_count = 0
        skipped_count = 0
        
        for root, dirs, files in os.walk(directory):
            # Skip hidden directories and common cache directories
            # Note: 'build' removed to allow indexing build directories if needed
            dirs[:] = [d for d in dirs if not d.startswith('.') and d not in {'node_modules', '__pycache__', 'CMakeFiles', 'dist', 'target'}]
            
            for file in files:
                file_path = Path(root) / file
                if self.should_index_file(file_path):
                    file_str = str(file_path)
                    if force_reindex or not self.cache.is_file_cached(file_str):
                        print(f"Indexing: {file_str}")
                        symbols = self.index_file(file_str, debug=debug, force=force_reindex)
                        if symbols:
                            indexed_count += 1
                        else:
                            if debug:
                                print(f"Debug: No symbols found in {file_str}", file=sys.stderr)
                    else:
                        skipped_count += 1
                        if debug:
                            print(f"Debug: Skipping cached file: {file_str}", file=sys.stderr)
        
        self.cache.save()
        print(f"Indexing complete. Indexed {indexed_count} files, skipped {skipped_count} cached files.")
        print(f"Total files in cache: {len(self.cache.symbols)}")
    
    def find_symbol(self, symbol_name: str, exact_match: bool = False) -> List[Symbol]:
        """Find a symbol by name."""
        results = []
        all_symbols = self.cache.get_all_symbols()
        
        for symbol in all_symbols:
            if exact_match:
                if symbol.name == symbol_name:
                    results.append(symbol)
            else:
                if symbol_name.lower() in symbol.name.lower():
                    results.append(symbol)
        
        # Sort by relevance (exact matches first, then by name length)
        results.sort(key=lambda s: (s.name != symbol_name, len(s.name), s.name))
        return results
    
    def find_definition(self, symbol_name: str) -> Optional[Symbol]:
        """Find the most likely definition of a symbol."""
        symbols = self.find_symbol(symbol_name, exact_match=True)
        
        # Prioritize classes, then functions, then others
        priority_order = ['class', 'namespace', 'function', 'enum', 'property', 'signal', 'variable']
        
        for symbol_type in priority_order:
            for symbol in symbols:
                if symbol.symbol_type == symbol_type:
                    return symbol
        
        return symbols[0] if symbols else None
    
    def find_references(self, symbol_name: str) -> List[Tuple[str, int, str]]:
        """Find all references to a symbol (simple grep-based)."""
        references = []
        
        for file_path in self.cache.symbols.keys():
            try:
                with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                    lines = f.readlines()
                    for i, line in enumerate(lines, 1):
                        if re.search(r'\b' + re.escape(symbol_name) + r'\b', line):
                            references.append((file_path, i, line.strip()))
            except Exception as e:
                print(f"Error searching {file_path}: {e}", file=sys.stderr)
        
        return references

def main():
    parser = argparse.ArgumentParser(description='Symbol finder for C++ and QML files')
    parser.add_argument('--index', action='store_true', help='Index/reindex all files')
    parser.add_argument('--force', action='store_true', help='Force reindex even if cached')
    parser.add_argument('--find', metavar='SYMBOL', help='Find symbol by name')
    parser.add_argument('--definition', metavar='SYMBOL', help='Find definition of symbol')
    parser.add_argument('--references', metavar='SYMBOL', help='Find all references to symbol')
    parser.add_argument('--exact', action='store_true', help='Exact match only')
    parser.add_argument('--emacs', action='store_true', help='Output in Emacs format')
    parser.add_argument('--root', metavar='DIR', default='.', help='Root directory to search')
    parser.add_argument('--cache-dir', metavar='DIR', default='.symbol_cache', help='Cache directory')
    parser.add_argument('--debug', action='store_true', help='Enable debug output')
    parser.add_argument('--stats', action='store_true', help='Show cache statistics')
    parser.add_argument('--list-files', action='store_true', help='List files that would be indexed')
    
    args = parser.parse_args()
    
    if args.debug:
        print(f"Debug: Working directory: {os.getcwd()}", file=sys.stderr)
        print(f"Debug: Root directory: {Path(args.root).resolve()}", file=sys.stderr)
        print(f"Debug: Cache directory: {args.cache_dir}", file=sys.stderr)
    
    finder = SymbolFinder(args.root, args.cache_dir)
    
    if args.list_files:
        # List all files that would be indexed
        directory = Path(args.root).resolve()
        print(f"Files that would be indexed from {directory}:")
        count = 0
        for root, dirs, files in os.walk(directory):
            dirs[:] = [d for d in dirs if not d.startswith('.') and d not in {'node_modules', '__pycache__', 'CMakeFiles', 'dist', 'target'}]
            for file in files:
                file_path = Path(root) / file
                if finder.should_index_file(file_path):
                    print(f"  {file_path}")
                    count += 1
        print(f"\nTotal: {count} files")
        sys.exit(0)
    
    if args.stats:
        print(f"Cache Statistics:")
        print(f"  Root: {finder.root_dir}")
        print(f"  Cache location: {finder.cache.cache_dir}")
        print(f"  Files indexed: {len(finder.cache.index)}")
        total_symbols = sum(len(symbols) for symbols in finder.cache.symbols.values())
        print(f"  Total symbols: {total_symbols}")
        if finder.cache.index:
            print(f"\nIndexed files:")
            for file_path, info in list(finder.cache.index.items())[:10]:
                print(f"    {file_path}: {info.get('symbol_count', 0)} symbols")
            if len(finder.cache.index) > 10:
                print(f"    ... and {len(finder.cache.index) - 10} more files")
        sys.exit(0)
    
    if args.index:
        finder.index_directory(force_reindex=args.force, debug=args.debug)
    elif args.find:
        symbols = finder.find_symbol(args.find, exact_match=args.exact)
        if args.emacs:
            # Emacs format: file:line:column:
            for symbol in symbols:
                print(f"{symbol.file_path}:{symbol.line_number}:1:{symbol.symbol_type} {symbol.name}")
        else:
            for symbol in symbols:
                print(f"{symbol.name} ({symbol.symbol_type}) - {symbol.file_path}:{symbol.line_number}")
                print(f"  {symbol.context}")
    elif args.definition:
        symbol = finder.find_definition(args.definition)
        if symbol:
            if args.emacs:
                print(f"{symbol.file_path}:{symbol.line_number}:1:")
            else:
                print(f"Definition: {symbol.name} ({symbol.symbol_type})")
                print(f"Location: {symbol.file_path}:{symbol.line_number}")
                print(f"Context: {symbol.context}")
        else:
            print(f"No definition found for '{args.definition}'")
            sys.exit(1)
    elif args.references:
        refs = finder.find_references(args.references)
        if args.emacs:
            for file_path, line_num, context in refs:
                print(f"{file_path}:{line_num}:1:{context}")
        else:
            for file_path, line_num, context in refs:
                print(f"{file_path}:{line_num}: {context}")
    else:
        # Default: index if no cache exists
        if not finder.cache.index:
            print("No cache found. Indexing files...")
            finder.index_directory()
        else:
            parser.print_help()

if __name__ == '__main__':
    main()