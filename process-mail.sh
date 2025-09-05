#!/bin/bash
# Simplified mail processing script
# Now that Proton handles filtering server-side, we just sync and optionally archive

# Configuration
MAILDIR="$HOME/Maildir"
ARCHIVE_LISTS=true  # Set to false if you don't want local date-based archives

# Function to create date-based archive of Lists folder
archive_lists() {
    if [ "$ARCHIVE_LISTS" != "true" ]; then
        return
    fi
    
    echo "Creating local date-based archives of mailing lists..."
    
    local year=$(date +%Y)
    local month=$(date +%m) 
    local day=$(date +%d)
    
    # Process messages in Lists folder
    if [ -d "$MAILDIR/Personal/Lists" ]; then
        find "$MAILDIR/Personal/Lists"/{new,cur} -type f 2>/dev/null | while read -r msg; do
            # Determine which list this belongs to based on List-Id header
            LIST_NAME=""
            
            if grep -q "List-Id:.*linux-kernel\.vger\.kernel\.org" "$msg" 2>/dev/null; then
                LIST_NAME="linux-kernel"
            elif grep -q "List-Id:.*std-discussion\.lists\.isocpp\.org" "$msg" 2>/dev/null; then
                LIST_NAME="std-discussion"
            elif grep -q "List-Id:.*gcc\.gnu\.gcc\.org" "$msg" 2>/dev/null; then
                LIST_NAME="gcc"
            elif grep -q "List-Id:.*interest\.qt-project\.org" "$msg" 2>/dev/null; then
                LIST_NAME="qt-interest"
            elif grep -q "List-Id:.*boost-announce\.lists\.boost\.org" "$msg" 2>/dev/null; then
                LIST_NAME="boost-announce"
            elif grep -q "List-Id:.*boost-interest\.lists\.boost\.org" "$msg" 2>/dev/null; then
                LIST_NAME="boost-interest"
            elif grep -q "List-Id:.*boost\.lists\.boost\.org" "$msg" 2>/dev/null; then
                LIST_NAME="boost"
            elif grep -q "List-Id:" "$msg" 2>/dev/null; then
                # Extract list name from List-Id header
                LIST_NAME=$(grep "List-Id:" "$msg" | sed -n 's/.*<\([^.]*\)\..*/\1/p' | head -1)
                if [ -z "$LIST_NAME" ]; then
                    LIST_NAME="misc"
                fi
            fi
            
            # If we identified a list, create archive copy
            if [ -n "$LIST_NAME" ]; then
                ARCHIVE_DIR="$MAILDIR/Personal/Archive/Lists/$LIST_NAME/$year/$month/$day"
                mkdir -p "$ARCHIVE_DIR"/{new,cur,tmp}
                
                # Determine target directory (new or cur)
                if [[ "$msg" == */new/* ]]; then
                    TARGET_DIR="$ARCHIVE_DIR/new"
                else
                    TARGET_DIR="$ARCHIVE_DIR/cur"
                fi
                
                # Copy if not already archived
                BASENAME=$(basename "$msg")
                if [ ! -f "$TARGET_DIR/$BASENAME" ]; then
                    cp "$msg" "$TARGET_DIR/"
                fi
            fi
        done
    fi
}

# Main processing
echo "=========================================="
echo "Mail Processing Started: $(date)"
echo "=========================================="

# Step 1: Sync mail with mbsync
echo ""
echo "Step 1: Syncing mail with mbsync..."
mbsync -a
SYNC_STATUS=$?

if [ $SYNC_STATUS -ne 0 ]; then
    echo "Note: mbsync returned status $SYNC_STATUS"
    echo "This can be normal if some folders are not yet created on the server."
fi

# Step 2: Create local archives (optional)
if [ "$ARCHIVE_LISTS" = "true" ]; then
    echo ""
    echo "Step 2: Creating local date-based archives..."
    archive_lists
else
    echo ""
    echo "Step 2: Skipping local archives (disabled)"
fi

# Step 3: Update mu index
echo ""
echo "Step 3: Updating mu index..."
mu index --quiet

# Step 4: Show summary
echo ""
echo "=========================================="
echo "Mail Processing Complete: $(date)"
echo "=========================================="

# Show folder statistics
echo ""
echo "Mail folder summary:"
echo "  INBOX: $(find "$MAILDIR/Personal/INBOX" -type f 2>/dev/null | wc -l) messages"
echo "  Lists: $(find "$MAILDIR/Personal/Lists" -type f 2>/dev/null | wc -l) messages"

if [ "$ARCHIVE_LISTS" = "true" ] && [ -d "$MAILDIR/Personal/Archive/Lists" ]; then
    echo ""
    echo "Local archives:"
    find "$MAILDIR/Personal/Archive/Lists" -maxdepth 1 -type d -name "[a-z]*" 2>/dev/null | sort | while read -r dir; do
        count=$(find "$dir" -type f 2>/dev/null | wc -l)
        if [ $count -gt 0 ]; then
            printf "  %-20s %d messages\n" "$(basename "$dir"):" "$count"
        fi
    done
fi

exit 0