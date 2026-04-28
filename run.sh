#!/bin/bash
# Setup and run time tracker
# Usage: ./run.sh          Run in foreground (for testing)
#        ./run.sh -d       Setup launch agent and run in background

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_DIR="$SCRIPT_DIR/.venv"
PLIST_FILE="$HOME/Library/LaunchAgents/com.jesper.timetracker.plist"
VENV_PYTHON="$VENV_DIR/bin/python3"
TRACKER_SCRIPT="$SCRIPT_DIR/menubar_tracker.py"
DAEMON_MODE=false

# Check for -d flag
if [ "$1" = "-d" ]; then
    DAEMON_MODE=true
fi

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Time Tracker Setup"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Step 1: Setup Python environment
echo "📦 Setting up Python environment..."
if [ ! -d "$VENV_DIR" ]; then
    echo "   Creating virtual environment..."
    python3 -m venv "$VENV_DIR"
    # shellcheck source=/dev/null
    source "$VENV_DIR/bin/activate" && pip3 install --upgrade pip
fi

# shellcheck source=/dev/null
source "$VENV_DIR/bin/activate"
if ! python3 -c "import rumps; import watchdog; from IOKit import IOKit" 2>/dev/null; then
    echo "   Installing dependencies..."
    pip install -q -r requirements.txt
fi
echo "   ✓ Python environment ready"
echo ""

# Step 2: Check code quality
echo "🔍 Checking code quality..."
if ! python3 -c "import ruff; import mypy" 2>/dev/null; then
    pip install -q ruff mypy
fi
ruff check . || { echo "   ✗ Ruff failed"; exit 1; }
mypy . || { echo "   ✗ Mypy failed"; exit 1; }
echo "   ✓ Code quality checks passed"
echo ""

# Step 3: Stop existing instances
echo "🔄 Stopping existing instances..."
launchctl stop com.jesper.timetracker 2>/dev/null || true
pkill -f "menubar_tracker.py" || true
sleep 1
echo "   ✓ Ready to start"
echo ""

# Step 4: Setup launch agent if daemon mode
if [ "$DAEMON_MODE" = true ]; then
    echo "🚀 Setting up auto-start on login..."
    mkdir -p "$HOME/Library/LaunchAgents"

    cat > "$PLIST_FILE" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.jesper.timetracker</string>
    <key>ProgramArguments</key>
    <array>
        <string>$VENV_PYTHON</string>
        <string>$TRACKER_SCRIPT</string>
    </array>
    <key>EnvironmentVariables</key>
    <dict>
        <key>PYTHONUNBUFFERED</key>
        <string>1</string>
    </dict>
    <key>RunAtLoad</key>
    <true/>
    <key>StandardOutPath</key>
    <string>$HOME/.timetracker.log</string>
    <key>StandardErrorPath</key>
    <string>$HOME/.timetracker.log</string>
    <key>KeepAlive</key>
    <true/>
</dict>
</plist>
EOF

    launchctl unload "$PLIST_FILE" 2>/dev/null || true
    sleep 1
    launchctl load "$PLIST_FILE"
    launchctl start com.jesper.timetracker 2>/dev/null || true

    echo "   ✓ Launch agent configured and running"
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "✅ Setup complete!"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "The time tracker is now running in the background and will:"
    echo "  • Start automatically on login"
    echo "  • Keep running in the background"
    echo "  • Update in real-time from CLI changes"
    echo ""
    echo "Logs: ~/.timetracker.log"
    echo "CLI:  python3 time_tracker.py [start|stop|status]"
else
    echo "▶️  Starting time tracker in foreground..."
    echo ""
    PYTHONUNBUFFERED=1 "$VENV_PYTHON" "$TRACKER_SCRIPT"
fi
