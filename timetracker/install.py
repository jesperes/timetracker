"""LaunchAgent installation and removal for the timetracker daemon."""

import shutil
import subprocess
import sys
from pathlib import Path

PLIST_LABEL = "com.jesper.timetracker"
PLIST_PATH = Path.home() / "Library" / "LaunchAgents" / f"{PLIST_LABEL}.plist"
LOG_PATH = Path.home() / ".timetracker.log"


def _daemon_path() -> str:
    """Locate the timetracker-daemon script installed alongside this interpreter."""
    found = shutil.which("timetracker-daemon")
    if found:
        return found
    # Fall back to the bin/ next to the current Python interpreter
    candidate = Path(sys.executable).parent / "timetracker-daemon"
    if candidate.exists():
        return str(candidate)
    raise RuntimeError(
        "Could not find timetracker-daemon. "
        "Make sure the package is installed (pip install .)."
    )


def install() -> None:
    """Install the LaunchAgent and start the daemon."""
    daemon = _daemon_path()
    plist = f"""\
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>{PLIST_LABEL}</string>
    <key>ProgramArguments</key>
    <array>
        <string>{daemon}</string>
    </array>
    <key>EnvironmentVariables</key>
    <dict>
        <key>PYTHONUNBUFFERED</key>
        <string>1</string>
    </dict>
    <key>RunAtLoad</key>
    <true/>
    <key>KeepAlive</key>
    <true/>
    <key>StandardOutPath</key>
    <string>{LOG_PATH}</string>
    <key>StandardErrorPath</key>
    <string>{LOG_PATH}</string>
</dict>
</plist>
"""
    PLIST_PATH.parent.mkdir(parents=True, exist_ok=True)
    PLIST_PATH.write_text(plist)

    # Unload any previous version before loading the new one
    subprocess.run(["launchctl", "unload", str(PLIST_PATH)], capture_output=True)
    subprocess.run(["launchctl", "load", str(PLIST_PATH)], check=True)

    print("Daemon installed and started.")
    print(f"  Plist:  {PLIST_PATH}")
    print(f"  Binary: {daemon}")
    print(f"  Log:    {LOG_PATH}")


def uninstall() -> None:
    """Stop the daemon and remove the LaunchAgent."""
    if not PLIST_PATH.exists():
        print("LaunchAgent not found — nothing to uninstall.")
        return

    subprocess.run(["launchctl", "unload", str(PLIST_PATH)], capture_output=True)
    subprocess.run(["pkill", "-f", "timetracker-daemon"], capture_output=True)
    PLIST_PATH.unlink()
    print("Daemon stopped and LaunchAgent removed.")
