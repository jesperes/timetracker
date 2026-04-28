#!/usr/bin/env python3
"""Simple time tracking utility for monitoring daily work time."""

import json
from datetime import datetime
from pathlib import Path


class TimeTracker:
    """Track time spent working during the day."""

    def __init__(self, data_dir=None):
        """Initialize the time tracker.

        Args:
            data_dir: Directory to store tracking data. Defaults to ~/.worktime
        """
        if data_dir is None:
            data_dir = Path.home() / ".worktime"

        self.data_dir = Path(data_dir)
        self.data_dir.mkdir(exist_ok=True)
        self.today_file = self.data_dir / f"{datetime.now().strftime('%Y-%m-%d')}.json"

    def start(self):
        """Record work session start time."""
        data = self._load_data()
        data["sessions"].append({
            "start": datetime.now().isoformat(),
            "end": None
        })
        self._save_data(data)
        print(f"Work session started at {datetime.now().strftime('%H:%M:%S')}")

    def stop(self):
        """Record work session end time."""
        data = self._load_data()
        if not data["sessions"] or data["sessions"][-1]["end"] is not None:
            print("No active work session to stop")
            return

        data["sessions"][-1]["end"] = datetime.now().isoformat()
        self._save_data(data)
        print(f"Work session ended at {datetime.now().strftime('%H:%M:%S')}")

    def status(self):
        """Show current day's work summary."""
        data = self._load_data()
        total_seconds = 0
        active_session = False

        for session in data["sessions"]:
            start = datetime.fromisoformat(session["start"])
            if session["end"]:
                end = datetime.fromisoformat(session["end"])
                duration = (end - start).total_seconds()
            else:
                duration = (datetime.now() - start).total_seconds()
                active_session = True

            total_seconds += duration

        hours = int(total_seconds // 3600)
        minutes = int((total_seconds % 3600) // 60)

        status_msg = f"Total work time today: {hours}h {minutes}m"
        if active_session:
            status_msg += " (session in progress)"

        print(status_msg)
        return total_seconds

    def _load_data(self):
        """Load tracking data for today."""
        if self.today_file.exists():
            with open(self.today_file) as f:
                return json.load(f)
        return {"date": datetime.now().isoformat(), "sessions": []}

    def _save_data(self, data):
        """Save tracking data to file."""
        with open(self.today_file, "w") as f:
            json.dump(data, f, indent=2)


def main():
    """CLI entry point."""
    import sys

    tracker = TimeTracker()

    if len(sys.argv) < 2:
        tracker.status()
        return

    command = sys.argv[1].lower()

    if command == "start":
        tracker.start()
    elif command == "stop":
        tracker.stop()
    elif command == "status":
        tracker.status()
    else:
        print(f"Unknown command: {command}")
        print("Usage: time_tracker.py [start|stop|status]")


if __name__ == "__main__":
    main()
