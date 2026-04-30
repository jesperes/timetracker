#!/usr/bin/env python3
"""Simple time tracking utility for monitoring daily work time."""

import json
from datetime import datetime
from pathlib import Path

from .config import load_config


class TimeTracker:
    """Track time spent working during the day."""

    def __init__(self, data_dir=None):
        if data_dir is None:
            data_dir = load_config()["data_directory"]

        self.data_dir = Path(data_dir)
        self.data_dir.mkdir(exist_ok=True)

    @property
    def today_file(self):
        return self.data_dir / f"{datetime.now().strftime('%Y-%m-%d')}.json"

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

    def summary(self):
        """Show detailed summary of today's sessions and breaks."""
        data = self._load_data()
        sessions = data["sessions"]

        if not sessions:
            print("No work sessions recorded today.")
            return

        def fmt_duration(seconds):
            h = int(seconds // 3600)
            m = int((seconds % 3600) // 60)
            if h > 0:
                return f"{h}h {m}m"
            return f"{m}m"

        def fmt_time(iso):
            return datetime.fromisoformat(iso).strftime("%H:%M")

        total_work = 0
        total_break = 0
        now = datetime.now()

        print(f"Day summary for {datetime.now().strftime('%Y-%m-%d')}")
        print()

        for i, session in enumerate(sessions):
            start = datetime.fromisoformat(session["start"])
            is_active = session["end"] is None
            end = now if is_active else datetime.fromisoformat(session["end"])
            duration = (end - start).total_seconds()
            total_work += duration

            label = f"  Work #{i + 1}:"
            end_str = "now" if is_active else fmt_time(session["end"])
            suffix = " (active)" if is_active else ""
            print(
                f"{label:<12} {fmt_time(session['start'])} – "
                f"{end_str}  ({fmt_duration(duration)}){suffix}"
            )

            if i + 1 < len(sessions):
                next_start = datetime.fromisoformat(sessions[i + 1]["start"])
                break_secs = (next_start - end).total_seconds()
                total_break += break_secs
                if break_secs >= 60:
                    print(
                        f"\033[2m  {'Break:':<10} "
                        f"{fmt_time(session['end'])} – "
                        f"{fmt_time(sessions[i + 1]['start'])}"
                        f"  ({fmt_duration(break_secs)})\033[0m"
                    )

        print()
        active_note = " (session in progress)" if sessions[-1]["end"] is None else ""
        print(f"  Total work:   {fmt_duration(total_work)}{active_note}")
        if total_break > 0:
            print(f"  Total breaks: {fmt_duration(total_break)}")

        # Collect all breaks >= 1 min for histogram
        breaks_min = []
        for i, session in enumerate(sessions[:-1]):
            if session["end"] is None:
                continue
            end = datetime.fromisoformat(session["end"])
            next_start = datetime.fromisoformat(sessions[i + 1]["start"])
            b = (next_start - end).total_seconds() / 60
            if b >= 1:
                breaks_min.append(b)

        if len(breaks_min) >= 2:
            print()
            print("  Break length histogram:")
            buckets = [0, 5, 10, 15, 20, 30, 45, 60, float("inf")]
            labels = ["<5m", "5-10m", "10-15m", "15-20m", "20-30m", "30-45m", "45-60m", "60m+"]
            counts = [0] * len(labels)
            for b in breaks_min:
                for j in range(len(labels)):
                    if buckets[j] <= b < buckets[j + 1]:
                        counts[j] += 1
                        break
            max_count = max(counts)
            bar_width = 20
            for label, count in zip(labels, counts):
                bar = "█" * int(count / max_count * bar_width) if max_count > 0 else ""
                print(f"  {label:>7}  {bar:<{bar_width}}  {count}")

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
    elif command == "summary":
        tracker.summary()
    elif command == "install":
        from .install import install
        install()
    elif command == "uninstall":
        from .install import uninstall
        uninstall()
    else:
        print(f"Unknown command: {command}")
        print("Usage: timetracker [start|stop|status|summary|install|uninstall]")
