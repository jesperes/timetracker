#!/usr/bin/env python3
"""Menu bar time tracker for macOS."""

import rumps
import threading
import time
from pathlib import Path
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
from time_tracker import TimeTracker
from config import load_config


def get_idle_time_seconds():
    """Get idle time in seconds using Quartz."""
    from Quartz import (
        CGEventSourceSecondsSinceLastEventType,
        kCGEventSourceStateHIDSystemState,
    )

    # Get seconds since last keyboard/mouse event (idle time)
    idle_seconds = CGEventSourceSecondsSinceLastEventType(
        kCGEventSourceStateHIDSystemState, 1
    )

    print(f"Idle seconds: {idle_seconds}")

    return idle_seconds


class FileChangeHandler(FileSystemEventHandler):
    """Handle file system events."""

    def __init__(self, callback, target_file):
        self.callback = callback
        self.target_file = target_file

    def on_modified(self, event):
        """Called when a file is modified."""
        if not event.is_directory and Path(event.src_path) == self.target_file:
            self.callback()


class TimeTrackerApp(rumps.App):
    """Menu bar application for time tracking."""

    def __init__(self, *args, **kwargs):
        super().__init__("⏱", *args, **kwargs)
        self.config = load_config()
        self.idle_threshold = self.config["idle_threshold_seconds"]
        self.idle_check_interval = self.config["idle_check_interval_seconds"]
        self.tracker = TimeTracker()
        self.observer = None
        self.idle_monitor_thread = None
        self.idle_auto_paused = False  # Track if we auto-paused due to idle
        self.current_idle_seconds = 0  # Current idle time

        # Menu items
        self.menu = [
            rumps.MenuItem("Start Work", callback=self.start_work),
            rumps.MenuItem("Stop Work", callback=self.stop_work),
            None,  # separator
            rumps.MenuItem("Reset Today", callback=self.reset_today),
        ]

        # Start file monitoring and idle detection
        self.start_file_monitor()
        self.start_idle_monitor()
        self.update_timer_display()

    def start_work(self, sender):
        """Start a work session."""
        self.tracker.start()
        self.is_running = True
        self.update_timer_display()

    def stop_work(self, sender):
        """Stop the current work session."""
        self.tracker.stop()
        self.is_running = False
        self.idle_auto_paused = False
        self.update_timer_display()

    def reset_today(self, sender):
        """Reset today's tracking."""
        response = rumps.alert(
            "Reset today's work time?",
            "This will delete all sessions for today.",
            ok="Reset",
            cancel="Cancel",
        )
        if response == 0:  # OK button clicked
            self.tracker.today_file.unlink(missing_ok=True)
            self.update_timer_display()

    def start_file_monitor(self):
        """Start monitoring the tracking file for changes."""
        watch_dir = self.tracker.data_dir
        event_handler = FileChangeHandler(self.update_timer_display, self.tracker.today_file)

        self.observer = Observer()
        self.observer.schedule(event_handler, str(watch_dir), recursive=False)
        self.observer.start()

    def start_idle_monitor(self):
        """Start monitoring for idle time."""
        self.idle_monitor_thread = threading.Thread(target=self._idle_monitor_loop, daemon=True)
        self.idle_monitor_thread.start()

    def _idle_monitor_loop(self):
        """Check idle time periodically and auto-pause if needed."""
        while True:
            try:
                idle_seconds = get_idle_time_seconds()
                self.current_idle_seconds = idle_seconds
                data = self.tracker._load_data()

                # Check if there's an active session
                has_active_session = any(s["end"] is None for s in data["sessions"])

                # Update menu bar display with idle status
                self.update_timer_display()

                if idle_seconds > self.idle_threshold and has_active_session:
                    # User is idle and session is active - auto pause
                    if not self.idle_auto_paused:
                        self.tracker.stop()
                        self.idle_auto_paused = True
                        print(f"Auto-paused work session (idle for {int(idle_seconds)}s)")
                elif idle_seconds <= self.idle_threshold and self.idle_auto_paused:
                    # User is active again - auto resume
                    self.tracker.start()
                    self.idle_auto_paused = False
                    print(f"Auto-resumed work session (user active after {int(idle_seconds)}s idle)")

            except Exception as e:
                print(f"Error in idle monitor: {e}")
                import traceback
                traceback.print_exc()

            time.sleep(self.idle_check_interval)

    def update_timer_display(self):
        """Update the menu bar title with current work time and idle status."""
        try:
            data = self.tracker._load_data()
            total_seconds = 0
            active_session = False

            for session in data["sessions"]:
                from datetime import datetime
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

            # Create title with icon and time
            if active_session:
                icon = "▶️"  # Playing indicator
            else:
                icon = "⏸"  # Paused indicator

            title = f"{icon} {hours}h {minutes}m"

            # Add idle status if session is active
            if active_session:
                remaining_seconds = max(0, self.idle_threshold - self.current_idle_seconds)
                mins = int(remaining_seconds // 60)
                secs = int(remaining_seconds % 60)

                if self.current_idle_seconds >= self.idle_threshold:
                    title += f" 💤 {mins}:{secs:02d}"
                else:
                    title += f" ⚡ {mins}:{secs:02d} remaining until idle"

            self.title = title

        except Exception:
            self.title = "⏱ Error"


def main():
    """Run the menu bar app."""
    app = TimeTrackerApp()
    app.run()


if __name__ == "__main__":
    main()
