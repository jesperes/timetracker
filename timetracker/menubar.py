#!/usr/bin/env python3
"""Menu bar time tracker for macOS."""

import subprocess
import threading
import time
from datetime import datetime
from pathlib import Path

import rumps
from watchdog.events import FileSystemEventHandler
from watchdog.observers import Observer

from .config import load_config
from .tracker import TimeTracker


def get_idle_time_seconds():
    """Get idle time in seconds using Quartz."""
    from Quartz import (
        CGEventSourceSecondsSinceLastEventType,
        kCGEventSourceStateHIDSystemState,
    )

    idle_seconds = CGEventSourceSecondsSinceLastEventType(
        kCGEventSourceStateHIDSystemState, 1
    )
    print(f"Idle seconds: {idle_seconds}")
    return idle_seconds


def is_camera_active() -> bool:
    """Return True if the camera is currently in use (macOS)."""
    # Apple Silicon: the ISP driver exposes FrontCameraActive / FrontCameraStreaming.
    # Try this first; if the class exists we trust it exclusively (VDCAssistant runs
    # persistently on Apple Silicon even with no active camera session).
    try:
        result = subprocess.run(
            ["ioreg", "-r", "-c", "AppleH13CamIn"],
            capture_output=True, text=True, timeout=3,
        )
        if result.returncode == 0 and result.stdout.strip():
            return (
                '"FrontCameraActive" = Yes' in result.stdout
                or '"FrontCameraStreaming" = Yes' in result.stdout
            )
    except Exception:
        pass

    # Intel Mac fallback: VDCAssistant is only present when a camera session is open.
    try:
        result = subprocess.run(
            ["pgrep", "-x", "VDCAssistant"],
            capture_output=True, timeout=2,
        )
        if result.returncode == 0:
            return True
    except Exception:
        pass

    return False


class FileChangeHandler(FileSystemEventHandler):
    """Handle file system events."""

    def __init__(self, callback, target_file):
        self.callback = callback
        self.target_file = target_file

    def on_modified(self, event):
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
        self.idle_auto_paused = False
        self.current_idle_seconds = 0
        self._current_date = datetime.now().date()

        self.menu = [
            rumps.MenuItem("Start Work", callback=self.start_work),
            rumps.MenuItem("Stop Work", callback=self.stop_work),
            None,
            rumps.MenuItem("Reset Today", callback=self.reset_today),
        ]

        self.start_file_monitor()
        self.start_idle_monitor()
        self.update_timer_display()

    def start_work(self, sender):
        self.tracker.start()
        self.is_running = True
        self.update_timer_display()

    def stop_work(self, sender):
        self.tracker.stop()
        self.is_running = False
        self.idle_auto_paused = False
        self.update_timer_display()

    def reset_today(self, sender):
        response = rumps.alert(
            "Reset today's work time?",
            "This will delete all sessions for today.",
            ok="Reset",
            cancel="Cancel",
        )
        if response == 0:
            self.tracker.today_file.unlink(missing_ok=True)
            self.update_timer_display()

    def start_file_monitor(self):
        watch_dir = self.tracker.data_dir
        event_handler = FileChangeHandler(self.update_timer_display, self.tracker.today_file)
        self.observer = Observer()
        self.observer.schedule(event_handler, str(watch_dir), recursive=False)
        self.observer.start()

    def start_idle_monitor(self):
        self.idle_monitor_thread = threading.Thread(target=self._idle_monitor_loop, daemon=True)
        self.idle_monitor_thread.start()

    def _idle_monitor_loop(self):
        """Check idle time periodically and auto-pause/resume as needed."""
        while True:
            try:
                # Midnight rollover: close any open session on the old day's file
                today = datetime.now().date()
                if today != self._current_date:
                    print(f"Date rolled over from {self._current_date} to {today}")
                    self.tracker.stop()
                    self.idle_auto_paused = True
                    self._current_date = today

                idle_seconds = get_idle_time_seconds()
                self.current_idle_seconds = idle_seconds
                data = self.tracker._load_data()

                has_active_session = any(s["end"] is None for s in data["sessions"])

                self.update_timer_display()

                camera_on = is_camera_active()
                print(f"Camera active: {camera_on}")

                if idle_seconds > self.idle_threshold and not camera_on and has_active_session:
                    if not self.idle_auto_paused:
                        self.tracker.stop()
                        self.idle_auto_paused = True
                        print(f"Auto-paused work session (idle for {int(idle_seconds)}s)")
                elif (idle_seconds <= self.idle_threshold or camera_on) and self.idle_auto_paused:
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

            icon = "▶️" if active_session else "⏸"
            title = f"{icon} {hours}h {minutes}m"

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
