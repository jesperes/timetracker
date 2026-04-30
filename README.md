# Time Tracker

A macOS menu bar app that automatically tracks daily work time using idle detection and camera activity.

## Installation

```bash
pip install .
timetracker install
```

`pip install .` installs the package and its dependencies. `timetracker install` writes the LaunchAgent plist and starts the daemon — it will also auto-start on future logins.

For development, use an editable install:

```bash
pip install -e .
timetracker install
```

## Uninstalling

```bash
timetracker uninstall
pip uninstall timetracker
```

## CLI

```bash
timetracker status     # Total work time today
timetracker summary    # Session-by-session breakdown with break histogram
timetracker start      # Manually start a session
timetracker stop       # Manually stop the current session
```

## Menu bar

The icon shows the current state:

- **▶️** actively tracking
- **⏸** paused

The title shows today's total time and a countdown to idle auto-pause.

## Automatic idle detection

- Pauses after 2 minutes of keyboard/mouse inactivity
- Resumes immediately on next activity
- Does **not** pause during video calls — camera activity (USB webcam or built-in) keeps the session running

## Configuration

Override defaults by creating `~/.config/timetracker/config.json`:

```json
{
  "idle_threshold_seconds": 120,
  "idle_check_interval_seconds": 10,
  "data_directory": "~/.worktime"
}
```

## Data

Sessions are stored as JSON in `~/.worktime/YYYY-MM-DD.json`. Each day gets its own file; files are created automatically at midnight rollover.

## Logs

```bash
tail -f ~/.timetracker.log
```
