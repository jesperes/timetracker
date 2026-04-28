# Time Tracker

A simple utility to track your daily work time.

## Usage

```bash
python3 time_tracker.py start   # Start a work session
python3 time_tracker.py stop    # End the current work session
python3 time_tracker.py status  # Show total work time for today
```

## How it works

- Each day gets its own tracking file stored in `~/.worktime/`
- Sessions are tracked as start/end timestamps in JSON format
- Running `status` shows total accumulated work time for the current day
- Sessions in progress are included in the total time calculation

## Data Format

Daily data is stored as JSON:
```json
{
  "date": "2026-04-28T10:30:00.123456",
  "sessions": [
    {
      "start": "2026-04-28T09:00:00.123456",
      "end": "2026-04-28T12:00:00.123456"
    }
  ]
}
```

## Setup

Run the single setup script:

```bash
./run.sh
```

This does everything:
1. Creates/updates the Python virtual environment
2. Installs dependencies
3. Starts the menu bar app
4. Sets up auto-start on login

## Menu Bar App (macOS)

The app shows:
- **▶️** icon when actively tracking time
- **⏸** icon when paused (no active session)
- Current day's total hours and minutes

Click the menu bar icon to:
- Start/stop work sessions
- Reset today's tracking
- Quit the app

The time updates in real-time using macOS FSEvents (no polling delay).

**Automatic idle detection:**
- Automatically pauses work sessions after 2 minutes of inactivity
- Resumes on next user activity (keyboard/mouse)
- Uses macOS IOKit for reliable idle time detection

## Configuration

Edit `config.json` to customize behavior:

```json
{
  "idle_threshold_seconds": 120,
  "idle_check_interval_seconds": 10,
  "data_directory": "~/.worktime"
}
```

- `idle_threshold_seconds` — How long to wait before auto-pausing (default: 120s / 2 min)
- `idle_check_interval_seconds` — How often to check for idle (default: 10s)
- `data_directory` — Where to store tracking data (default: ~/.worktime)

## Management

**View logs:**
```bash
tail -f ~/.timetracker.log
```

**Manual LaunchAgent control:**
```bash
launchctl start com.jesper.timetracker    # Start now
launchctl stop com.jesper.timetracker     # Stop now
launchctl unload ~/Library/LaunchAgents/com.jesper.timetracker.plist  # Disable auto-start
```

**Restart everything:**
```bash
./run.sh
```

## Future enhancements

- Pause/resume without ending session
- Historical reports and analytics
- Automatic detection of work vs idle time
- Integration with calendar or task systems
- Launch on startup option
