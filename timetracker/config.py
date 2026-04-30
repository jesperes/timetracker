"""Configuration management for time tracker."""

import json
from pathlib import Path

CONFIG_DIR = Path.home() / ".config" / "timetracker"
CONFIG_FILE = CONFIG_DIR / "config.json"

DEFAULT_CONFIG: dict = {
    "idle_threshold_seconds": 300,
    "idle_check_interval_seconds": 5,
    "data_directory": "~/.worktime",
}

def load_config() -> dict:
    """Load configuration, falling back to defaults for missing values."""
    config = DEFAULT_CONFIG.copy()
    if CONFIG_FILE.exists():
        try:
            with open(CONFIG_FILE) as f:
                config.update(json.load(f))
        except Exception:
            pass
    config["data_directory"] = str(Path(str(config["data_directory"])).expanduser())
    return config
