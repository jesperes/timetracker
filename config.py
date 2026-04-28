"""Configuration management for time tracker."""

import json
from pathlib import Path


# Default configuration
DEFAULT_CONFIG = {
    "idle_threshold_seconds": 120,
    "idle_check_interval_seconds": 10,
    "data_directory": "~/.worktime",
}


def load_config() -> dict:
    """Load configuration from config.json, with defaults for missing values."""
    config_file = Path(__file__).parent / "config.json"

    config = DEFAULT_CONFIG.copy()

    if config_file.exists():
        try:
            with open(config_file) as f:
                user_config = json.load(f)
                config.update(user_config)
        except Exception:
            pass

    # Expand home directory in paths
    if "data_directory" in config:
        config["data_directory"] = str(Path(str(config["data_directory"])).expanduser())

    return config
