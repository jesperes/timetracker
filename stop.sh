#!/bin/bash

PLIST_FILE="$HOME/Library/LaunchAgents/com.jesper.timetracker.plist"
pkill -f menubar_tracker.py
launchctl stop "$PLIST_FILE"
sleep 2
launchctl unload "$PLIST_FILE"
