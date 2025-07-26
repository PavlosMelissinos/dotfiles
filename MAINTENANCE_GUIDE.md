# System Maintenance Guide

This guide provides comprehensive solutions for cleaning up and maintaining your Linux home directory setup.

## Quick Start

Run the master maintenance script:
```bash
# Run all maintenance tasks
~/.local/bin/maintenance-runner.sh --all

# Run only quick maintenance
~/.local/bin/maintenance-runner.sh --quick

# See what would be done without making changes
~/.local/bin/maintenance-runner.sh --dry-run --all

# Set up automated maintenance
~/.local/bin/maintenance-runner.sh --setup-automation
```

## Issues Found and Solutions

### 1. Log File Management

**Issues Identified:**
- Slack logs with 193,538 lines in browser1.log (5.1MB)
- Multiple Slack webapp logs, each 5MB+
- Antigen debug log with 27,962 lines
- Total Slack logs directory: ~37MB

**Solutions Created:**
- `/home/pavlos/.local/bin/log-rotation.sh` - Automated log rotation
- `/home/pavlos/.local/bin/system-cleanup.sh` - Comprehensive cleanup
- Automatic logrotate configuration in `~/.config/logrotate/personal.conf`

**Commands to clean up logs:**
```bash
# Rotate logs immediately
~/.local/bin/log-rotation.sh

# Clean up all system logs and caches
~/.local/bin/system-cleanup.sh

# Set up daily automatic log rotation
~/.local/bin/personal-logrotate
```

### 2. Configuration Directory Cleanup

**Issues Identified:**
- 22 database files in .config directory
- Signal database: 100MB
- Chromium cache and databases: 1.5GB total
- Kodi databases: 648MB
- Various temp and cache directories

**Large Directories Found:**
- Signal: 1.8GB
- Chromium: 1.5GB
- Slack: 1.2GB
- Kodi: 648MB
- Emacs: 531MB

**Cleanup Commands:**
```bash
# Clean up all cache and temp files
~/.local/bin/system-cleanup.sh

# Vacuum SQLite databases to reclaim space
find ~/.config -name "*.sqlite" -exec sqlite3 {} "VACUUM;" \;

# Clean old browser cache (>7 days)
find ~/.cache/mozilla/firefox -name "cache2" -type d -exec find {} -type f -mtime +7 -delete \;
```

### 3. Package Management Maintenance

**Issues Identified:**
- home-manager stateVersion "23.11" (could be updated but stable)
- Several broken symlinks in cache directories
- Old Poetry virtualenvs consuming 1.9GB

**Solutions:**
```bash
# Maintain home-manager configuration
~/.local/bin/home-manager-maintenance.sh

# Clean up old Python environments
find ~/.cache/pypoetry/virtualenvs -maxdepth 1 -type d -mtime +30 -exec rm -rf {} \;

# Fix broken symlinks
find ~/.local/bin ~/.config ~/.cache -type l -exec test ! -e {} \; -print -delete
```

### 4. Performance Optimization

**Issues Identified:**
- Shell startup time: 2.068 seconds (should be <0.5s)
- Unoptimized zsh configuration
- Large cache directories affecting disk I/O

**Solutions Created:**
- `/home/pavlos/.local/bin/performance-optimizer.sh` - Shell and system optimization
- Performance-optimized zsh profile
- Compiled zsh files for faster loading

**Optimization Commands:**
```bash
# Optimize shell performance
~/.local/bin/performance-optimizer.sh

# Compile zsh files for faster loading
find ~/.config/zsh -name "*.zsh" -exec zsh -c "zcompile '{}'" \;

# Add performance profile to zsh
echo "source ~/.config/zsh/performance-profile.zsh" >> ~/.config/zsh/.zshrc
```

## Automated Solutions

### Daily Maintenance (via cron)
```bash
0 3 * * * ~/.local/bin/maintenance-runner.sh --quick
```

### Weekly Maintenance
```bash
0 4 * * 0 ~/.local/bin/maintenance-runner.sh --weekly
```

### Monthly Maintenance
```bash
0 5 1 * * ~/.local/bin/maintenance-runner.sh --monthly
```

### Log Rotation
```bash
0 2 * * * ~/.local/bin/personal-logrotate
```

## Manual Maintenance Commands

### Immediate Cleanup Commands

**Clean Slack logs (most urgent):**
```bash
# Keep only last 3 of each log type, truncate large files
find ~/.config/Slack/logs -name "browser*.log" | sort -V | head -n -3 | xargs rm -f
find ~/.config/Slack/logs -name "*.log" -size +5M -exec tail -n 10000 {} \; > {}.tmp && mv {}.tmp {}
```

**Clean cache directories:**
```bash
# Clean Firefox cache (keep last 7 days)
find ~/.cache/mozilla/firefox -name "cache2" -type d -exec find {} -type f -mtime +7 -delete \;

# Clean old Poetry virtualenvs
find ~/.cache/pypoetry/virtualenvs -maxdepth 1 -type d -mtime +30 -exec rm -rf {} \;

# Clean LibreOffice cache
rm -rf ~/.config/libreoffice/4/cache/*
```

**Clean trash:**
```bash
# Remove files older than 30 days from trash
find ~/.local/share/Trash/files -type f -mtime +30 -delete
```

### Database Optimization
```bash
# Vacuum all SQLite databases
find ~/.config -name "*.sqlite" -o -name "*.db" | while read db; do
  sqlite3 "$db" "VACUUM;" 2>/dev/null && echo "Optimized: $db"
done
```

### Home Manager Maintenance
```bash
# Clean old generations (keep last 10)
home-manager expire-generations "-10 days"

# Run garbage collection
nix-collect-garbage -d

# Optimize nix store
nix-store --optimise
```

## Prevention Strategies

### 1. Log Rotation Setup
- Automatic log rotation via cron
- Size-based rotation (5MB limit for Slack logs)
- Line-based truncation (10,000 lines max)

### 2. Cache Management
- Regular cleanup of browser caches
- Poetry virtualenv cleanup
- Automatic temp file removal

### 3. Performance Monitoring
- Shell startup time monitoring
- Disk usage alerts
- Weekly health checks

### 4. Configuration Drift Prevention
- Regular home-manager validation
- Symlink health checks
- Backup of configurations

## Scripts Created

All scripts are located in `/home/pavlos/.local/bin/`:

1. **`system-cleanup.sh`** - Main cleanup script
2. **`log-rotation.sh`** - Log rotation and management
3. **`performance-optimizer.sh`** - Shell and system optimization
4. **`home-manager-maintenance.sh`** - Home Manager maintenance
5. **`maintenance-runner.sh`** - Master script orchestrating all tasks

## Disk Space Recovery Potential

Based on analysis, you can potentially recover:
- **~35MB** from Slack logs cleanup
- **~500MB** from old browser cache
- **~1GB** from old Poetry virtualenvs
- **~200MB** from database optimization
- **~100MB** from trash cleanup

**Total potential recovery: ~1.8GB**

## Monitoring

After running maintenance, monitor with:
```bash
# Check system health
~/.local/bin/system-health-check

# View maintenance logs
ls -la ~/.local/log/

# Check disk usage
du -sh ~/.cache ~/.config ~/.local/share/Trash
```

## Troubleshooting

If scripts fail:
1. Check permissions: `chmod +x ~/.local/bin/*.sh`
2. Check dependencies: `bc`, `sqlite3`, `logrotate`
3. Run with `--dry-run` first to preview changes
4. Check logs in `~/.local/log/`

For more help: `~/.local/bin/maintenance-runner.sh --help`
