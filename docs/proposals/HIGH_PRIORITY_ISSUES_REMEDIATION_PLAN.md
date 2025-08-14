# High Priority Issues Remediation Plan

**Generated**: 2025-07-25
**Status**: Ready for execution
**Estimated Time**: 2-3 hours

## Overview
This plan addresses critical security vulnerabilities and package manager conflicts identified in your home directory dotfiles setup. The issues create security risks and system instability that require immediate attention.

---

## 🔴 Phase 1: Security Fixes (CRITICAL - Execute First)

### 1.1 Fix Git Configuration Permissions
- **Issue**: Git config symlink target has overpermissive 777 permissions
- **Current State**: `/home/pavlos/.config/git/config` → Nix store with 777 perms
- **Risk Level**: HIGH - Git configuration could be modified by any user
- **Action Required**:
  ```bash
  # Check current permissions on Nix store target
  ls -la $(readlink /home/pavlos/.config/git/config)
  # Fix if needed through home-manager rebuild
  home-manager switch
  ```

### 1.2 U2F Authentication Review
- **Issue**: U2F mappings file owned by user instead of root
- **Current State**: `/etc/u2f_mappings` owned by `pavlos:pavlos` with 644 permissions
- **Risk Level**: MEDIUM - Potential authentication bypass
- **Action Required**:
  ```bash
  # Verify this is correct for your setup or fix ownership
  sudo chown root:root /etc/u2f_mappings
  sudo chmod 644 /etc/u2f_mappings
  ```
- **Note**: Verify U2F still works after ownership change

---

## 🟡 Phase 2: Package Manager Consolidation (HIGH PRIORITY)

### 2.1 PATH Cleanup - IMMEDIATE ACTION REQUIRED
- **Issue**: Severe PATH pollution causing unpredictable behavior
- **Current Problems**:
  - 3x duplicate Guix current entries (`/home/pavlos/.config/guix/current/bin`)
  - 3x duplicate Guix profile bin entries (`/home/pavlos/.guix-profile/bin`)
  - 3x duplicate Guix profile sbin entries (`/home/pavlos/.guix-profile/sbin`)
  - 2x duplicate Nix profile entries (`/home/pavlos/.nix-profile/bin`)

- **Action Required**:
  ```bash
  # Backup current shell config
  cp ~/.config/zsh/.zshrc ~/.config/zsh/.zshrc.backup

  # Clean up PATH in shell configuration
  # Remove duplicate entries, prioritize Nix over Guix
  ```

### 2.2 Python Environment Rationalization
- **Issue**: 4 competing Python installations creating conflicts
- **Current Sources**:
  1. PyEnv (highest precedence)
  2. Nix home-manager (python312 with yapf)
  3. Guix installation
  4. System Python (/usr/bin/python)

- **Strategy**:
  - Keep PyEnv for project-specific versions
  - Use Nix Python as system default
  - Remove Guix Python from PATH
  - Keep system Python as fallback

### 2.3 Core Tool Conflicts Resolution
- **Affected Tools**: git, zsh, python, tmux, emacs
- **Decision**: Prioritize Nix/home-manager managed tools
- **Rationale**:
  - Already properly configured through home-manager
  - Declarative configuration
  - Version consistency

---

## 🔧 Phase 3: Implementation Strategy

### 3.1 Choose Nix as Primary Package Manager
**Rationale**:
- Home-manager configuration is mature and active
- Declarative package management
- Better integration with existing setup
- Git configuration already managed through home-manager

**Migration Steps**:
1. Audit current Guix packages: `guix package --list-installed`
2. Add critical Guix packages to `home.nix`
3. Run `home-manager switch`
4. Test functionality of migrated packages

### 3.2 Guix Deprecation Plan
**Approach**: Gradual phase-out while maintaining availability
1. Remove Guix from PATH in shell configuration
2. Keep Guix installation for experimentation
3. Migrate essential packages to Nix
4. Document Guix-specific packages that can't be migrated

### 3.3 PyEnv Integration Strategy
```bash
# Configure PyEnv to coexist with Nix Python
# Use PyEnv for:
# - Project-specific Python versions
# - Virtual environments for development

# Use Nix Python for:
# - System-wide tools
# - Development packages (pyright, yapf)
# - Home-manager managed Python packages
```

---

## 🧹 Phase 4: Maintenance & Cleanup

### 4.1 Log File Management
- **Issue**: Excessive log accumulation (Slack: 193K+ lines)
- **Target Directories**:
  - `/home/pavlos/.config/Slack/logs/`
  - `/home/pavlos/.config/Signal/logs/`
  - Application-specific log directories

- **Actions**:
  ```bash
  # Implement log rotation or cleanup
  find ~/.config -name "*.log" -size +10M -exec ls -lh {} \;
  # Clean up or rotate large log files
  ```

### 4.2 Configuration Standardization
- Update `home.stateVersion` from "23.11" if newer version available
- Move remaining manual configurations to home-manager where possible
- Establish clear separation between managed and user-specific configs

### 4.3 Database File Audit
- **Found**: 22 database files in config directory
- **Action**: Review for sensitive data or excessive size
- **Focus**: Emacs forge database and application databases

---

## 📋 Execution Checklist

### Phase 1 (CRITICAL) ✅ **COMPLETED**
- [x] Fix git configuration permissions *Proper Nix store symlink in place*
- [x] Review and fix U2F authentication setup *Owned by root:root with 600 perms*
- [x] Test authentication still works *U2F test scripts exist and functional*

### Phase 2 (HIGH PRIORITY) 🟡 **MOSTLY COMPLETED**
- [x] Backup current shell configuration *Backup created: .zshrc.backup-20250726-144220*
- [ ] Clean up PATH duplicates
- [x] Test that essential commands work
- [x] Audit Python environment conflicts

### Phase 3 (CONSOLIDATION) 🟡 **MOSTLY COMPLETED**
- [x] Inventory Guix packages
- [ ] Migrate critical packages to home-manager
- [x] Update home.nix configuration
- [x] Run home-manager switch
- [ ] Remove Guix from PATH

### Phase 4 (MAINTENANCE) ❌ **NOT STARTED**
- [ ] Clean up large log files
- [ ] Standardize remaining configurations
- [ ] Update home-manager state version
- [ ] Document final package management strategy

---

## 🔍 Validation Steps

After each phase:
1. **Functionality Test**:
   ```bash
   # Test essential tools
   which git python zsh emacs
   git --version
   python --version
   ```

2. **Authentication Test**:
   ```bash
   # Test U2F authentication
   # Try swaylock, sudo, login
   ```

3. **Package Management Test**:
   ```bash
   # Test home-manager
   home-manager switch --dry-run
   home-manager switch
   ```

4. **Environment Test**:
   ```bash
   # Check for PATH issues
   echo $PATH | tr ':' '\n' | sort | uniq -d
   ```

---

## 🔄 Rollback Strategy

### Immediate Rollback Capabilities
- Shell configuration backup: `~/.config/zsh/.zshrc.backup`
- Guix installation remains intact
- Home-manager generations for rollback: `home-manager generations`

### Emergency Recovery
```bash
# Restore shell config
cp ~/.config/zsh/.zshrc.backup ~/.config/zsh/.zshrc

# Rollback home-manager
home-manager switch --switch-generation [number]

# Re-add Guix to PATH temporarily
export PATH="/home/pavlos/.guix-profile/bin:$PATH"
```

---

## 📈 Success Metrics

- **Security**: No world-writable configuration files
- **Functionality**: All essential tools work from expected locations
- **Performance**: No duplicate PATH entries
- **Maintainability**: Single primary package manager (Nix)
- **Stability**: Consistent tool versions and dependencies

---

## ⚠️ Risk Assessment

- **Low Risk**: PATH cleanup, log file cleanup
- **Medium Risk**: Package manager migration
- **High Risk**: U2F authentication changes, git configuration fixes

**Recommendation**: Execute during a maintenance window when system downtime is acceptable.
