#!/bin/bash
# U2F Authentication Test Suite
# Tests U2F functionality before/after configuration changes

set -euo pipefail

SCRIPT_NAME="$(basename "$0")"
TIMESTAMP="$(date '+%Y-%m-%d %H:%M:%S')"
LOG_FILE="$HOME/.cache/u2f-test-$(date '+%Y%m%d-%H%M%S').log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo "[$TIMESTAMP] $1" | tee -a "$LOG_FILE"
}

# Test result tracking
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0

# Test result functions
test_pass() {
    echo -e "${GREEN}✓ PASS${NC}: $1"
    log "PASS: $1"
    ((TESTS_PASSED++))
}

test_fail() {
    echo -e "${RED}✗ FAIL${NC}: $1"
    log "FAIL: $1"
    ((TESTS_FAILED++))
}

test_skip() {
    echo -e "${YELLOW}⚠ SKIP${NC}: $1"
    log "SKIP: $1"
    ((TESTS_SKIPPED++))
}

test_info() {
    echo -e "${BLUE}ℹ INFO${NC}: $1"
    log "INFO: $1"
}

# Header
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}U2F Authentication Test Suite${NC}"
echo -e "${BLUE}Started: $TIMESTAMP${NC}"
echo -e "${BLUE}Log: $LOG_FILE${NC}"
echo -e "${BLUE}======================================${NC}"
echo

# Test 1: Check U2F mappings file exists and permissions
test_info "Test 1: U2F mappings file check"
if [[ -f /etc/u2f_mappings ]]; then
    PERMS=$(stat -c "%a" /etc/u2f_mappings)
    OWNER=$(stat -c "%U:%G" /etc/u2f_mappings)
    test_pass "U2F mappings file exists: /etc/u2f_mappings"
    test_info "Permissions: $PERMS, Owner: $OWNER"
    
    # Check if readable by current user
    if [[ -r /etc/u2f_mappings ]]; then
        test_pass "U2F mappings file is readable by current user"
    else
        test_fail "U2F mappings file is NOT readable by current user"
    fi
else
    test_fail "U2F mappings file does not exist: /etc/u2f_mappings"
fi
echo

# Test 2: Check PAM configuration files
test_info "Test 2: PAM configuration check"
PAM_FILES=("/etc/pam.d/sudo" "/etc/pam.d/swaylock" "/etc/pam.d/gdm-password")

for pam_file in "${PAM_FILES[@]}"; do
    if [[ -f "$pam_file" ]]; then
        if grep -q "pam_u2f.so" "$pam_file"; then
            test_pass "U2F configured in $pam_file"
            # Show the actual line
            test_info "$(grep "pam_u2f.so" "$pam_file" | head -1)"
        else
            test_skip "U2F not configured in $pam_file"
        fi
    else
        test_skip "$pam_file does not exist"
    fi
done
echo

# Test 3: Check if U2F device is detected
test_info "Test 3: U2F device detection"
if command -v lsusb >/dev/null 2>&1; then
    # Look for common U2F device vendors
    U2F_VENDORS=("Yubico" "Feitian" "Nitrokey" "SoloKeys")
    DEVICE_FOUND=false
    
    for vendor in "${U2F_VENDORS[@]}"; do
        if lsusb | grep -qi "$vendor"; then
            test_pass "U2F device detected: $(lsusb | grep -i "$vendor")"
            DEVICE_FOUND=true
            break
        fi
    done
    
    if [[ "$DEVICE_FOUND" == false ]]; then
        test_skip "No known U2F devices detected via lsusb"
    fi
else
    test_skip "lsusb not available for device detection"
fi
echo

# Test 4: Test sudo authentication (dry run - check configuration)
test_info "Test 4: Sudo U2F configuration test"
if sudo -n true 2>/dev/null; then
    test_info "Current user has passwordless sudo (skipping interactive test)"
else
    test_info "Sudo requires authentication - U2F would be triggered here"
    test_info "To test manually: run 'sudo -k && sudo echo test' and use U2F key"
fi
echo

# Test 5: Test if pamu2fcfg is available
test_info "Test 5: U2F tools availability"
if command -v pamu2fcfg >/dev/null 2>&1; then
    test_pass "pamu2fcfg tool is available"
    test_info "Version: $(pamu2fcfg --version 2>&1 | head -1 || echo 'version unknown')"
else
    test_fail "pamu2fcfg tool is NOT available"
fi

if command -v pam_u2f >/dev/null 2>&1; then
    test_pass "pam_u2f is available"
else
    test_skip "pam_u2f command not found (this is normal)"
fi
echo

# Test 6: Swaylock configuration check
test_info "Test 6: Swaylock U2F configuration"
if [[ -f /etc/pam.d/swaylock ]]; then
    if grep -q "pam_u2f.so" /etc/pam.d/swaylock; then
        test_pass "Swaylock is configured for U2F"
        # Check if swaylock is available
        if command -v swaylock >/dev/null 2>&1; then
            test_pass "Swaylock binary is available"
            test_info "To test manually: run 'swaylock' and use U2F key + Enter"
        else
            test_fail "Swaylock binary is NOT available"
        fi
    else
        test_skip "Swaylock is not configured for U2F"
    fi
else
    test_skip "Swaylock PAM file does not exist"
fi
echo

# Test 7: Check U2F mappings file content (basic validation)
test_info "Test 7: U2F mappings file content validation"
if [[ -r /etc/u2f_mappings ]]; then
    LINES=$(wc -l < /etc/u2f_mappings)
    test_info "U2F mappings file has $LINES lines"
    
    # Basic format check (should contain username and key data)
    if grep -q "^[a-zA-Z].*:" /etc/u2f_mappings; then
        test_pass "U2F mappings file appears to have valid format"
    else
        test_fail "U2F mappings file format appears invalid"
    fi
    
    # Check file size (should be reasonable for key data)
    SIZE=$(stat -c "%s" /etc/u2f_mappings)
    if [[ $SIZE -gt 50 && $SIZE -lt 10000 ]]; then
        test_pass "U2F mappings file size is reasonable ($SIZE bytes)"
    else
        test_fail "U2F mappings file size is suspicious ($SIZE bytes)"
    fi
else
    test_skip "Cannot read U2F mappings file for content validation"
fi
echo

# Summary
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}Test Summary${NC}"
echo -e "${BLUE}======================================${NC}"
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
echo -e "${RED}Failed: $TESTS_FAILED${NC}"
echo -e "${YELLOW}Skipped: $TESTS_SKIPPED${NC}"
echo -e "${BLUE}Total: $((TESTS_PASSED + TESTS_FAILED + TESTS_SKIPPED))${NC}"
echo
echo -e "${BLUE}Log saved to: $LOG_FILE${NC}"

# Exit with appropriate code
if [[ $TESTS_FAILED -gt 0 ]]; then
    echo -e "${RED}Some tests failed. Review the results before making changes.${NC}"
    exit 1
else
    echo -e "${GREEN}All tests passed or were skipped. System appears functional.${NC}"
    exit 0
fi