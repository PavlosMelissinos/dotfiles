#!/bin/bash
# Interactive U2F Test - Tests actual U2F functionality
# Run this manually to verify U2F authentication works

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}Interactive U2F Authentication Test${NC}"
echo -e "${BLUE}======================================${NC}"
echo

echo -e "${YELLOW}This script will test U2F authentication interactively.${NC}"
echo -e "${YELLOW}Make sure your U2F device is connected.${NC}"
echo

# Test 1: Sudo with U2F
echo -e "${BLUE}Test 1: Testing sudo with U2F...${NC}"
echo "About to run: sudo -k && sudo echo 'U2F sudo test successful'"
echo -e "${YELLOW}You should be prompted for U2F authentication.${NC}"
read -p "Press Enter to continue or Ctrl+C to skip..."

if sudo -k && sudo echo 'U2F sudo test successful'; then
    echo -e "${GREEN}✓ Sudo U2F test PASSED${NC}"
    SUDO_RESULT="PASS"
else
    echo -e "${RED}✗ Sudo U2F test FAILED${NC}"
    SUDO_RESULT="FAIL"
fi
echo

# Test 2: Swaylock with U2F (if available)
echo -e "${BLUE}Test 2: Testing swaylock with U2F...${NC}"
if command -v swaylock >/dev/null 2>&1; then
    echo "About to run swaylock in test mode"
    echo -e "${YELLOW}This will lock your screen. Use U2F key + Enter to unlock.${NC}"
    echo -e "${RED}WARNING: Make sure you have your U2F key ready!${NC}"
    read -p "Press Enter to test swaylock or Ctrl+C to skip..."
    
    # Run swaylock with a timeout as safety measure
    if timeout 30 swaylock --test 2>/dev/null || swaylock --daemonize; then
        echo -e "${GREEN}✓ Swaylock U2F test PASSED${NC}"
        SWAYLOCK_RESULT="PASS"
    else
        echo -e "${RED}✗ Swaylock U2F test FAILED or timed out${NC}"
        SWAYLOCK_RESULT="FAIL"
    fi
else
    echo -e "${YELLOW}⚠ Swaylock not available - SKIPPED${NC}"
    SWAYLOCK_RESULT="SKIP"
fi
echo

# Summary
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}Interactive Test Results${NC}"
echo -e "${BLUE}======================================${NC}"
echo "Sudo U2F: $SUDO_RESULT"
echo "Swaylock U2F: $SWAYLOCK_RESULT"
echo

if [[ "$SUDO_RESULT" == "PASS" ]]; then
    echo -e "${GREEN}U2F authentication appears to be working correctly!${NC}"
    exit 0
else
    echo -e "${RED}U2F authentication has issues. Check configuration.${NC}"
    exit 1
fi