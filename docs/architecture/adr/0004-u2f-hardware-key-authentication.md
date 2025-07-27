# ADR-0004: U2F Hardware Key Authentication

**Date**: 2025-07-27
**Status**: Accepted

## Context

System authentication traditionally relies on passwords, which have several
security limitations:
- Vulnerable to phishing attacks
- Can be compromised through data breaches
- Users often reuse weak passwords
- No protection against keyloggers or shoulder surfing

Hardware security keys using U2F (Universal 2nd Factor) protocol provide:
- Cryptographic authentication resistant to phishing
- Protection against credential stuffing attacks
- Physical presence requirement for authentication
- Support for multiple keys for redundancy

Integration points in this system:
- GDM (GNOME Display Manager) login
- sudo authentication for administrative tasks
- Swaylock screen unlock
- Web services (GitHub, GitLab, cloud providers)

## Decision

We will implement U2F hardware key authentication as the primary second factor
for system and service authentication.

Configuration approach:
- Use pam_u2f module for system-level authentication
- Store key mappings in `/etc/u2f_mappings` for system-wide access
- Configure multiple hardware keys for redundancy
- Integrate with GDM, sudo, and swaylock
- Use hardware keys for all supported web services

We will maintain password authentication as a fallback mechanism in case
hardware keys are unavailable.

## Consequences

**Positive:**
- Significantly improved security against phishing and credential attacks
- Reduced reliance on memorized passwords
- Protection against keyloggers and shoulder surfing
- Consistent authentication experience across system and web services
- Regulatory compliance benefits for sensitive data
- Multiple key support provides redundancy

**Negative:**
- Dependency on physical hardware that can be lost or damaged
- Additional cost for purchasing multiple hardware keys
- Potential compatibility issues with some services or applications
- Recovery complexity if all keys are lost
- Need to carry hardware keys for authentication
- Some applications may not support U2F protocol
