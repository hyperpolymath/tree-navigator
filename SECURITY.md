# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |

## Security Requirements

This project follows the RSR (Rhodium Standard Repository) security guidelines:

- **Cryptography**: No MD5/SHA1 for security purposes (SHA256+ required)
- **Network**: HTTPS only (no plaintext HTTP URLs)
- **Secrets**: No hardcoded secrets; use environment variables
- **Dependencies**: SHA-pinned GitHub Actions for supply-chain security
- **Analysis**: CodeQL SAST and OSSF Scorecard enabled

## Reporting a Vulnerability

**Please do NOT report security vulnerabilities through public GitHub issues.**

Instead, report vulnerabilities via:
1. **GitHub Security Advisories**: Use the "Security" tab → "Report a vulnerability"
2. **Email**: Contact the maintainer directly (see CITATION.cff)

### What to Include

- Type of vulnerability (e.g., buffer overflow, injection, etc.)
- Full paths of affected source files
- Step-by-step reproduction instructions
- Proof-of-concept or exploit code (if available)
- Impact assessment

### Response Timeline

- **Initial response**: Within 48 hours
- **Status update**: Within 7 days
- **Fix timeline**: Depends on severity (critical: 24-72h, high: 1-2 weeks)

### Safe Harbor

We follow responsible disclosure. Security researchers acting in good faith are protected from legal action.
