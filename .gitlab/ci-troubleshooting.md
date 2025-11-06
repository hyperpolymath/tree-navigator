# GitLab CI/CD Troubleshooting Guide

## Common Issues

### Build Failures

#### "GNAT not found"
**Problem:** GNAT compiler not available in PATH
**Solution:** Check that `.gnat_base` template is being used

#### "gprbuild: command not found"
**Problem:** GPRbuild not installed
**Solution:** Ensure `before_script` in `.gnat_base` runs successfully

### Test Failures

#### Permission denied errors
**Problem:** Test files not executable
**Solution:** Add `chmod +x` before running scripts

#### "No such file or directory"
**Problem:** Artifacts not properly passed between stages
**Solution:** Check `dependencies` and `needs` in job configuration

### Deployment Issues

#### "kubectl: command not found"
**Problem:** kubectl not installed in deploy job
**Solution:** Check `.deploy_base` includes kubectl installation

#### "unauthorized: authentication required"
**Problem:** Not logged into container registry
**Solution:** Verify `CI_REGISTRY_PASSWORD` is available

### Performance Issues

#### Jobs taking too long
**Solutions:**
- Enable caching: Use `.cache_template`
- Parallelize builds: Use `-j0` flag for gprbuild
- Use `needs:` instead of `dependencies:` where possible

## Viewing Logs

### Job logs
```
GitLab UI > CI/CD > Pipelines > [Pipeline] > [Job]
```

### Artifacts
```
GitLab UI > CI/CD > Pipelines > [Pipeline] > [Job] > Browse
```

## Manual Pipeline Triggers

Run pipeline manually:
```
GitLab UI > CI/CD > Pipelines > Run Pipeline
```

With variables:
```
Add variable: KEY=VALUE
```

## Debugging Tips

### Enable debug mode
Add to job:
```yaml
variables:
  CI_DEBUG_TRACE: "true"
```

### Run job locally with GitLab Runner
```bash
gitlab-runner exec docker job_name
```

### Check pipeline configuration
```bash
# Lint .gitlab-ci.yml
gitlab-ci-lint < .gitlab-ci.yml
```

## Getting Help

1. Check job logs for error messages
2. Review this troubleshooting guide
3. Check GitLab CI/CD documentation
4. Open an issue with:
   - Job log excerpt
   - Pipeline URL
   - Environment details
