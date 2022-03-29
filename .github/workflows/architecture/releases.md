# Release Process

```mermaid
sequenceDiagram
  Git Client->>Project Repo: Semver Tag is Pushed (ex. v1.2.5)
  Project Repo->>Project Repo: Create a draft release
  Project Repo->>Project Repo: Run Builds
  loop Every build we intend to release
    Project Repo->>Project Repo: Upload Asset to Draft Release
    opt If All assets have been uploaded
      Project Repo->>Project Repo: Publish the release
    end
  end
  Launcher Repo->>Project Repo: Pull down latest/pinned release for it's process
```
