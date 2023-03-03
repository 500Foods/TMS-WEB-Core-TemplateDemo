# TMS WEB Core Template: Demo
This repository contains an example of using [TMS WEB Core](https://www.tmssoftware.com/site/tmswebcore.asp) with the [AdminLTE 4](https://github.com/ColorlibHQ/AdminLTE/tree/v4-dev) (Bootstrap 5) template as a front-end to the [TMS XData Template: Demo Data](https://github.com/500Foods/TMS-XData-TemplateDemoData) REST API server.

More details will be provded as this project unfolds.  Initially, only a few features have been implemented.  Starting with a Login page and a simple dashboard.  It is expected that this project will continue to advance in step with the XData project.

## Usage Note: Post-Build Command

The Delphi project included in this repository is configured with a post-build command that is used to further customize the html and css files that are generated after this project is built.  The command defined in the project file references a script that is outside this repository. It should be updated to reference the script that is included in the PostBuild folder in this repository.  This change can be implemented by changing the  **Project** | **Options** | **Build Events** | **Post-build events** | **Command** entry:

*Replace*

```powershell -ExecutionPolicy Unrestricted -file ..\BuildScripts\PostBuildTemplate.ps1 "$(OUTPUTDIR)"```

*With*

```powershell -ExecutionPolicy Unrestricted -file .\BuildScripts\PostBuildTemplate.ps1 "$(OUTPUTDIR)"```

This just involves removing a period to reference the script in this project folder rather than a script outside this project folder. The repository was configured this way to allow for personalized build scripts.  If you wish to customize this particular build script, simply move (or copy) the BuildScripts folder up one level and leave this command as-is.  You can then customize the build script however you like without it being overwritten by the repository.

## Key Dependencies
As with any modern web application, other JavaScript libraries/dependencies have been used in this project. Most of the time, this is handled via a CDN link (usually JSDelivr) in the Project.html file. In some cases, for performance or other reasons, they may be included directly.
- [TMS WEB Core](https://www.tmssoftware.com/site/tmswebcore.asp) - This is a TMS WEB Core project, after all
- [AdminLTE 4](https://github.com/ColorlibHQ/AdminLTE/tree/v4-dev) - Naturally
- [Tabulator](https://www.tabulator.info) - Fantastic pure JavaScript web data tables
- [Font Awesome](https://www.fontawesome.com) - The very best icons

## Sponsor / Donate / Support
If you find this work interesting, helpful, or useful, or that it has sved you time, money, or both, please consider direclty supporting these efforts financially via [GitHub Sponsors](https://github.com/sponsors/500Foods) or donating via [Buy Me a Pizza](https://www.buymeacoffee.com/andrewsimard500). Also, be sure to check out these other [GitHub Repositories](https://github.com/500Foods?tab=repositories&q=&sort=stargazers) that may be of interest to you.
