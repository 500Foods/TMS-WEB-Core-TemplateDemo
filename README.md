# TMS WEB Core Template: Demo
This repository contains an example of using [TMS WEB Core](https://www.tmssoftware.com/site/tmswebcore.asp) with the [AdminLTE 4](https://github.com/ColorlibHQ/AdminLTE/tree/v4-dev) (Bootstrap 5) template as a front-end to the [TMS XData Template: Demo Data](https://github.com/500Foods/TMS-XData-TemplateDemoData) REST API server. This project originated as part of a series of blog posts about using [TMS XData](https://www.tmssoftware.com/site/tmswebcore.asp) and [TMS WEB Core](https://www.tmssoftware.com/site/tmswebcore.asp) with different kinds of templates, the first of which can be found [here](https://www.tmssoftware.com/site/blog.asp?post=1068).

## Getting Started

Initially, only a few features have been implemented.  Starting with a Login page and a simple dashboard.  It is expected that this project will continue to advance in step with the XData project. However, it should be immediately usable, with the ability to login to the XData server, assuming that all the defaults are used in terms of ports, passwords and son on.  When the web application starts normally, the login form is displayed.

![image](https://user-images.githubusercontent.com/41052272/223620295-e22f9de8-c351-41a2-ab24-390d23052fa0.png)
*Project Login Form*

NOTE: The project can be configured for multiple icon sources.  Font Awesome 6 Pro Duotone icons are shown in this example. While the project supports these as a configuration option, these icons (and the required license) are not included in this project.

## Deployment

An optional configuration file can be used to pass the URL for the XData server when deploying this project in a production setting (or anywhere other than the default development environment).  At the moment, this is just a JSON file with a single entry.  The app looks for it in 'td_configuration.json' in the same folder as the app itself (wherever Project1.html is, for example). This needs to be readable by the app.  If there are any access issues, be sure that it is accessible from the browser first (eg: http://localhost/Project1/td_configuration.json).  Here's an example of what the file should contain.
```
{
  "Server": "http://localhost:12345/tms/xdata"
}
```
Note also that this mechanism can be used to test development client code against something other than the development XData server.  For example, the Server value could be pointed at a different test or production server.

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
- [Bootstrap](https://getbootstrap.com/) - No introduction needed
- [Tabulator](https://www.tabulator.info) - Fantastic pure JavaScript web data tables
- [Font Awesome](https://www.fontawesome.com) - The very best icons
- [Material Design Icons](https://pictogrammers.com/library/mdi/) - Used throughout Home Assistant
- [Luxon](https://moment.github.io/luxon/#/) - For handling date/time conversions

## Additional Notes
While this project is currently under active development, feel free to give it a try and post any issues you encounter.  Or start a discussion if you would like to help steer the project in a particular direction.  Early days yet, so a good time to have your voice heard.  As the project unfolds, additional resources will be made available, including platform binaries, more documentation, demos, and so on.

## Repository Information
[![Count Lines of Code](https://github.com/500Foods/TMS-WEB-Core-TemplateDemo/actions/workflows/main.yml/badge.svg)](https://github.com/500Foods/TMS-WEB-Core-TemplateDemo/actions/workflows/main.yml)
<!--CLOC-START -->
```
Last Updated at 2023-12-24 05:15:46 UTC
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
CSS                              5           1933             92          26324
Pascal                          11            483            731           2032
HTML                             8            129            198           1215
JavaScript                       3             21            226           1134
Delphi Form                     10              0              0            888
Markdown                         1             18              2             56
YAML                             2              8             12             33
PowerShell                       1             15             26              6
-------------------------------------------------------------------------------
SUM:                            41           2607           1287          31688
-------------------------------------------------------------------------------
```
<!--CLOC-END-->

## Sponsor / Donate / Support
If you find this work interesting, helpful, or valuable, or that it has saved you time, money, or both, please consider directly supporting these efforts financially via [GitHub Sponsors](https://github.com/sponsors/500Foods) or donating via [Buy Me a Pizza](https://www.buymeacoffee.com/andrewsimard500). Also, check out these other [GitHub Repositories](https://github.com/500Foods?tab=repositories&q=&sort=stargazers) that may interest you.

## More TMS WEB Core and TMS XData Content
If you're interested in other TMS WEB Core and TMS XData content, follow along on 𝕏 at [@WebCoreAndMore](https://x.com/WebCoreAndMore), join our 𝕏 [Web Core and More Community](https://twitter.com/i/communities/1683267402384183296), or check out the [TMS Software Blog](https://www.tmssoftware.com/site/blog.asp).
