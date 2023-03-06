# PostBuild.PS1
#
# This is run after Delphi has finished its build of the application.
#
echo "PostBuild Script is Running"



# Files that we might be updating
$ProjectFile=$args[0].Substring(0,$args[0].Length-1)+'\Project1.html'
$TemplateFile=$args[0].Substring(0,$args[0].Length-1)+'\css\template.css'
# echo $args[0]
# echo $ProjectFile
# echo $TemplateFile



# This replaces the default Font Awesome 6 Free library with a Font Awesome 6 Pro (Kit) library
# echo " - Updating Icon Set in Project HTML"
# $SearchFor='iconset="default"'
# $ReplaceWith='iconset="Duotone"'
# (Get-Content $ProjectFile) -replace [regex]::Escape($SearchFor), $ReplaceWith | Out-File -encoding ASCII $ProjectFile

# echo " - Updating Font Awesome Library in Project HTML"
# $SearchFor='<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@fortawesome/fontawesome-free@6.2.1/css/all.min.css" integrity="sha256-Z1K5uhUaJXA7Ll0XrZ/0JhX4lAtZFpT6jkKrEDT0drU=" crossorigin="anonymous">'
# $ReplaceWith='<script crossorigin="anonymous" src="https://kit.fontawesome.com/1234567890.js"></script>'
# (Get-Content $ProjectFile) -replace [regex]::Escape($SearchFor), $ReplaceWith | Out-File -encoding ASCII $ProjectFile



# This replaces the default Source Sans Pro font with Cairo
# echo " - Replacing Source Sans Pro with Cairo in Project HTML"
# $SearchFor='<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,400i,700&display=fallback">'
# $ReplaceWith='<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Cairo:300,400,400i,700&display=fallback">'
# (Get-Content $ProjectFile) -replace [regex]::Escape($SearchFor), $ReplaceWith | Out-File -encoding ASCII $ProjectFile

# echo " - Replacing Source Sans Pro with Cairo in Template CSS"
# $SearchFor='font-family: "Source Sans Pro";'
# $ReplaceWith='font-family: "Cairo";'
# (Get-Content $TemplateFile) -replace [regex]::Escape($SearchFor), $ReplaceWith | Out-File -encoding ASCII $TemplateFile
 


echo "PostBuild Script is Complete"

