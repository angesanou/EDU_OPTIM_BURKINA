@echo off
REM ==========================================================================
REM  EDU-OPTIM BURKINA - lancement par double-clic (Windows)
REM  Prerequis : R 4.2 ou plus recent (https://cran.r-project.org)
REM ==========================================================================
setlocal
set "RS="
for /d %%d in ("C:\Program Files\R\R-*") do set "RS=%%d\bin\Rscript.exe"
if not defined RS for /d %%d in ("%LOCALAPPDATA%\Programs\R\R-*") do set "RS=%%d\bin\Rscript.exe"
if not defined RS for /f "delims=" %%r in ('where Rscript 2^>nul') do set "RS=%%r"
if not defined RS (
  echo R est introuvable sur ce poste.
  echo Installez R depuis https://cran.r-project.org puis relancez ce fichier.
  pause
  exit /b 1
)
cd /d "%~dp0"
echo R utilise : %RS%
echo Demarrage d'EDU-OPTIM BURKINA...
echo Le navigateur va s'ouvrir. Gardez cette fenetre ouverte ; fermez-la pour arreter l'application.
"%RS%" "%~dp0demarrer.R"
if errorlevel 1 (
  echo.
  echo Le lancement a echoue : lisez le message ci-dessus.
)
pause
