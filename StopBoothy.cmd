start "" "%PROGRAMFILES%\Git\bin\sh.exe" -c "docker compose down && docker image rm dsa3101-2210-03-priv-shiny:latest && docker image rm dsa3101-2210-03-priv-flask:latest"
taskkill/im "Docker Desktop.exe"
