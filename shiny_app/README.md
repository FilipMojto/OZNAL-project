
# Shiny App

This repository contains a Shiny application for linear/polynomial regression analysis.

## Quickstart Guide for Collaborators

1. **Clone the repository**  
   ```bash
   git clone https://github.com/FilipMojto/OZNAL-project.git
   cd OZNAL-project/shiny_app/app
   ```

2. **Install `renv`** (if not already installed)  
   In your R console:
   ```r
   install.packages("renv", repos = "https://cloud.r-project.org/")
   ```

3. **Restore R package dependencies**  
   Activate the project and restore the exact package versions:
   ```r
   renv::activate()
   renv::restore(prompt = FALSE)
   ```

4. **Run the Shiny app**  
   ```r
   shiny::runApp('.', host = '0.0.0.0', port = 3838)
   ```

---

## 🗂️ Files Included

Make sure the following are in the repository so collaborators can run the app successfully:

```
shiny_app/
├── app/                        
│   ├── app.R                   # Shiny application entrypoint
│   └── renv.lock               # Snapshot of project dependencies
├── Dockerfile                  # (Optional) Docker setup file
├── README.md                   # This guide
└── .gitignore                  # Excludes renv/library/, .RData, etc.
```

- **`app.R`** (or `ui.R` + `server.R`): Core Shiny code.  
- **`renv.lock`**: Ensures reproducible package versions.  
- **`Dockerfile`**: (Optional) Containerizes the app.  
- **`.gitignore`**: Excludes local artifacts (private library, workspace files).

---

## Optional: Docker

Build and run with Docker:

```bash
docker build -t my-shiny-app .
docker run -p 3838:3838 --rm -v $(pwd)/app:/app my-shiny-app
```
