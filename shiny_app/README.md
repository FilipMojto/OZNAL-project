## 📁 App Structure

```
shiny_app/
├── app/                        # R working directory
├── app/renv                    # Renv directory with pre-installed packages
├── app/app.R                   # R Markdown technical documentation
├── app/renv.lock               # Dependency list of renv
├── Dockerfile                  # Docker Image setup
├── README.md                   # App description (this file)
```

## Build the image

```bash
    docker build -t <image_name> .
```

## Run the container

We recommend attaching volume for dynamic code midifications without needing to rebuild the
whole image.

```bash
     docker run -p 3838:3838 -v <absolute_path_to_app_folder>:/app linear_regression
```
