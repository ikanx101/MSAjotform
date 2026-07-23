docker build -t ikanx101/shiny-msa .

docker run -p 3131:3838 -d --name msa_converter --restart unless-stopped ikanx101/shiny-msa:latest