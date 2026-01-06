# read me

## how to build

_login_ dulu

```
docker login
```

```
docker build -t ikanx101/shiny-msa .
```

```
sudo docker tag ikanx101/shiny-msa:latest ikanx101/shiny-msa:latest
sudo docker push ikanx101/shiny-msa:latest
```

## how to run

```
docker run -p 3838:3838 ikanx101/shiny-msa
```
