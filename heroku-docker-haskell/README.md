# About

A minimal example of a Haskell server that can be built with one command (`run`) using [docker-compose](https://docs.docker.com/compose/), and can be deployed via Docker to Heroku.

# Run locally

```sh
./run
```

Then in another terminal:
```sh
$ curl localhost:8080
heroku-docker-haskell example running.
```

# Deploy to Heroku

Following the instructions here:

https://devcenter.heroku.com/articles/container-registry-and-runtime

```sh
heroku container:login
heroku create
heroku container:push web
heroku container:release web
heroku open
```
