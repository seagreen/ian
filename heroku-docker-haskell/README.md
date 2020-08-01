# Run on Heroku

(This doesn't work, see below)

Following the instructions here:

https://devcenter.heroku.com/articles/container-registry-and-runtime

```sh
heroku container:login
heroku create
heroku container:push web
heroku container:release web
heroku open
```

And then running:
```sh
heroku logs --tail
```

I get:
```
2020-08-01T19:20:00.504119+00:00 heroku[web.1]: State changed from down to starting
2020-08-01T19:21:29.510832+00:00 heroku[router]: at=error code=H20 desc="App boot timeout" method=GET path="/" host=still-caverns-93130.herokuapp.com request_id=ce95bda9-1424-4cd9-acfb-c279add392df fwd="69.151.217.99" dyno= connect= service= status=503 bytes= protocol=https
2020-08-01T19:22:02.935712+00:00 heroku[web.1]: State changed from starting to down
2020-08-01T19:22:17.414297+00:00 heroku[web.1]: Starting process with command `/bin/sh -c /opt/app/heroku-docker-haskell`
2020-08-01T19:22:23.438503+00:00 heroku[web.1]: Stopping all processes with SIGTERM
2020-08-01T19:22:23.512124+00:00 heroku[web.1]: Process exited with status 143
```

For more info on H20 app boot timeout error: https://devcenter.heroku.com/articles/error-codes#h20-app-boot-timeout

# Run locally

(This does work)

```sh
./run
```

Then in another terminal:
```sh
curl localhost:8080
# heroku-docker-haskell example running.
```
