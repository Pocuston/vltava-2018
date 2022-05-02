# Prerequisities

1.  Docker
2.  Elm (https://guide.elm-lang.org/install.html)

# First time setup

```
npm install -g elm-live elm-format elm-test
npm install -g graphcool
cd server
graphcool local up
```

# Run development

```
cd server
graphcool deploy
cd client
elm-live index.elm --open --port=8001 
cd ../
docker-compose up
```
Then, open http://localhost/vltava for application with live reloading.
For debugging run elm-reactor and open http://localhost:8000/index.debug.html
```
elm-reactor
```
# Run tests
```
elm-test --watch
```

# Development API URLs
```
Simple API:        http://localhost:60000/simple/v1/cjglzx2tc00040138dwoe11xk
Relay API:         http://localhost:60000/relay/v1/cjglzx2tc00040138dwoe11xk
Subscriptions API: ws://localhost:60000/subscriptions/v1/cjglzx2tc00040138dwoe11xk
```

# Production API
```
Simple API:        http://localhost:60000/simple/v1/cjgt2cc9r00080125d98qhmrk
Relay API:         http://localhost:60000/relay/v1/cjgt2cc9r00080125d98qhmrk
Subscriptions API: ws://localhost:60000/subscriptions/v1/cjgt2cc9r00080125d98qhmrk
```

# Build
```
elm-make index.elm --output=./build/index.js
```

# Deploy database changes
```
graphcool deploy --target=prod
```

# Reference

* [elm-bulma](http://package.elm-lang.org/packages/surprisetalk/elm-bulma/6.0.2)
