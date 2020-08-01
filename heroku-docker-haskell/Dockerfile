FROM fpco/stack-build:lts-16.6 as dependencies
RUN mkdir /opt/build
COPY stack.yaml stack.yaml.lock package.yaml /opt/build/
RUN cd /opt/build && stack build --system-ghc --dependencies-only

FROM fpco/stack-build:lts-16.6 as build
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build
RUN cd /opt/build && stack install --system-ghc

FROM fpco/stack-build:lts-16.6 as server
WORKDIR /opt/app
COPY --from=build /root/.local/bin/heroku-docker-haskell .
RUN adduser --disabled-password newuser
USER newuser
CMD /opt/app/heroku-docker-haskell
