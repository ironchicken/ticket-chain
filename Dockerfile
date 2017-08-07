FROM haskell:8
MAINTAINER Richard Lewis <richard@rjlewis.me.uk>

ENV HOME /root

RUN apt-get update &&\
    apt-get install -y --no-install-recommends build-essential pkg-config
RUN cabal update

COPY ticket-chain.cabal /root/ticket-chain.cabal

WORKDIR /root

RUN cabal sandbox init
RUN cabal configure; exit 0
RUN cabal install --only-dependencies

COPY src /root/src
COPY prog /root/prog
COPY tests /root/tests
COPY Setup.hs /root/Setup.hs

RUN cabal build &&\
    cabal install

ENTRYPOINT [".cabal-sandbox/bin/ticket-wallet"]
