FROM haskell:8

RUN apt-get update
RUN apt-get install -y libffi-dev

RUN mkdir app

ADD Setup.hs app/Setup.hs
ADD src app/src
ADD bs-gen.cabal app/bs-gen.cabal

RUN cd app

RUN stack install --install-ghc

WORKDIR /
CMD [ "/dist/build/bs-gen/bs-gen" ]
