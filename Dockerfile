FROM haskell:7.8

RUN apt-get update
RUN apt-get install -y libffi-dev

ADD . .

RUN cd /
RUN cabal sandbox init
RUN cabal update
RUN cabal install

WORKDIR /
CMD [ "/dist/build/bs-gen/bs-gen" ]
