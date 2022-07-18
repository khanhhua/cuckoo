FROM library/haskell:latest as build
USER root
RUN cabal update
WORKDIR /opt/app
COPY . /opt/app
RUN cd /opt/app && cabal install

FROM ubuntu:latest
WORKDIR /opt/app
# NOTICE THIS LINE
COPY ./data /opt/app/data
COPY --from=build /root/.cabal/bin/cuckoobird .
EXPOSE 3000
CMD ["/opt/app/cuckoobird"]