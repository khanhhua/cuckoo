FROM library/haskell:latest as build
USER root
RUN cabal update
WORKDIR /opt/app
COPY . /opt/app
RUN mkdir /opt/build && cabal install --installdir=/opt/tmp
RUN cp $(readlink /opt/tmp/cuckoobird) /opt/build

FROM ubuntu:latest
WORKDIR /opt/app
# NOTICE THIS LINE
COPY ./data /opt/app/data
COPY --from=build /opt/build/cuckoobird .
EXPOSE 3000
CMD ["/opt/app/cuckoobird"]