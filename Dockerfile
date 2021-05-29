# we need an old version of debian with gcc-4.9 otherwise the version of TinyOS doesn't work
# it is possible that we could upgrade TinyOS to a newer version and use a newer toolchain
FROM debian:jessie

# quick and dirty container that is able to compile and run the code
# FIXME: the container could be made smaller


RUN apt-get update && apt-get install -y --no-install-recommends unzip build-essential python wget ca-certificates automake autoconf libtool libc6-dev python-serial emacs gperf bison flex libpython2.7-dev vim default-jdk

RUN ln -s /usr/bin/x86_64-linux-gnu-python2.7-config /usr/local/bin/python2.73-config

WORKDIR /tmp/nesc


# this version seems is the oldest that can compile with emacs included in jessie
ENV NESC_VERSION 1.3.6

ENV NESC_URL https://github.com/tinyos/nesc/archive/refs/tags/v${NESC_VERSION}.zip
ENV NESC_FILE v${NESC_VERSION}.zip

RUN wget ${NESC_URL} && \
    unzip ${NESC_FILE} && \
    cd nesc-* && \
    ./Bootstrap && \
    ./configure && \
    make && make install && \
    rm -rf /tmp/nesc

WORKDIR /opt

ENV TOSROOT /opt/tinyos-main-release_tinyos_2_1_2/
ENV MAKERULES ${TOSROOT}/support/make/Makerules
ENV TOSDIR /opt/tinyos-main-release_tinyos_2_1_2/tos/

RUN wget https://github.com/tinyos/tinyos-main/archive/refs/tags/release_tinyos_2_1_2.zip && \
    unzip release_tinyos_2_1_2.zip && \
    rm release_tinyos_2_1_2.zip

RUN cd /opt/tinyos-main-release_tinyos_2_1_2/tools && \
    ./Bootstrap && \
    ./configure && \
    make install < /dev/null

WORKDIR /opt/sensecode
