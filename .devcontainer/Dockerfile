ARG VARIANT=ubuntu-20.04
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

RUN mkdir -p /sdk
WORKDIR /sdk

RUN git clone https://github.com/cc65/cc65.git
WORKDIR /sdk/cc65
RUN nice make -j2
RUN make install PREFIX=/usr prefix=/usr
