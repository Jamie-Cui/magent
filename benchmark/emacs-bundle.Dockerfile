# syntax=docker/dockerfile:1
FROM ubuntu:22.04 AS build

ARG EMACS_VERSION=29.4
ARG EMACS_SHA256=ba897946f94c36600a7e7bb3501d27aa4112d791bfe1445c61ed28550daca235
ARG OUTPUT_UID=1000
ARG OUTPUT_GID=1000

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential \
        ca-certificates \
        curl \
        libgnutls28-dev \
        libjansson-dev \
        libncurses-dev \
        libxml2-dev \
        pkg-config \
        texinfo \
        xz-utils \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp
RUN curl --fail --location --silent --show-error \
        --connect-timeout 30 \
        --retry 4 \
        --retry-all-errors \
        --retry-delay 2 \
        --output emacs.tar.xz \
        "https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz" \
    && echo "${EMACS_SHA256}  emacs.tar.xz" | sha256sum --check \
    && tar --extract --file emacs.tar.xz

WORKDIR /tmp/emacs-${EMACS_VERSION}
RUN ./configure \
        --prefix=/installed-agent/emacs \
        --without-x \
        --without-dbus \
        --without-gsettings \
        --without-native-compilation \
        --without-selinux \
        --without-sound \
        --without-tree-sitter \
        --with-gnutls \
        --with-json \
        --with-xml2 \
    && make -j"$(nproc)" \
    && make install \
    && chown -R "${OUTPUT_UID}:${OUTPUT_GID}" /installed-agent/emacs

FROM scratch
COPY --from=build /installed-agent/emacs /
