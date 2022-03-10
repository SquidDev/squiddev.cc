# syntax=docker/dockerfile:1.4
FROM docker.io/debian:11 as builder
RUN apt-get update -qq \
 && apt-get install locales graphviz npm haskell-stack libz-dev -qqy \
 && apt-get clean \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen \
 && mkdir /src \

ENV LC_ALL="en_US.UTF-8" LANG="en_US.UTF-8"
WORKDIR /src

COPY *.cabal stack.yaml stack.yaml.lock /src/
RUN stack install --only-dependencies --flag hakyll:-previewServer --flag hakyll:-watchServer -j4

COPY package.json package-lock.json /src/
RUN npm ci

COPY . /src/
RUN --mount=type=cache,id=squiddev_cc_stack_work,target=/src/.stack_work \
    stack run -- build && stack run -- check

FROM scratch as artifacts
COPY --from=builder /src/_site/ /
