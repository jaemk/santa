FROM clfoundation/sbcl:2.1.5-slim-buster

RUN apt-get update --yes && apt-get install --yes \
    git \
    build-essential \
    curl

WORKDIR /app

# build dependencies
COPY ./deps.lisp ./deps.lisp
COPY ./Makefile ./Makefile
COPY ./santa.asd ./santa.asd
RUN make qlmanifest || (cat build/build.log && exit 1)
RUN make buildapp || (cat build/build.log && exit 1)

COPY ./src ./src

# copy over git dir and embed latest commit hash
COPY ./.git ./.git
# make sure there's no trailing newline
RUN git rev-parse HEAD | cut -c1-7 > commit_hash.txt
RUN rm -rf ./.git

RUN make || (cat build/build.log && exit 1)
RUN mkdir -p bin
RUN mv build/bin/santa bin/santa && rm -rf build/

COPY ./static ./static

CMD [ "bin/santa" ]

