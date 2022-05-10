FROM ubuntu:18.04

ARG QUICKLISP_URL=http://beta.quicklisp.org/dist/quicklisp/2020-12-20/distinfo.txt

# install required debian packages
RUN apt-get update -y && \
    apt-get install -y \
    build-essential \
    curl \
    cmake \
    git \
    rlwrap \
    sbcl \
    graphviz \
    locales \
 && locale-gen en_US.UTF-8 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*


# Add LANG default to en_US.UTF-8
ENV LANGUAGE en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8


# install quicklisp (requirements: curl, sbcl)
RUN curl -o /tmp/quicklisp.lisp 'https://beta.quicklisp.org/quicklisp.lisp' && \
    sbcl --noinform --non-interactive --load /tmp/quicklisp.lisp --eval \
        "(quicklisp-quickstart:install :dist-url \"${QUICKLISP_URL}\")" && \
    sbcl --noinform --non-interactive --load ~/quicklisp/setup.lisp --eval \
        '(ql-util:without-prompting (ql:add-to-init-file))' && \
    echo '#+quicklisp(push "/src" ql:*local-project-directories*)' >> ~/.sbclrc && \
    # mkdir /src && cd /src && git clone https://github.com/nightfly19/cl-arrows.git && \
    rm -f /tmp/quicklisp.lisp


# quickload libraries
ADD . /src/papergraph
WORKDIR /app
RUN sbcl --eval "(ql:quickload :papergraph)" --quit && \
    sbcl --load "/src/papergraph/app/build.lisp" && mv papergraph /bin/ && \
    cp /src/papergraph/docker/papergraph.sh /bin/ && chmod +x /bin/papergraph.sh

ENTRYPOINT ["/bin/bash", "papergraph.sh"]
CMD ["-r"]
