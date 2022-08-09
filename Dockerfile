FROM ubuntu AS builder

RUN apt-get update && apt-get install -y --no-install-recommends \
	autoconf \
	camlp4 \
	g++ \
	gcc \
	libbz2-dev \
	libc6-dev \
	libgd-gd2-noxpm-ocaml-dev \
	libmagic-dev \
	libminiupnpc-dev \
	libnatpmp-dev \
	libnum-ocaml-dev \
	make \
	ocaml-nox \
	zlib1g-dev

WORKDIR /app
COPY . /app

RUN ./configure \
	--disable-directconnect \
	--disable-fasttrack \
	--disable-gnutella \
	--disable-gnutella2 \
	--enable-batch \
	--enable-upnp-natpmp \
	&& make

FROM ubuntu

RUN adduser --disabled-password --gecos "" mlnet
RUN apt-get update && apt-get install -y --no-install-recommends \
	ca-certificates \
	libgd3 \
	libmagic1 \
	libminiupnpc17 \
	libnatpmp1 \
	&& rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/mlnet /usr/bin/

USER mlnet
ENTRYPOINT ["/usr/bin/mlnet"]
