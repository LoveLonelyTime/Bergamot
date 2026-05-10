FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update

# C/C++
RUN apt-get install -y \
    build-essential \
    g++ \
    gcc \
    make \
    cmake \
    git \
    curl \
    wget \
    xz-utils \
    ca-certificates \
    vim

# OpenJDK 17
RUN apt-get install -y openjdk-17-jdk

# Verilator
RUN apt-get install -y \
    help2man \
    perl \
    python3 \
    autoconf \
    flex \
    bison \
    ccache \
    libgoogle-perftools-dev \
    libjemalloc-dev \
    numactl \
    perl-doc

RUN (apt-get install -y libfl2 || echo "libfl2 not available, skipping") && \
    (apt-get install -y libfl-dev || echo "libfl-dev not available, skipping") && \
    (apt-get install -y zlibc zlib1g zlib1g-dev || echo "zlib packages not available, skipping")

# Gtkwave
RUN apt-get install -y gtkwave

# DTC
RUN apt-get install -y device-tree-compiler

# Others
RUN apt-get install -y \
    pkg-config \
    libtool

RUN rm -rf /var/lib/apt/lists/*

# Install Scala 2.13.18
RUN curl -fL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz | gzip -d > cs \
    && chmod +x cs \
    && ./cs install scala:2.13.18 scalac:2.13.18 \
    && rm cs

ENV PATH="/root/.local/share/coursier/bin:${PATH}"

# Install Mill
RUN curl -L https://repo1.maven.org/maven2/com/lihaoyi/mill-dist/1.1.5/mill-dist-1.1.5-mill.sh -o mill \
    && chmod +x mill \
    && mv mill /usr/local/bin/mill

# Install Verilator
RUN git clone https://github.com/verilator/verilator /tmp/verilator \
    && cd /tmp/verilator \
    && git checkout stable \
    && autoconf \
    && ./configure \
    && make -j $(nproc) \
    && make install \
    && rm -rf /tmp/verilator

# Install riscv-gnu-toolchain
RUN wget -O /tmp/riscv32-toolchain.tar.xz \
    https://toolchains.bootlin.com/downloads/releases/toolchains/riscv32-ilp32d/tarballs/riscv32-ilp32d--glibc--stable-2025.08-1.tar.xz \
    && mkdir -p /opt/riscv32-ilp32d \
    && tar -xJf /tmp/riscv32-toolchain.tar.xz \
    -C /opt/riscv32-ilp32d \
    --strip-components=1 \
    && rm /tmp/riscv32-toolchain.tar.xz

ENV PATH="/opt/riscv32-ilp32d/bin:${PATH}"

# Check
RUN scala -version \
    && scalac -version \
    && mill --version \
    && verilator --version \
    && dtc --version \
    && java -version \
    && gcc --version \
    && make --version \
    && riscv32-buildroot-linux-gnu-gcc --version

# Setup workplace
WORKDIR /workspace

# Keep alive
CMD ["sleep", "infinity"]