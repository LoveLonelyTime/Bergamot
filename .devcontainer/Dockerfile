FROM ubuntu:latest

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
RUN apt-get install -y verilator

# Gtkwave
RUN apt-get install -y gtkwave

# DTC
RUN apt-get install -y device-tree-compiler

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

# Install riscv-toolchain
COPY riscv-toolchain/ /opt/riscv-toolchain/

RUN chmod -R +x /opt/riscv-toolchain/bin

ENV PATH="/opt/riscv-toolchain/bin:${PATH}"

# Check
RUN scala -version \
    && scalac -version \
    && mill --version \
    && verilator --version \
    && dtc --version \
    && java -version \
    && gcc --version \
    && make --version \
    && riscv32-unknown-linux-gnu-gcc --version

# Setup workplace
WORKDIR /workspace

# Keep alive
CMD ["sleep", "infinity"]