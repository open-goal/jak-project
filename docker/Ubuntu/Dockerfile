FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update
RUN apt-get install -y gcc make cmake build-essential g++ nasm clang-format libxrandr-dev libxinerama-dev libxcursor-dev libpulse-dev libxi-dev python lld clang curl

RUN sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d -b /usr/local/bin

ARG user=jak

RUN useradd -m -d /home/${user} ${user}

USER $user

WORKDIR /home/$user

RUN mkdir /home/$user/jak-project/
COPY --chown=jak:jak . /home/$user/jak-project

WORKDIR /home/$user/jak-project/build

RUN cmake -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=lld" -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=lld" -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ ..
WORKDIR /home/$user/jak-project
RUN cmake -B build && cmake --build build -j 8 
