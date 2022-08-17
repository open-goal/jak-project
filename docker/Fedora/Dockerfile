FROM fedora:latest

RUN dnf install -y curl cmake python lld clang nasm libX11-devel libXrandr-devel libXinerama-devel libXcursor-devel libXi-devel pulseaudio-libs-devel
RUN sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d -b /usr/local/bin

ARG user=jak

RUN useradd -m -d /home/${user} ${user}

USER $user

WORKDIR /home/$user

RUN mkdir /home/$user/jak-project/
COPY --chown=jak:jak . /home/$user/jak-project

WORKDIR /home/$user/jak-project

RUN cmake -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=lld" -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=lld" -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -B build
RUN cmake --build build -j$(nproc)