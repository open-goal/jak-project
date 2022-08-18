FROM archlinux:latest

RUN pacman -Syyu --noconfirm --needed cmake libpulse base-devel nasm python git libx11 libxrandr libxinerama libxcursor libxi

# makepkg user and workdir
ARG user=jak
RUN useradd --system --create-home $user \
  && echo "$user ALL=(ALL:ALL) NOPASSWD:ALL" > /etc/sudoers.d/$user
USER $user
WORKDIR /home/$user

# Install yay
RUN git clone https://aur.archlinux.org/yay.git \
  && cd yay \
  && makepkg -sri --needed --noconfirm \
  && cd \
  # Clean up
  && rm -rf .cache yay

RUN yay --noconfirm -S go-task

RUN mkdir /home/$user/jak-project/
COPY --chown=jak:jak . /home/$user/jak-project
RUN git config --global --add safe.directory /home/jak/jak-project

WORKDIR /home/$user/jak-project

RUN cmake -B build && cmake --build build -j 8