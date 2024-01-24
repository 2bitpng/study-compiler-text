FROM --platform=linux/x86-64 debian:buster

RUN apt-get -y update
RUN apt-get -y install wget

RUN wget -P /usr/share/keyrings https://github.com/smlsharp/repos/raw/main/debian/dists/buster/smlsharp-archive-keyring.gpg
RUN wget -P /etc/apt/sources.list.d https://github.com/smlsharp/repos/raw/main/debian/dists/buster/smlsharp.list
RUN apt-get -y update
RUN apt-get -y install git
RUN apt-get -y install make
RUN apt-get -y install smlsharp
