FROM haskell:8.10.2
RUN apt-get update -y && apt-get install libv4l-dev -y 
RUN apt-get install libavdevice-dev -y
COPY . /code
WORKDIR /code
CMD ["bash"]


