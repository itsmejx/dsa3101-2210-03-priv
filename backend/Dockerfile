FROM ubuntu:bionic

COPY requirements.txt requirements.txt
COPY scripts scripts

RUN mkdir DB
RUN apt -y update
RUN apt -y upgrade
RUN apt install -y python3
RUN apt install -y python3-pip
RUN pip3 install --upgrade pip
RUN apt install -y python3-venv
RUN mkdir -p /python_env
RUN python3 -m venv /python_env
RUN . /python_env/bin/activate
RUN pip3 install -r requirements.txt
RUN apt install -y git
RUN apt install -y protobuf-compiler python3-lxml
RUN mkdir -p /tensorflow/models
RUN git clone https://github.com/tensorflow/models.git /tensorflow/models

ENV FLASK_APP=api.py
EXPOSE 5000

WORKDIR /tensorflow/models/research

RUN protoc object_detection/protos/*.proto --python_out=.
RUN export PYTHONPATH=$PYTHONPATH:`pwd`:`pwd`/slim
RUN cp object_detection/packages/tf2/setup.py .
RUN python3 -m pip install --use-feature=2020-resolver .

WORKDIR /scripts

CMD ["flask", "run"]
#ENTRYPOINT ["python3", "-u", "-m", "flask", "run", "--host=0.0.0.0"]