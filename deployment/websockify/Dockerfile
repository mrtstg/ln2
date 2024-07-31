FROM python:3.12-slim
RUN apt-get update
RUN apt-get install gettext-base
WORKDIR /root
ENV PYTHONUNBUFFERED=1
RUN pip install websockify==0.12.0
CMD ["/bin/bash", "/entrypoint.sh"]
