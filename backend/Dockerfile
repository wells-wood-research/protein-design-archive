FROM python:3.9-slim
WORKDIR /app
COPY ./requirements.txt /app/requirements.txt
RUN pip install --no-cache-dir --upgrade -r /app/requirements.txt
COPY ./ /app
RUN pip install --no-cache-dir .
# gthread worker: meinheld (the previous base image) spins at 100% CPU and
# never finishes sending large responses such as /all-designs.
CMD gunicorn -k gthread -w ${GUNICORN_WORKERS:-4} --threads 4 -b 0.0.0.0:80 \
    --access-logfile - backend.app:app
