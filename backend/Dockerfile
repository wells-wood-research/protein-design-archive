FROM tiangolo/meinheld-gunicorn-flask:python3.9
COPY ./requirements.txt /app/requirements.txt
RUN pip install --no-cache-dir --upgrade -r /app/requirements.txt
COPY ./ /app
RUN pip install .
COPY ./config/pda.conf /etc/nginx/conf.d/default.conf
