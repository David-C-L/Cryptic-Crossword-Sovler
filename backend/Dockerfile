FROM python:3 
COPY . /app
WORKDIR /app
RUN apt-get update
RUN apt-get install -y tesseract-ocr gunicorn
RUN apt-get install -y python3-opencv python3-dotenv python3-gevent
RUN pip install --no-cache-dir -r requirements.txt
WORKDIR ./Server
ENV PORT=5000
ENV PRODUCTION=1
CMD gunicorn app:app --worker-class gevent --bind 0.0.0.0:${PORT}

