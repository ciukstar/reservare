FROM ubuntu:22.04
RUN mkdir -p /opt/reservare \
	&& apt-get update \
	&& apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
	&& apt-get install -y ca-certificates && update-ca-certificates \
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/*

ARG YESOD_SUPERUSER_USERNAME
ARG YESOD_SUPERUSER_PASSWORD
ARG YESOD_GOOGLE_CLIENT_ID
ARG YESOD_GOOGLE_CLIENT_SECRET
ARG YESOD_GCLOUD_PROJECT_ID
ARG YESOD_DEMO_LANG=EN

WORKDIR /opt/reservare
COPY reservare /opt/reservare
COPY static /opt/reservare/static
COPY config /opt/reservare/config

ENV YESOD_PORT=8080
ENV YESOD_SUPERUSER_USERNAME=${YESOD_SUPERUSER_USERNAME}
ENV YESOD_SUPERUSER_PASSWORD=${YESOD_SUPERUSER_PASSWORD}
ENV YESOD_GOOGLE_CLIENT_ID=${YESOD_GOOGLE_CLIENT_ID}
ENV YESOD_GOOGLE_CLIENT_SECRET=${YESOD_GOOGLE_CLIENT_SECRET}
ENV YESOD_GCLOUD_PROJECT_ID=${YESOD_GCLOUD_PROJECT_ID}
ENV YESOD_DEMO_LANG=${YESOD_DEMO_LANG}

EXPOSE 8080
CMD ["./reservare"]
