FROM composer:2.8 AS composer
FROM mlocati/php-extension-installer:2.7 AS php-extension-installer
FROM php:8.3-cli-bookworm AS php-dev

COPY --from=composer /usr/bin/composer /usr/bin/
COPY --from=php-extension-installer /usr/bin/install-php-extensions /usr/bin/

ARG UID=10001
ARG GID=10001

RUN <<EOF
    set -e
    groupmod --gid=${GID} www-data
    usermod --uid=${UID} --gid=${GID} www-data
    apt-get update
    apt-get install --no-install-recommends --no-install-suggests -q -y unzip
EOF

RUN <<EOF
    set -e
    install-php-extensions opcache pcntl sockets pgsql bcmath intl uv
    apt-get remove -q -y ${PHPIZE_DEPS} ${BUILD_DEPENDS}
EOF

RUN <<EOF
    set -e
    mkdir /var/.composer
    chown www-data:www-data /var/.composer
EOF

ENV COMPOSER_CACHE_DIR=/var/.composer

USER www-data

WORKDIR /app
