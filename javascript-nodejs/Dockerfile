FROM node:8.2.1-alpine

RUN npm install -g amqplib

COPY src /home/rabbit/src
COPY package.json /home/rabbit/package.json
COPY entrypoint /home/rabbit/entrypoint

RUN adduser -S rabbit \
 && chown -R rabbit. /home/rabbit \
 && chmod 700 /home/rabbit/src \
 && chmod 700 /home/rabbit
USER rabbit
WORKDIR /home/rabbit

RUN npm install

ENTRYPOINT ["/home/rabbit/entrypoint"]
CMD ["send"]
