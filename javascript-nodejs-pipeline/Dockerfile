FROM node:20-alpine

WORKDIR /app

COPY package*.json ./
RUN npm ci --omit=dev

COPY producer.js worker.js consumer.js ./

# Overridden per service in docker-compose.yml
CMD ["node", "worker.js"]
