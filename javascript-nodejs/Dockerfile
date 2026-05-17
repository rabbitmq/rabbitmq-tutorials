FROM node:20-alpine

WORKDIR /app

COPY package*.json ./
RUN npm ci --omit=dev

COPY src ./src

# Overridden per service in docker-compose.yml
CMD ["node", "src/send.js"]
