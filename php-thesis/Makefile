up:
	docker compose up -d

down:
	docker compose down

php:
	docker compose exec php bash

run-send:
	docker compose exec php php hello-world/send.php

run-receive:
	docker compose exec php php hello-world/receive.php

run-new-task:
	docker compose exec php php work-queue/new_task.php "A very hard task which takes two seconds.."

run-worker:
	docker compose exec php php work-queue/worker.php

run-emit-log:
	docker compose exec php php pub-sub/emit_log.php

run-receive-logs:
	docker compose exec php php pub-sub/receive_logs.php

run-emit-log-direct:
	docker compose exec php php routing/emit_log_direct.php warning "Something went wrong"

run-receive-logs-direct:
	docker compose exec php php routing/receive_logs_direct.php info warning error

run-emit-log-topic:
	docker compose exec php php topics/emit_log_topic.php "kern.critical" "A critical kernel error"

run-receive-logs-topic:
	docker compose exec php php topics/receive_logs_topic.php "#"

run-rpc-server:
	docker compose exec php php rpc/rpc_server.php

run-rpc-client:
	docker compose exec php php rpc/rpc_client.php

run-confirms-message:
	docker compose exec php php publisher-confirms/message.php

run-confirms-batch:
	docker compose exec php php publisher-confirms/batch.php
