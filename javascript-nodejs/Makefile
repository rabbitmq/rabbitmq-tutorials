all: build clean targets

targets: send new_task emit_log emit_log_direct emit_log_topic rpc_client

TAG:=rabbitmq/js-test
NAME:=JStest
AMQ_HOST:=rabbit

receive: clean receiveCID receiveLogs

build:
	docker build -t ${TAG} .

rabbitCID:
	@echo Rabbit
	@docker run -d \
		--name=${NAME}-rabbit \
		--hostname rabbit \
	  --cidfile="rabbitCID" \
		-t rabbitmq:3
	@echo -n Let Rabbit Settle.
	@sleep 1; echo -n '.'
	@sleep 2; echo -n '.'
	@sleep 3; echo -n '.'
	@sleep 2; echo '!'

send: rabbitCID receiveCID
	@echo Send
	@docker run -d \
		--env AMQ_HOST=${AMQ_HOST} \
		--link ${NAME}-rabbit:rabbit \
	  --cidfile="sendCID" \
		-t ${TAG}
	-@docker logs -f `cat sendCID`
	docker rm `cat sendCID`
	-@rm sendCID
	docker logs `cat receiveCID`

receiveCID: rabbitCID
	@echo Receive
	@docker run -d \
		--name=${NAME}-receive \
		--env AMQ_HOST=${AMQ_HOST} \
		--link ${NAME}-rabbit:rabbit \
	  --cidfile="receiveCID" \
		-t ${TAG} receive

rpc_serverCID: rabbitCID
	@echo RPC Server
	@docker run -d \
		--name=${NAME}-rpc_server \
		--env AMQ_HOST=${AMQ_HOST} \
		--link ${NAME}-rabbit:rabbit \
	  --cidfile="rpc_serverCID" \
		-t ${TAG} rpc_server

rpc_client: rabbitCID rpc_serverCID
	@echo RPC Client
	@docker run -d \
		--env AMQ_HOST=${AMQ_HOST} \
		--link ${NAME}-rabbit:rabbit \
	  --cidfile="rpc_clientCID" \
		-t ${TAG} rpc_client 3
	-@docker logs -f `cat rpc_clientCID`
	docker rm `cat rpc_clientCID`
	-@rm rpc_clientCID
	docker logs `cat rpc_serverCID`

workerCID: rabbitCID
	@echo Worker
	@docker run -d \
	--name=${NAME}-worker \
		--env AMQ_HOST=${AMQ_HOST} \
		--link ${NAME}-rabbit:rabbit \
	  --cidfile="workerCID" \
		-t ${TAG} worker

new_task: rabbitCID workerCID
	@echo New Task
	@docker run -d \
		--env AMQ_HOST=${AMQ_HOST} \
		--link ${NAME}-rabbit:rabbit \
	  --cidfile="new_taskCID" \
		-t ${TAG} new_task
	-@docker logs -f `cat new_taskCID`
	docker rm `cat new_taskCID`
	-@rm new_taskCID
	docker logs `cat workerCID`

emit_log: rabbitCID receive_logsCID
	@echo Emit Log
	@docker run -d \
		--env AMQ_HOST=${AMQ_HOST} \
	  --cidfile="emit_logCID" \
		--link ${NAME}-rabbit:rabbit \
		-t ${TAG} emit_log
	-@docker logs -f `cat emit_logCID`
	docker rm `cat emit_logCID`
	-@rm emit_logCID
	docker logs `cat receive_logsCID`

receive_logsCID: rabbitCID
	@echo Receive Logs
	@docker run -d \
		--name=${NAME}-receive_logs \
		--env AMQ_HOST=${AMQ_HOST} \
		--link ${NAME}-rabbit:rabbit \
	  --cidfile="receive_logsCID" \
		-t ${TAG} receive_logs

emit_log_direct: rabbitCID receive_logs_directCID
	@echo Emit Log_direct
	@docker run -d \
		--env AMQ_HOST=${AMQ_HOST} \
	  --cidfile="emit_log_directCID" \
		--link ${NAME}-rabbit:rabbit \
		-t ${TAG} emit_log_direct
	-@docker logs -f `cat emit_log_directCID`
	docker rm `cat emit_log_directCID`
	-@rm emit_log_directCID
	docker logs `cat receive_logs_directCID`

receive_logs_directCID: rabbitCID
	@echo Receive Logs_direct
	@docker run -d \
		--name=${NAME}-receive_logs_direct \
		--env AMQ_HOST=${AMQ_HOST} \
		--link ${NAME}-rabbit:rabbit \
	  --cidfile="receive_logs_directCID" \
		-t ${TAG} receive_logs_direct info

emit_log_topic: rabbitCID receive_logs_topicCID
	@echo Emit Log_topic
	@docker run -d \
		--env AMQ_HOST=${AMQ_HOST} \
	  --cidfile="emit_log_topicCID" \
		--link ${NAME}-rabbit:rabbit \
		-t ${TAG} emit_log_topic
	-@docker logs -f `cat emit_log_topicCID`
	docker rm `cat emit_log_topicCID`
	-@rm emit_log_topicCID
	docker logs `cat receive_logs_topicCID`

receive_logs_topicCID: rabbitCID
	@echo Receive Logs_topic
	@docker run -d \
		--name=${NAME}-receive_logs_topic \
		--env AMQ_HOST=${AMQ_HOST} \
		--link ${NAME}-rabbit:rabbit \
	  --cidfile="receive_logs_topicCID" \
		-t ${TAG} receive_logs_topic anonymous.info

kill:
	-docker kill `cat receiveCID`
	-docker kill `cat receive_logsCID`
	-docker kill `cat receive_logs_directCID`
	-docker kill `cat receive_logs_topicCID`
	-docker kill `cat rpc_serverCID`
	-docker kill `cat workerCID`

rm-image:
	-docker rm `cat receiveCID`
	-docker rm `cat receive_logsCID`
	-docker rm `cat receive_logs_directCID`
	-docker rm `cat receive_logs_topicCID`
	-docker rm `cat rpc_serverCID`
	-docker rm `cat workerCID`
	-rm receiveCID
	-rm receive_logsCID
	-rm receive_logs_directCID
	-rm receive_logs_topicCID
	-rm rpc_serverCID
	-rm workerCID

rm: kill rm-image

clean: rm

enter:
	docker exec -i -t `cat sendCID` /bin/sh

logs:
	@echo Receive
	-docker logs `cat receiveCID`
	@echo Receive Logs
	-docker logs `cat receive_logsCID`
	@echo Worker
	-docker logs `cat workerCID`
	@echo RPC Server
	-docker logs `cat rpc_serverCID`

receiveLogs:
	docker logs -f `cat receiveCID`

killrabbit:
	-docker kill `cat rabbitCID`

rmrabbit: killrabbit
	-docker rm `cat rabbitCID`
	-@rm rabbitCID
