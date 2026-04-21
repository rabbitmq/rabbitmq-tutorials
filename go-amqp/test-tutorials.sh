#!/usr/bin/env bash
# End-to-end smoke tests for go-amqp tutorials.
# Requires: bash, go, RabbitMQ 4.x on localhost:5672 (guest/guest).
#
# Usage:
#   ./go-amqp/test-tutorials.sh
#   cd go-amqp && ./test-tutorials.sh
#
# Environment:
#   SKIP_BROKER_CHECK=1  Skip TCP check on port 5672 (tests still need a broker).

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT"

PASS=0
FAIL=0

pass() {
	printf 'ok\t%s\n' "$1"
	PASS=$((PASS + 1))
}

fail() {
	printf 'FAIL\t%s\n' "$1" >&2
	FAIL=$((FAIL + 1))
}

require_go() {
	command -v go >/dev/null 2>&1 || {
		echo "go is not on PATH" >&2
		exit 1
	}
}

check_broker() {
	if command -v nc >/dev/null 2>&1; then
		nc -z localhost 5672 2>/dev/null
		return
	fi
	{ echo >/dev/tcp/127.0.0.1/5672; } 2>/dev/null
}

require_broker() {
	if [[ "${SKIP_BROKER_CHECK:-}" == "1" ]]; then
		return 0
	fi
	if ! check_broker; then
		echo "RabbitMQ does not appear to be reachable on localhost:5672" >&2
		echo "Start the broker or set SKIP_BROKER_CHECK=1 to skip this check." >&2
		exit 1
	fi
}

assert_file_contains() {
	local file=$1
	local needle=$2
	local msg=$3
	if [[ -f "$file" ]] && grep -qF -- "$needle" "$file"; then
		pass "$msg"
	else
		fail "$msg (log should contain: $needle)"
		if [[ -f "$file" ]]; then
			echo "--- $file (first 40 lines) ---" >&2
			head -n 40 "$file" >&2
		fi
	fi
}

# Run command in background; sleep $secs; kill. Captures stdout/stderr to $log.
run_timed() {
	local log=$1
	local secs=$2
	shift 2
	: >"$log"
	"$@" >"$log" 2>&1 &
	local pid=$!
	sleep "$secs"
	kill "$pid" 2>/dev/null || true
	wait "$pid" 2>/dev/null || true
}

test_t1_hello() {
	local log recv_log
	log=$(mktemp)
	recv_log=$(mktemp)
	go run receive.go >"$recv_log" 2>&1 &
	local rpid=$!
	sleep 2
	go run send.go >"$log" 2>&1
	sleep 1
	kill "$rpid" 2>/dev/null || true
	wait "$rpid" 2>/dev/null || true
	assert_file_contains "$log" "Sent" "T1 send publishes"
	assert_file_contains "$recv_log" "Hello World" "T1 receive gets message"
	rm -f "$log" "$recv_log"
}

test_t2_work_queues() {
	local log recv_log
	log=$(mktemp)
	recv_log=$(mktemp)
	go run worker.go >"$recv_log" 2>&1 &
	local rpid=$!
	sleep 2
	go run new_task.go e2e.task.test >"$log" 2>&1
	sleep 3
	kill "$rpid" 2>/dev/null || true
	wait "$rpid" 2>/dev/null || true
	assert_file_contains "$log" "Sent" "T2 new_task publishes"
	assert_file_contains "$recv_log" "e2e.task.test" "T2 worker receives task"
	rm -f "$log" "$recv_log"
}

test_t3_pubsub() {
	local log recv_log
	log=$(mktemp)
	recv_log=$(mktemp)
	go run receive_logs.go >"$recv_log" 2>&1 &
	local rpid=$!
	sleep 2
	go run emit_log.go e2e pubsub hello >"$log" 2>&1
	sleep 2
	kill "$rpid" 2>/dev/null || true
	wait "$rpid" 2>/dev/null || true
	assert_file_contains "$log" "Sent" "T3 emit_log publishes"
	assert_file_contains "$recv_log" "e2e pubsub hello" "T3 receive_logs receives broadcast"
	rm -f "$log" "$recv_log"
}

test_t4_routing() {
	local log recv_log
	log=$(mktemp)
	recv_log=$(mktemp)
	go run receive_logs_direct.go warn >"$recv_log" 2>&1 &
	local rpid=$!
	sleep 2
	go run emit_log_direct.go warn "e2e routing" >"$log" 2>&1
	sleep 2
	kill "$rpid" 2>/dev/null || true
	wait "$rpid" 2>/dev/null || true
	assert_file_contains "$log" "Sent" "T4 emit_log_direct publishes"
	assert_file_contains "$recv_log" "e2e routing" "T4 receive_logs_direct gets routed message"
	rm -f "$log" "$recv_log"
}

test_t5_topics() {
	local log recv_log
	log=$(mktemp)
	recv_log=$(mktemp)
	go run receive_logs_topic.go "kern.*" >"$recv_log" 2>&1 &
	local rpid=$!
	sleep 2
	go run emit_log_topic.go kern.info "e2e topic" >"$log" 2>&1
	sleep 2
	kill "$rpid" 2>/dev/null || true
	wait "$rpid" 2>/dev/null || true
	assert_file_contains "$log" "Sent" "T5 emit_log_topic publishes"
	assert_file_contains "$recv_log" "e2e topic" "T5 receive_logs_topic matches binding"
	rm -f "$log" "$recv_log"
}

test_t6_rpc() {
	local log srv_log
	log=$(mktemp)
	srv_log=$(mktemp)
	go run rpc_server.go >"$srv_log" 2>&1 &
	local spid=$!
	sleep 2
	if ! go run rpc_client.go 10 >"$log" 2>&1; then
		kill "$spid" 2>/dev/null || true
		wait "$spid" 2>/dev/null || true
		fail "T6 rpc_client process failed"
		rm -f "$log" "$srv_log"
		return
	fi
	kill "$spid" 2>/dev/null || true
	wait "$spid" 2>/dev/null || true
	assert_file_contains "$log" "Requesting fib(10)" "T6 rpc_client sends request"
	assert_file_contains "$log" "Got 55" "T6 rpc_client gets fib(10)==55"
	rm -f "$log" "$srv_log"
}

test_publisher_confirms() {
	local log
	log=$(mktemp)
	run_timed "$log" 4 go run publisher_confirms.go
	assert_file_contains "$log" "Confirmed" "publisher_confirms sees accepted publish"
	assert_file_contains "$log" "Received a message" "publisher_confirms consumer sees message"
	rm -f "$log"
}

test_rpc_amqp10() {
	local log
	log=$(mktemp)
	run_timed "$log" 5 go run rpc_amqp10.go
	assert_file_contains "$log" "pong" "rpc_amqp10 ping/pong (Direct Reply-To)"
	rm -f "$log"
}

main() {
	require_go
	require_broker
	echo "Running go-amqp tutorial smoke tests from $ROOT"
	test_t1_hello
	test_t2_work_queues
	test_t3_pubsub
	test_t4_routing
	test_t5_topics
	test_t6_rpc
	test_publisher_confirms
	test_rpc_amqp10
	echo "----"
	echo "Passed: $PASS  Failed: $FAIL"
	if [[ "$FAIL" -gt 0 ]]; then
		exit 1
	fi
}

main "$@"
