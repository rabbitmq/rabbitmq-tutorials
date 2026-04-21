#!/usr/bin/env bash
# End-to-end smoke tests for dotnet-amqp tutorials.
# Requires: bash, dotnet 8+, RabbitMQ 4.x on localhost:5672 (guest/guest).
# Does not start RabbitMQ; ensure the broker is already running.
#
# Usage:
#   ./dotnet-amqp/test-tutorials.sh
#   cd dotnet-amqp && ./test-tutorials.sh
#
# Environment:
#   SKIP_BROKER_CHECK=1     Skip TCP check on port 5672 (tests still need a broker)
#   SKIP_MGMT_DELETE=1      Do not DELETE tutorial queues via management HTTP API before T1/T2
#
# If tutorial queues (hello, task_queue) already have consumers from other processes, T1/T2 can
# fail until those queues are deleted or the other consumers stop. By default this script tries
# DELETE http://localhost:15672/api/queues/%2F/<name> (requires the management plugin).

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

require_dotnet() {
	command -v dotnet >/dev/null 2>&1 || {
		echo "dotnet is not on PATH" >&2
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

build_once() {
	dotnet build "$ROOT/dotnet-amqp.sln" -c Release -v minimal >/dev/null
}

# Wait until log file contains a line (consumer ready, etc.). Handles slow `dotnet run` cold start.
wait_for_log() {
	local file=$1
	local needle=$2
	local max_attempts=${3:-120}
	local i=0
	while [[ $i -lt $max_attempts ]]; do
		if [[ -f "$file" ]] && grep -qF -- "$needle" "$file" 2>/dev/null; then
			return 0
		fi
		sleep 0.25
		i=$((i + 1))
	done
	return 1
}

# Remove a queue so stale consumers from other tutorial runs cannot steal messages (needs management plugin).
delete_queue_mgmt() {
	local name=$1
	if [[ "${SKIP_MGMT_DELETE:-}" == "1" ]]; then
		return 0
	fi
	if ! command -v curl >/dev/null 2>&1; then
		return 0
	fi
	curl -sf -o /dev/null -u guest:guest -X DELETE "http://localhost:15672/api/queues/%2F/${name}" 2>/dev/null || true
}

# dotnet run --project Dir/Dir.csproj -- args...
# Use line-buffered stdout when not attached to a TTY so logs capture "Received..." lines before kill.
runproj() {
	local proj=$1
	shift
	if command -v stdbuf >/dev/null 2>&1; then
		stdbuf -oL -eL dotnet run --no-build -c Release -f net8.0 --project "$ROOT/$proj/$proj.csproj" -- "$@"
	else
		dotnet run --no-build -c Release -f net8.0 --project "$ROOT/$proj/$proj.csproj" -- "$@"
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
	delete_queue_mgmt hello
	runproj Receive >"$recv_log" 2>&1 &
	local rpid=$!
	wait_for_log "$recv_log" "Waiting for messages" || true
	sleep 1
	runproj Send >"$log" 2>&1
	sleep 3
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
	delete_queue_mgmt task_queue
	runproj Worker >"$recv_log" 2>&1 &
	local rpid=$!
	wait_for_log "$recv_log" "Waiting for messages" || true
	sleep 1
	runproj NewTask "e2e.task.test" >"$log" 2>&1
	sleep 4
	kill "$rpid" 2>/dev/null || true
	wait "$rpid" 2>/dev/null || true
	assert_file_contains "$log" "Sent" "T2 NewTask publishes"
	assert_file_contains "$recv_log" "e2e.task.test" "T2 Worker receives task"
	rm -f "$log" "$recv_log"
}

test_t3_pubsub() {
	local log recv_log
	log=$(mktemp)
	recv_log=$(mktemp)
	runproj ReceiveLogs >"$recv_log" 2>&1 &
	local rpid=$!
	wait_for_log "$recv_log" "Waiting for messages" || true
	sleep 1
	runproj EmitLog "e2e" "pubsub" "hello" >"$log" 2>&1
	sleep 3
	kill "$rpid" 2>/dev/null || true
	wait "$rpid" 2>/dev/null || true
	assert_file_contains "$log" "Sent" "T3 EmitLog publishes"
	assert_file_contains "$recv_log" "e2e pubsub hello" "T3 ReceiveLogs receives broadcast"
	rm -f "$log" "$recv_log"
}

test_t4_routing() {
	local log recv_log
	log=$(mktemp)
	recv_log=$(mktemp)
	runproj ReceiveLogsDirect warn >"$recv_log" 2>&1 &
	local rpid=$!
	wait_for_log "$recv_log" "Waiting for messages" || true
	sleep 1
	runproj EmitLogDirect warn "e2e routing" >"$log" 2>&1
	sleep 3
	kill "$rpid" 2>/dev/null || true
	wait "$rpid" 2>/dev/null || true
	assert_file_contains "$log" "Sent" "T4 EmitLogDirect publishes"
	assert_file_contains "$recv_log" "e2e routing" "T4 ReceiveLogsDirect gets routed message"
	rm -f "$log" "$recv_log"
}

test_t5_topics() {
	local log recv_log
	log=$(mktemp)
	recv_log=$(mktemp)
	runproj ReceiveLogsTopic "kern.*" >"$recv_log" 2>&1 &
	local rpid=$!
	wait_for_log "$recv_log" "Waiting for messages" || true
	sleep 1
	runproj EmitLogTopic kern.info "e2e topic" >"$log" 2>&1
	sleep 3
	kill "$rpid" 2>/dev/null || true
	wait "$rpid" 2>/dev/null || true
	assert_file_contains "$log" "Sent" "T5 EmitLogTopic publishes"
	assert_file_contains "$recv_log" "e2e topic" "T5 ReceiveLogsTopic matches binding"
	rm -f "$log" "$recv_log"
}

test_t6_rpc() {
	local log srv_log
	log=$(mktemp)
	srv_log=$(mktemp)
	runproj RPCServer >"$srv_log" 2>&1 &
	local spid=$!
	sleep 3
	if ! runproj RPCClient >"$log" 2>&1; then
		kill "$spid" 2>/dev/null || true
		wait "$spid" 2>/dev/null || true
		fail "T6 RPCClient process failed"
		rm -f "$log" "$srv_log"
		return
	fi
	kill "$spid" 2>/dev/null || true
	wait "$spid" 2>/dev/null || true
	assert_file_contains "$log" "Requesting fib(10)" "T6 RPCClient sends request"
	assert_file_contains "$log" "Got '55'" "T6 RPCClient gets fib(10)==55"
	rm -f "$log" "$srv_log"
}

test_publisher_confirms() {
	local log
	log=$(mktemp)
	run_timed "$log" 6 runproj PublisherConfirms
	assert_file_contains "$log" "Accepted Message" "PublisherConfirms sees accepted publish"
	assert_file_contains "$log" "Received a message" "PublisherConfirms consumer sees message"
	rm -f "$log"
}

test_rpc_amqp10() {
	local log
	log=$(mktemp)
	run_timed "$log" 8 runproj RpcAmqp10
	assert_file_contains "$log" "pong" "RpcAmqp10 ping/pong (Direct Reply-To)"
	rm -f "$log"
}

main() {
	require_dotnet
	require_broker
	echo "Building dotnet-amqp (Release)..."
	build_once
	echo "Running dotnet-amqp tutorial smoke tests from $ROOT"
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
