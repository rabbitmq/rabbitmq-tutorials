#!/usr/bin/env bash

set -o errexit
set -o pipefail
# set -o xtrace

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
readonly script_dir
echo "[INFO] script_dir: '$script_dir'"

readonly docker_name_prefix='java-stream-mvn'
readonly rmq_docker_name="$docker_name_prefix-rabbitmq"
readonly rmq_image=${RABBITMQ_IMAGE:-rabbitmq:3-management}
readonly rmq_config_dir="$script_dir/rmq-conf"

if [[ $1 == 'pull' ]]
then
    readonly docker_pull_args='--pull always'
else
    readonly docker_pull_args=''
fi

set -o nounset

function config_rabbitmq
{
    mkdir -p "$rmq_config_dir"
    echo '[rabbitmq_top,rabbitmq_management,rabbitmq_stream,rabbitmq_stream_management,rabbitmq_amqp1_0].' > "$rmq_config_dir/enabled_plugins"
    echo 'loopback_users = none' > "$rmq_config_dir/rabbitmq.conf"
}

function start_rabbitmq
{
    echo "[INFO] starting RabbitMQ server docker container"
    docker rm --force "$rmq_docker_name" 2>/dev/null || echo "[INFO] $rmq_docker_name was not running"
    # shellcheck disable=SC2086
    docker run --detach $docker_pull_args \
        --name "$rmq_docker_name" \
        --env 'RABBITMQ_SERVER_ADDITIONAL_ERL_ARGS=-rabbitmq_stream advertised_host localhost' \
        --network host \
        --volume "$rmq_config_dir:/etc/rabbitmq:ro" \
        "$rmq_image"
}

function wait_rabbitmq
{
    set +o errexit

    declare -i count=12
    while (( count > 0 )) && [[ "$(docker inspect --format='{{.State.Running}}' "$rmq_docker_name")" != 'true' ]]
    do
        echo '[WARNING] RabbitMQ container is not yet running...'
        sleep 5
        (( count-- ))
    done

    declare -i count=12
    while (( count > 0 )) && ! docker exec "$rmq_docker_name" epmd -names | grep -F 'name rabbit'
    do
        echo '[WARNING] epmd is not reporting rabbit name just yet...'
        sleep 5
        (( count-- ))
    done

    docker exec "$rmq_docker_name" rabbitmqctl await_startup

    set -o errexit
}

config_rabbitmq

start_rabbitmq

wait_rabbitmq

docker exec "$rmq_docker_name" rabbitmq-diagnostics erlang_version
docker exec "$rmq_docker_name" rabbitmqctl version
