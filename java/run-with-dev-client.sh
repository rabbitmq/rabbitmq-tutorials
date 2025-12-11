#!/bin/bash
# Run a Java tutorial example with the development version of rabbitmq-java-client

set -o errexit
set -o nounset
set -o pipefail

if (( $# == 0 ))
then
    echo "Usage: $0 <ClassName> [args...]" >&2
    echo "Example: $0 PublisherConfirmsAsync" >&2
    exit 1
fi

declare -r class_name="$1"
shift

declare -r client_dir="../../rabbitmq-java-client"
declare -r client_jar="$client_dir"/target/amqp-client-*-SNAPSHOT.jar

if [[ ! -f "$client_jar" ]]
then
    echo "Building rabbitmq-java-client..." >&2
    (cd "$client_dir" && ./mvnw clean package -DskipTests)
fi

echo "Getting compile classpath..." >&2

compile_cp=$(cd "$client_dir" && ./mvnw dependency:build-classpath -DincludeScope=compile 2>&1 | grep -E "^/.*\.jar" | head -1)
readonly compile_cp

declare -r full_cp=".:$client_jar:$compile_cp"

echo "Compiling ${class_name}.java..." >&2
javac -cp "$full_cp" "${class_name}.java"

echo "Running $class_name..." >&2
java -cp "$full_cp" "$class_name" "$@"
