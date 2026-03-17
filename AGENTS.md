# Instructions for AI Agents

## Overview

This repository contains runnable code for the [RabbitMQ tutorials](https://www.rabbitmq.com/tutorials)
in many languages and client libraries. The accompanying prose lives on the website: this repo holds only executable examples.

All tutorials require a RabbitMQ node running on `localhost` with default settings and use
the default `localhost`-constrained seed user `guest` with well known credentials.

Tutorial code here and the prose at [`rabbitmq/rabbitmq-website`](https://github.com/rabbitmq/rabbitmq-website)
should be kept in sync: not necessarily in lockstep, but updated consistently within a few days of each other.


## Repository Structure

Each top-level directory (called a tutorial port) is a self-contained tutorial set for one language (client library),
with its own `README.md` covering setup and run instructions. Some directories use symlinks
to reuse code.

Directories with a `-stream` suffix cover the [RabbitMQ Stream protocol](https://www.rabbitmq.com/docs/streams); the rest cover AMQP 0-9-1.

Individual directories (ports) can have their local `AGENTS.md` with language-specific instructions, e.g.
around how to avoid standard input and output pitfalls when asked to run permutations of these tutorials
(e.g. T1 publishers in Java and a T1 consumer that uses Swift 6 and Bunny.swift).

AMQP 0-9-1 tutorials follow a standard numbering with consistent base filenames across languages:

 1. Tutorial 1 (a.k.a. T1, "Hello World"): `send` / `receive`
 2. Tutorial 2 (a.k.a. T2, "Work Queues"): `new_task` / `worker`
 3. Tutorial 3 (T3, "Publish/Subscribe"): `emit_log` / `receive_logs`
 4. Tutorial 4 (T4, "Routing"): `emit_log_direct` / `receive_logs_direct`
 5. Tutorial 5 (T5, "Topics"): `emit_log_topic` / `receive_logs_topic`
 6. Tutorial 6 (T6, "RPC"): `rpc_client` / `rpc_server`
 7. Tutorial 7 (T7, "Publisher Confirms"): `publisher_confirms`

Stream tutorials cover `send` / `receive` and `offset_tracking_send` / `offset_tracking_receive`.


## Adding or Modifying Tutorials

 * Read the language directory's README first
 * Follow the existing code style and naming conventions of the directory you are editing
 * Tutorials are learning material, not production code: keep them simple, readable, and self-contained
 * Keep dependencies minimal; use the idiomatic RabbitMQ client library for the language


## Style

 * Only add comments where the logic isn't self-evident; keep them concise
 * Use proper English grammar and punctuation
 * Never add full stops to Markdown list items


## Git

 * Never add yourself to the list of commit co-authors
 * Never mention yourself in commit messages in any way
