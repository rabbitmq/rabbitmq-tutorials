#!/usr/bin/env python
import time
import random
import re
import subprocess
import signal
import sys
import os

def run(cmd, verbose=False, **kwargs):
    if verbose:
        print " [s] Running %r" % (cmd,)
    p = subprocess.Popen(cmd.split(),
                         stdout=subprocess.PIPE,
                         **kwargs)
    p.wait()

    if verbose:
        for line in p.stdout:
            line = line.strip()
            if line:
                print ' [s]  %s' % (line,)
        print " [s] Done"
    time.sleep(0.1)
    return p.returncode


def spawn(cmd, verbose=False, **kwargs):
    if verbose:
        print " [r] Waiting for %r" % (cmd,)
    p = subprocess.Popen(cmd.split(),
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE,
                         **kwargs)
    time.sleep(0.4)
    return p

def wait(p, match, verbose=False):
    os.kill(p.pid, signal.SIGINT)
    p.wait()
    r = False
    for line in p.stdout:
        line = line.strip()
        if re.search(match, line):
            r = True
        if verbose:
            print " [r]  %s" % (line,)
    if verbose:
        print " [r] Done"
    return r



def gen(prog, arg="", **kwargs):
    Prog = ''.join([w.capitalize() for w in prog.split('_')])
    ctx = {
        'prog': prog,
        'Prog': Prog,
        'rubyver': os.environ.get('RUBYVER', '1.8'),
        'arg': arg,
        'java': kwargs.get('java', Prog),
        'dotnet': kwargs.get('dotnet', Prog),
        }
    return [
        ('python', './venv/bin/python %(prog)s.py %(arg)s' % ctx),
        ('erlang', './%(prog)s.erl %(arg)s' % ctx),
        ('java', 'java -cp .:commons-io-1.2.jar:commons-cli-1.1.jar:'
             'rabbitmq-client.jar %(java)s %(arg)s' % ctx),
        ('dotnet', 'env MONO_PATH=lib/bin mono %(dotnet)s.exe %(arg)s' % ctx),
        ('ruby', 'env RUBYOPT=-rubygems GEM_HOME=gems/gems RUBYLIB=gems/lib '
             'ruby%(rubyver)s %(prog)s.rb %(arg)s' % ctx),
        ('php', 'php %(prog)s.php %(arg)s' % ctx),
        ('python-puka', './venv/bin/python %(prog)s.py %(arg)s' % ctx),
        ]

def skip(cwd_cmd, to_skip):
    return [(cwd,cmd) for cwd, cmd in cwd_cmd if cwd not in to_skip]

tests = {
    'tut1': (gen('send'), gen('receive', java='Recv'), 'Hello World!'),
    'tut2': (gen('new_task', arg='%(arg)s'), gen('worker'), '%(arg)s'),
    'tut3': (gen('emit_log', arg='%(arg)s'), gen('receive_logs'), '%(arg)s'),
    'tut4': (skip(gen('emit_log_direct', arg='%(arg)s %(arg2)s'),
                  ['erlang', 'php']),
             skip(gen('receive_logs_direct', arg='%(arg)s'),
                  ['erlang', 'php']),
             '%(arg2)s'),
    'tut5': (skip(gen('emit_log_topic', arg='%(arg)s.foo %(arg2)s'),
                  ['erlang', 'php']),
             skip(gen('receive_logs_topic', arg='%(arg)s.*'),
                  ['erlang', 'php']),
             '%(arg2)s'),
    'tut6': (skip(gen('rpc_client', java='RPCClient', dotnet='RPCClient'),
                  ['erlang', 'php']),
             skip(gen('rpc_server', java='RPCServer', dotnet='RPCServer'),
                  ['erlang', 'php']),
             'fib[(]30[)]'),
    }

verbose = len(sys.argv) > 1
errors = 0

for test in sorted(tests.keys()):
    (send_progs, recv_progs, output_mask) = tests[test]
    for scwd, send_cmd in send_progs:
        for rcwd, recv_cmd in recv_progs:
            ctx = {
                'arg':  'rand_%s' % (random.randint(1,100),),
                'arg2': 'rand_%s' % (random.randint(1,100),),
                }
            rcmd = recv_cmd % ctx
            scmd = send_cmd % ctx
            mask = output_mask % ctx
            p = spawn(rcmd, verbose=verbose, cwd=rcwd)
            e = run(scmd, verbose=verbose, cwd=scwd)
            if wait(p, mask, verbose=verbose) and e == 0:
                print " [+] %s %-30s ok" % (test, scwd+'/'+rcwd)
            else:
                print " [!] %s %-30s FAILED %r %r (error=%r)" % \
                    (test, scwd+'/'+rcwd, rcmd, scmd, e)
                errors += 1

if errors:
    print " [!] %s tests failed" % (errors,)

sys.exit(errors)
