#!/usr/bin/env python
import time
import random
import re
import subprocess
import signal
import sys
import os

multiplier = float(os.environ.get('SLOWNESS', 1))

def run(cmd, **kwargs):
    p = subprocess.Popen(cmd.split(),
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE,
                         **kwargs)
    p.wait()
    out = p.stdout.read()
    err = p.stderr.read()

    time.sleep(0.2 * multiplier)
    return p.returncode, out + '\n' + err

def spawn(cmd, **kwargs):
    p = subprocess.Popen(cmd.split(),
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE,
                         **kwargs)
    time.sleep(0.5 * multiplier)
    return p

def wait(p, match):
    os.kill(p.pid, signal.SIGINT)
    p.wait()
    out = p.stdout.read()
    err = p.stderr.read()
    return bool(re.search(match, out)), out + '\n' + err



def gen(prog, arg="", **kwargs):
    Prog = ''.join([w.capitalize() for w in prog.split('_')])
    ctx = {
        'prog': prog,
        'Prog': Prog,
        'rubyver': os.environ.get('RUBYVER', '1.9'),
        'arg': arg,
        'java': kwargs.get('java', Prog),
        'dotnet': kwargs.get('dotnet', Prog),
        }
    return [
        ('python', './venv/bin/python %(prog)s.py %(arg)s' % ctx),
        ('perl', 'perl %(prog)s.pl %(arg)s' % ctx),
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
                  ['php']),
             skip(gen('receive_logs_direct', arg='%(arg)s'),
                  ['php']),
             '%(arg2)s'),
    'tut5': (skip(gen('emit_log_topic', arg='%(arg)s.foo %(arg2)s'),
                  ['php']),
             skip(gen('receive_logs_topic', arg='%(arg)s.*'),
                  ['php']),
             '%(arg2)s'),
    'tut6': (skip(gen('rpc_client', java='RPCClient', dotnet='RPCClient'),
                  ['erlang']),
             skip(gen('rpc_server', java='RPCServer', dotnet='RPCServer'),
                  ['erlang']),
             'fib[(]30[)]'),
    }

errors = 0

print " [.] Running tests with SLOWNESS=%r" % (multiplier,)
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
            p = spawn(rcmd, cwd=rcwd)
            serror, sout = run(scmd, cwd=scwd)
            matched, rout = wait(p, mask)
            if matched and serror == 0:
                print " [+] %s %-30s ok" % (test, scwd+'/'+rcwd)
            else:
                print " [!] %s %-30s FAILED" % (test, scwd+'/'+rcwd)
                print " [!] %r output (error=%s):\n%s\n" % (scmd, serror,
                                                            sout.strip())
                print " [!] %r output:\n%s\n" % (rcmd, rout.strip())
                errors += 1

if errors:
    print " [!] %s tests failed" % (errors,)

sys.exit(errors)
