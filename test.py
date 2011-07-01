#!/usr/bin/env python
import time
import re
import subprocess
import signal
import sys

def run(cmd, verbose=False, **kwargs):
    if verbose:
        print " [s] Running %r" % (cmd,)
    p = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE, **kwargs)
    p.wait()

    if verbose:
        for line in p.stdout:
            line = p.stdout.readline().strip()
            if line:
                print ' [s]  %s' % (line,)
        print " [s] Done"
    time.sleep(0.1)


def spawn(cmd, verbose=False, **kwargs):
    if verbose:
        print " [r] Waiting for %r" % (cmd,)
    p = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE, **kwargs)
    time.sleep(0.4)
    return p

def wait(p, match, verbose=False):
    p.send_signal(signal.SIGINT)
    p.wait()
    r = False
    for line in p.stdout:
        if re.search(match, line):
            r = True
        if verbose:
            print " [r]  %s" % (line.strip(),)
    if verbose:
        print " [r] Done"
    return r



def gen(prog, arg="", **kwargs):
    Prog = ''.join([w.capitalize() for w in prog.split('_')])
    ctx = {
        'python': kwargs.get('python', prog),
        'erlang': kwargs.get('erlang', prog),
        'java': kwargs.get('java', Prog),
        'dotnet': kwargs.get('dotnet', Prog),
        'ruby': kwargs.get('ruby', prog),
        'php': kwargs.get('php', prog),
        'arg': arg,
        }
    return [
        ('python', './venv/bin/python %(python)s.py %(arg)s' % ctx),
        ('erlang', './%(python)s.erl %(arg)s' % ctx),
        ('java', 'java -cp .:commons-io-1.2.jar:commons-cli-1.1.jar:'
             'rabbitmq-client.jar %(java)s %(arg)s' % ctx),
        ('dotnet', 'env MONO_PATH=lib/bin mono %(dotnet)s.exe %(arg)s' % ctx),
        # ('ruby', 'env RUBYOPT=-rubygems GEM_HOME=gems/gems RUBYLIB=gems/lib '
        #      'ruby1.8 %(ruby)s.rb %(arg)s' % ctx),
        ('php', 'php %(php)s.php %(arg)s' % ctx),
        ]

tests = {
    'tut1': (gen('send'), gen('receive', java='Recv'), 'Hello World!'),
    'tut2': (gen('new_task', arg='xxxxxx'), gen('worker'), 'xxxxxx'),
    'tut3': (gen('emit_log', arg='123456'), gen('receive_logs'), '123456'),
    }


verbose = len(sys.argv) > 1

for test in sorted(tests.keys()):
    (send_progs, recv_progs, mask) = tests[test]
    for scwd, scmd in send_progs:
        for rcwd, rcmd in recv_progs:
            p = spawn(rcmd, verbose=verbose, cwd=rcwd)
            run(scmd, verbose=verbose, cwd=scwd)
            if wait(p, mask, verbose=verbose):
                print " [+] %s %-20s  ok" % (test, scwd+'/'+rcwd)
            else:
                print " [!] %s %-20s  FAILED %r %r" % (test, scwd+'/'+rcwd, rcmd, scmd)
