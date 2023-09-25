#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#***************************************************************************
#                                  _   _ ____  _
#  Project                     ___| | | |  _ \| |
#                             / __| | | | |_) | |
#                            | (__| |_| |  _ <| |___
#                             \___|\___/|_| \_\_____|
#
# Copyright (C) Daniel Stenberg, <daniel@haxx.se>, et al.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at https://curl.se/docs/copyright.html.
#
# You may opt to use, copy, modify, merge, publish, distribute and/or sell
# copies of the Software, and permit persons to whom the Software is
# furnished to do so, under the terms of the COPYING file.
#
# This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
# KIND, either express or implied.
#
# SPDX-License-Identifier: curl
#
###########################################################################
#
import difflib
import filecmp
import logging
import os
from datetime import timedelta
import pytest

from testenv import Env, CurlClient, LocalClient


log = logging.getLogger(__name__)


class TestDownload:

    @pytest.fixture(autouse=True, scope='class')
    def _class_scope(self, env, httpd, nghttpx):
        if env.have_h3():
            nghttpx.start_if_needed()
        httpd.clear_extra_configs()
        httpd.reload()

    @pytest.fixture(autouse=True, scope='class')
    def _class_scope(self, env, httpd):
        indir = httpd.docs_dir
        env.make_data_file(indir=indir, fname="data-10k", fsize=10*1024)
        env.make_data_file(indir=indir, fname="data-100k", fsize=100*1024)
        env.make_data_file(indir=indir, fname="data-1m", fsize=1024*1024)
        env.make_data_file(indir=indir, fname="data-10m", fsize=10*1024*1024)
        env.make_data_file(indir=indir, fname="data-50m", fsize=50*1024*1024)

    # download 1 file
    @pytest.mark.parametrize("proto", ['http/1.1', 'h2', 'h3'])
    def test_02_01_download_1(self, env: Env, httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        curl = CurlClient(env=env)
        url = f'https://{env.authority_for(env.domain1, proto)}/data.json'
        r = curl.http_download(urls=[url], alpn_proto=proto)
        r.check_response(http_status=200)

    # download 2 files
    @pytest.mark.parametrize("proto", ['http/1.1', 'h2', 'h3'])
    def test_02_02_download_2(self, env: Env, httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        curl = CurlClient(env=env)
        url = f'https://{env.authority_for(env.domain1, proto)}/data.json?[0-1]'
        r = curl.http_download(urls=[url], alpn_proto=proto)
        r.check_response(http_status=200, count=2)

    # download 100 files sequentially
    @pytest.mark.parametrize("proto", ['http/1.1', 'h2', 'h3'])
    def test_02_03_download_100_sequential(self, env: Env,
                                           httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        curl = CurlClient(env=env)
        urln = f'https://{env.authority_for(env.domain1, proto)}/data.json?[0-99]'
        r = curl.http_download(urls=[urln], alpn_proto=proto)
        r.check_response(http_status=200, count=100, connect_count=1)

    # download 100 files parallel
    @pytest.mark.parametrize("proto", ['h2', 'h3'])
    def test_02_04_download_100_parallel(self, env: Env,
                                         httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        max_parallel = 50
        curl = CurlClient(env=env)
        urln = f'https://{env.authority_for(env.domain1, proto)}/data.json?[0-99]'
        r = curl.http_download(urls=[urln], alpn_proto=proto, extra_args=[
            '--parallel', '--parallel-max', f'{max_parallel}'
        ])
        r.check_response(http_status=200, count=100)
        if proto == 'http/1.1':
            # http/1.1 parallel transfers will open multiple connections
            assert r.total_connects > 1, r.dump_logs()
        else:
            # http2 parallel transfers will use one connection (common limit is 100)
            assert r.total_connects == 1, r.dump_logs()

    # download 500 files sequential
    @pytest.mark.parametrize("proto", ['http/1.1', 'h2', 'h3'])
    def test_02_05_download_500_sequential(self, env: Env,
                                           httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        if proto == 'h3' and env.curl_uses_lib('msh3'):
            pytest.skip("msh3 shaky here")
        curl = CurlClient(env=env)
        urln = f'https://{env.authority_for(env.domain1, proto)}/data.json?[0-499]'
        r = curl.http_download(urls=[urln], alpn_proto=proto)
        r.check_response(http_status=200, count=500)
        if proto == 'http/1.1':
            # http/1.1 parallel transfers will open multiple connections
            assert r.total_connects > 1, r.dump_logs()
        else:
            # http2 parallel transfers will use one connection (common limit is 100)
            assert r.total_connects == 1, r.dump_logs()

    # download 500 files parallel
    @pytest.mark.parametrize("proto", ['h2', 'h3'])
    def test_02_06_download_500_parallel(self, env: Env,
                                         httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        count = 500
        max_parallel = 50
        curl = CurlClient(env=env)
        urln = f'https://{env.authority_for(env.domain1, proto)}/data.json?[000-{count-1}]'
        r = curl.http_download(urls=[urln], alpn_proto=proto, extra_args=[
            '--parallel', '--parallel-max', f'{max_parallel}'
        ])
        r.check_response(http_status=200, count=count, connect_count=1)

    # download files parallel, check connection reuse/multiplex
    @pytest.mark.parametrize("proto", ['h2', 'h3'])
    def test_02_07_download_reuse(self, env: Env,
                                  httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        count = 200
        curl = CurlClient(env=env)
        urln = f'https://{env.authority_for(env.domain1, proto)}/data.json?[0-{count-1}]'
        r = curl.http_download(urls=[urln], alpn_proto=proto,
                               with_stats=True, extra_args=[
            '--parallel', '--parallel-max', '200'
        ])
        r.check_response(http_status=200, count=count)
        # should have used at most 2 connections only (test servers allow 100 req/conn)
        # it may be just 1 on slow systems where request are answered faster than
        # curl can exhaust the capacity or if curl runs with address-sanitizer speed
        assert r.total_connects <= 2, "h2 should use fewer connections here"

    # download files parallel with http/1.1, check connection not reused
    @pytest.mark.parametrize("proto", ['http/1.1'])
    def test_02_07b_download_reuse(self, env: Env,
                                   httpd, nghttpx, repeat, proto):
        if env.curl_uses_lib('wolfssl'):
            pytest.skip("wolfssl session reuse borked")
        count = 6
        curl = CurlClient(env=env)
        urln = f'https://{env.authority_for(env.domain1, proto)}/data.json?[0-{count-1}]'
        r = curl.http_download(urls=[urln], alpn_proto=proto,
                               with_stats=True, extra_args=[
            '--parallel'
        ])
        r.check_response(count=count, http_status=200)
        # http/1.1 should have used count connections
        assert r.total_connects == count, "http/1.1 should use this many connections"

    @pytest.mark.parametrize("proto", ['http/1.1', 'h2', 'h3'])
    def test_02_08_1MB_serial(self, env: Env,
                              httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        count = 20
        urln = f'https://{env.authority_for(env.domain1, proto)}/data-1m?[0-{count-1}]'
        curl = CurlClient(env=env)
        r = curl.http_download(urls=[urln], alpn_proto=proto)
        r.check_response(count=count, http_status=200)

    @pytest.mark.parametrize("proto", ['h2', 'h3'])
    def test_02_09_1MB_parallel(self, env: Env,
                              httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        count = 20
        urln = f'https://{env.authority_for(env.domain1, proto)}/data-1m?[0-{count-1}]'
        curl = CurlClient(env=env)
        r = curl.http_download(urls=[urln], alpn_proto=proto, extra_args=[
            '--parallel'
        ])
        r.check_response(count=count, http_status=200)

    @pytest.mark.skipif(condition=Env().slow_network, reason="not suitable for slow network tests")
    @pytest.mark.skipif(condition=Env().ci_run, reason="not suitable for CI runs")
    @pytest.mark.parametrize("proto", ['http/1.1', 'h2', 'h3'])
    def test_02_10_10MB_serial(self, env: Env,
                              httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        count = 20
        urln = f'https://{env.authority_for(env.domain1, proto)}/data-10m?[0-{count-1}]'
        curl = CurlClient(env=env)
        r = curl.http_download(urls=[urln], alpn_proto=proto)
        r.check_response(count=count, http_status=200)

    @pytest.mark.skipif(condition=Env().slow_network, reason="not suitable for slow network tests")
    @pytest.mark.skipif(condition=Env().ci_run, reason="not suitable for CI runs")
    @pytest.mark.parametrize("proto", ['h2', 'h3'])
    def test_02_11_10MB_parallel(self, env: Env,
                              httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        if proto == 'h3' and env.curl_uses_lib('msh3'):
            pytest.skip("msh3 stalls here")
        count = 20
        urln = f'https://{env.authority_for(env.domain1, proto)}/data-10m?[0-{count-1}]'
        curl = CurlClient(env=env)
        r = curl.http_download(urls=[urln], alpn_proto=proto, extra_args=[
            '--parallel'
        ])
        r.check_response(count=count, http_status=200)

    @pytest.mark.parametrize("proto", ['h2', 'h3'])
    def test_02_12_head_serial_https(self, env: Env,
                                     httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        count = 100
        urln = f'https://{env.authority_for(env.domain1, proto)}/data-10m?[0-{count-1}]'
        curl = CurlClient(env=env)
        r = curl.http_download(urls=[urln], alpn_proto=proto, extra_args=[
            '--head'
        ])
        r.check_response(count=count, http_status=200)

    @pytest.mark.parametrize("proto", ['h2'])
    def test_02_13_head_serial_h2c(self, env: Env,
                                    httpd, nghttpx, repeat, proto):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        count = 100
        urln = f'http://{env.domain1}:{env.http_port}/data-10m?[0-{count-1}]'
        curl = CurlClient(env=env)
        r = curl.http_download(urls=[urln], alpn_proto=proto, extra_args=[
            '--head', '--http2-prior-knowledge', '--fail-early'
        ])
        r.check_response(count=count, http_status=200)

    @pytest.mark.skipif(condition=Env().slow_network, reason="not suitable for slow network tests")
    @pytest.mark.skipif(condition=Env().ci_run, reason="not suitable for CI runs")
    def test_02_20_h2_small_frames(self, env: Env, httpd, repeat):
        # Test case to reproduce content corruption as observed in
        # https://github.com/curl/curl/issues/10525
        # To reliably reproduce, we need an Apache httpd that supports
        # setting smaller frame sizes. This is not released yet, we
        # test if it works and back out if not.
        httpd.set_extra_config(env.domain1, lines=[
            f'H2MaxDataFrameLen 1024',
        ])
        assert httpd.stop()
        if not httpd.start():
            # no, not supported, bail out
            httpd.set_extra_config(env.domain1, lines=None)
            assert httpd.start()
            pytest.skip(f'H2MaxDataFrameLen not supported')
        # ok, make 100 downloads with 2 parallel running and they
        # are expected to stumble into the issue when using `lib/http2.c`
        # from curl 7.88.0
        count = 100
        urln = f'https://{env.authority_for(env.domain1, "h2")}/data-1m?[0-{count-1}]'
        curl = CurlClient(env=env)
        r = curl.http_download(urls=[urln], alpn_proto="h2", extra_args=[
            '--parallel', '--parallel-max', '2'
        ])
        r.check_response(count=count, http_status=200)
        srcfile = os.path.join(httpd.docs_dir, 'data-1m')
        self.check_downloads(curl, srcfile, count)
        # restore httpd defaults
        httpd.set_extra_config(env.domain1, lines=None)
        assert httpd.stop()
        assert httpd.start()

    # download via lib client, 1 at a time, pause/resume at different offsets
    @pytest.mark.parametrize("pause_offset", [0, 10*1024, 100*1023, 640000])
    def test_02_21_h2_lib_serial(self, env: Env, httpd, nghttpx, pause_offset, repeat):
        count = 10
        docname = 'data-10m'
        url = f'https://localhost:{env.https_port}/{docname}'
        client = LocalClient(name='h2-download', env=env)
        if not client.exists():
            pytest.skip(f'example client not built: {client.name}')
        r = client.run(args=[
             '-n', f'{count}', '-P', f'{pause_offset}', url
        ])
        r.check_exit_code(0)
        srcfile = os.path.join(httpd.docs_dir, docname)
        self.check_downloads(client, srcfile, count)

    # download via lib client, several at a time, pause/resume
    @pytest.mark.parametrize("pause_offset", [100*1023])
    def test_02_22_h2_lib_parallel_resume(self, env: Env, httpd, nghttpx, pause_offset, repeat):
        count = 10
        max_parallel = 5
        docname = 'data-10m'
        url = f'https://localhost:{env.https_port}/{docname}'
        client = LocalClient(name='h2-download', env=env)
        if not client.exists():
            pytest.skip(f'example client not built: {client.name}')
        r = client.run(args=[
            '-n', f'{count}', '-m', f'{max_parallel}',
            '-P', f'{pause_offset}', url
        ])
        r.check_exit_code(0)
        srcfile = os.path.join(httpd.docs_dir, docname)
        self.check_downloads(client, srcfile, count)

    # download, several at a time, pause and abort paused
    @pytest.mark.parametrize("pause_offset", [100*1023])
    def test_02_23_h2_lib_parallel_abort(self, env: Env, httpd, nghttpx, pause_offset, repeat):
        count = 200
        max_parallel = 100
        docname = 'data-10m'
        url = f'https://localhost:{env.https_port}/{docname}'
        client = LocalClient(name='h2-download', env=env)
        if not client.exists():
            pytest.skip(f'example client not built: {client.name}')
        r = client.run(args=[
            '-n', f'{count}', '-m', f'{max_parallel}', '-a',
            '-P', f'{pause_offset}', url
        ])
        r.check_exit_code(0)
        srcfile = os.path.join(httpd.docs_dir, docname)
        # downloads should be there, but not necessarily complete
        self.check_downloads(client, srcfile, count, complete=False)

    # speed limited download
    @pytest.mark.parametrize("proto", ['h2', 'h3'])
    def test_02_24_speed_limit(self, env: Env, httpd, nghttpx, proto, repeat):
        if proto == 'h3' and not env.have_h3():
            pytest.skip("h3 not supported")
        count = 1
        url = f'https://{env.authority_for(env.domain1, proto)}/data-1m'
        curl = CurlClient(env=env)
        r = curl.http_download(urls=[url], alpn_proto=proto, extra_args=[
            '--limit-rate', f'{196 * 1024}'
        ])
        r.check_response(count=count, http_status=200)
        assert r.duration > timedelta(seconds=4), \
            f'rate limited transfer should take more than 4s, not {r.duration}'

    # make extreme parallel h2 upgrades, check invalid conn reuse
    # before protocol switch has happened
    def test_02_25_h2_upgrade_x(self, env: Env, httpd, repeat):
        # not locally reproducible timeouts with certain SSL libs
        # Since this test is about connection reuse handling, we skip
        # it on these builds. Although we would certainly like to understand
        # why this happens.
        if env.curl_uses_lib('bearssl'):
            pytest.skip('CI workflows timeout on bearssl build')
        url = f'http://localhost:{env.http_port}/data-100k'
        client = LocalClient(name='h2-upgrade-extreme', env=env, timeout=15)
        if not client.exists():
            pytest.skip(f'example client not built: {client.name}')
        r = client.run(args=[url])
        assert r.exit_code == 0, f'{client.dump_logs()}'

    # Special client that tests TLS session reuse in parallel transfers
    def test_02_26_session_shared_reuse(self, env: Env, httpd, repeat):
        curl = CurlClient(env=env)
        url = f'https://{env.domain1}:{env.https_port}/data-100k'
        client = LocalClient(name='tls-session-reuse', env=env)
        if not client.exists():
            pytest.skip(f'example client not built: {client.name}')
        r = client.run(args=[url])
        r.check_exit_code(0)

    def check_downloads(self, client, srcfile: str, count: int,
                        complete: bool = True):
        for i in range(count):
            dfile = client.download_file(i)
            assert os.path.exists(dfile)
            if complete and not filecmp.cmp(srcfile, dfile, shallow=False):
                diff = "".join(difflib.unified_diff(a=open(srcfile).readlines(),
                                                    b=open(dfile).readlines(),
                                                    fromfile=srcfile,
                                                    tofile=dfile,
                                                    n=1))
                assert False, f'download {dfile} differs:\n{diff}'
