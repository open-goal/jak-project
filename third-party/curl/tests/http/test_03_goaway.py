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
import logging
import time
from datetime import timedelta
from threading import Thread
import pytest

from testenv import Env, CurlClient, ExecResult


log = logging.getLogger(__name__)


class TestGoAway:

    @pytest.fixture(autouse=True, scope='class')
    def _class_scope(self, env, httpd, nghttpx):
        if env.have_h3():
            nghttpx.start_if_needed()
        httpd.clear_extra_configs()
        httpd.reload()

    # download files sequentially with delay, reload server for GOAWAY
    def test_03_01_h2_goaway(self, env: Env, httpd, nghttpx, repeat):
        proto = 'h2'
        count = 3
        self.r = None
        def long_run():
            curl = CurlClient(env=env)
            #  send 10 chunks of 1024 bytes in a response body with 100ms delay in between
            urln = f'https://{env.authority_for(env.domain1, proto)}' \
                   f'/curltest/tweak?id=[0-{count - 1}]'\
                   '&chunks=10&chunk_size=1024&chunk_delay=100ms'
            self.r = curl.http_download(urls=[urln], alpn_proto=proto)

        t = Thread(target=long_run)
        t.start()
        # each request will take a second, reload the server in the middle
        # of the first one.
        time.sleep(1.5)
        assert httpd.reload()
        t.join()
        r: ExecResult = self.r
        r.check_response(count=count, http_status=200)
        # reload will shut down the connection gracefully with GOAWAY
        # we expect to see a second connection opened afterwards
        assert r.total_connects == 2
        for idx, s in enumerate(r.stats):
            if s['num_connects'] > 0:
                log.debug(f'request {idx} connected')
        # this should take `count` seconds to retrieve
        assert r.duration >= timedelta(seconds=count)

    # download files sequentially with delay, reload server for GOAWAY
    @pytest.mark.skipif(condition=not Env.have_h3(), reason="h3 not supported")
    def test_03_02_h3_goaway(self, env: Env, httpd, nghttpx, repeat):
        proto = 'h3'
        if proto == 'h3' and env.curl_uses_lib('msh3'):
            pytest.skip("msh3 stalls here")
        if proto == 'h3' and env.curl_uses_lib('quiche'):
            pytest.skip("does not work in CI, but locally for some reason")
        count = 3
        self.r = None
        def long_run():
            curl = CurlClient(env=env)
            #  send 10 chunks of 1024 bytes in a response body with 100ms delay in between
            urln = f'https://{env.authority_for(env.domain1, proto)}' \
                   f'/curltest/tweak?id=[0-{count - 1}]'\
                   '&chunks=10&chunk_size=1024&chunk_delay=100ms'
            self.r = curl.http_download(urls=[urln], alpn_proto=proto)

        t = Thread(target=long_run)
        t.start()
        # each request will take a second, reload the server in the middle
        # of the first one.
        time.sleep(1.5)
        assert nghttpx.reload(timeout=timedelta(seconds=2))
        t.join()
        r: ExecResult = self.r
        # this should take `count` seconds to retrieve
        assert r.duration >= timedelta(seconds=count)
        r.check_response(count=count, http_status=200, connect_count=2)
        # reload will shut down the connection gracefully with GOAWAY
        # we expect to see a second connection opened afterwards
        for idx, s in enumerate(r.stats):
            if s['num_connects'] > 0:
                log.debug(f'request {idx} connected')

    # download files sequentially with delay, reload server for GOAWAY
    def test_03_03_h1_goaway(self, env: Env, httpd, nghttpx, repeat):
        proto = 'http/1.1'
        count = 3
        self.r = None
        def long_run():
            curl = CurlClient(env=env)
            #  send 10 chunks of 1024 bytes in a response body with 100ms delay in between
            urln = f'https://{env.authority_for(env.domain1, proto)}' \
                   f'/curltest/tweak?id=[0-{count - 1}]'\
                   '&chunks=10&chunk_size=1024&chunk_delay=100ms'
            self.r = curl.http_download(urls=[urln], alpn_proto=proto)

        t = Thread(target=long_run)
        t.start()
        # each request will take a second, reload the server in the middle
        # of the first one.
        time.sleep(1.5)
        assert httpd.reload()
        t.join()
        r: ExecResult = self.r
        r.check_response(count=count, http_status=200, connect_count=2)
        # reload will shut down the connection gracefully with GOAWAY
        # we expect to see a second connection opened afterwards
        for idx, s in enumerate(r.stats):
            if s['num_connects'] > 0:
                log.debug(f'request {idx} connected')
        # this should take `count` seconds to retrieve
        assert r.duration >= timedelta(seconds=count)
