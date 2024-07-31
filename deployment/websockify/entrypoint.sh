#!/bin/bash
envsubst < /tokens.cfg.template > /tokens.cfg && python -m websockify --token-source=/tokens.cfg --token-plugin=TokenFile 6080
