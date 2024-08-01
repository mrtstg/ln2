#!/bin/bash
install -v -m 700 -o root -g root proxmox-fs-agent /usr/local/bin/
install -v -m 644 -o root -g root proxmox-fs-agent.service /etc/systemd/system/
systemctl daemon-reload
systemctl enable proxmox-fs-agent.service
