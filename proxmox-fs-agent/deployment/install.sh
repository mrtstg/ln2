#!/bin/bash
mkdir -p /var/lib/proxmox-fs-agent && chown root:root /var/lib/proxmox-fs-agent && chmod 600 /var/lib/proxmox-fs-agent
[ -f "./crt.pem" ] && install -v -m 400 -o root -g root crt.pem /var/lib/proxmox-fs-agent/crt.pem
[ -f "./crt-key.pem"] && install -v -m 400 -o root -g root crt-key.pem /var/lib/proxmox-fs-agent/crt-key.pem
install -v -m 700 -o root -g root proxmox-fs-agent /usr/local/bin/
install -v -m 644 -o root -g root proxmox-fs-agent.service /etc/systemd/system/
systemctl daemon-reload
systemctl enable proxmox-fs-agent.service
